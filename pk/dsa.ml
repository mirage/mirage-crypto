open Mirage_crypto.Uncommon

open Common

type pub = { p : Z.t ; q : Z.t ; gg : Z.t ; y : Z.t }

let pub ?(fips = false) ~p ~q ~gg ~y () =
  let* () = guard Z.(one < gg && gg < p) (`Msg "bad generator") in
  let* () = guard (Z_extra.pseudoprime q) (`Msg "q is not prime") in
  let* () = guard (Z.is_odd p && Z_extra.pseudoprime p) (`Msg "p is not prime") in
  let* () = guard Z.(zero < y && y < p) (`Msg "y not in 0..p-1") in
  let* () = guard (q < p) (`Msg "q is not smaller than p") in
  let* () = guard Z.(zero = (pred p) mod q) (`Msg "p - 1 mod q <> 0") in
  let* () =
    if fips then
      match Z.numbits p, Z.numbits q with
      | 1024, 160 | 2048, 224 | 2048, 256 | 3072, 256 -> Ok ()
      | _ -> Error (`Msg "bit length of p or q not FIPS specified")
    else
      Ok ()
  in
  Ok { p ; q ; gg ; y }

type priv =
  { p : Z.t ; q : Z.t ; gg : Z.t ; x : Z.t ; y : Z.t }

let priv ?fips ~p ~q ~gg ~x ~y () =
  let* _ = pub ?fips ~p ~q ~gg ~y () in
  let* () = guard Z.(zero < x && x < q) (`Msg "x not in 1..q-1") in
  let* () = guard Z.(y = powm gg x p) (`Msg "y <> g ^ x mod p") in
  Ok { p ; q ; gg ; x ; y }

let pub_of_priv { p; q; gg; y; _ } = { p; q; gg; y }

type keysize = [ `Fips1024 | `Fips2048 | `Fips3072 | `Exactly of int * int ]

let expand_size = function
  | `Fips1024       -> (1024, 160)
  | `Fips2048       -> (2048, 256)
  | `Fips3072       -> (3072, 256)
  | `Exactly (l, n) ->
      if 3 <= l && 2 <= n then (l, n) else
        invalid_arg "Dsa.generate: bits: `Exactly (%d, %d)" l n

type mask = [ `No | `Yes | `Yes_with of Mirage_crypto_rng.g ]

let expand_mask = function
  | `No         -> `No
  | `Yes        -> `Yes None
  | `Yes_with g -> `Yes (Some g)

(*
 * FIPS.186-4-style derivation:
 * - p and q are derived using a method numerically like the one described in
 *   A.1.1.2, adapted to use the native rng.
 * - g is derived as per A.2.1.
 *)
let params ?g size =
  let two = Z.(~$2) in
  let (l, n) = expand_size size in
  let q = Z_extra.prime ?g ~msb:1 n in
  let p =
    let q_q  = Z.(q * two) in
    until Z_extra.pseudoprime @@ fun () ->
      let x = Z_extra.gen_bits ?g ~msb:1 l in
      Z.(x - (x mod q_q) + one)
  in
  let gg =
    let e = Z.(pred p / q) in
    until ((<>) Z.one) @@ fun () ->
      let h = Z_extra.gen_r ?g two Z.(pred p) in
      Z.(powm h e p)
  in
  (* all checks above are already satisfied *)
  (p, q, gg)

let generate ?g size =
  let (p, q, gg) = params ?g size in
  let x = Z_extra.gen_r ?g Z.one q in
  let y = Z.(powm gg x p) in
  (* checks are satisfied due to construction *)
  { p; q; gg; x; y }


module K_gen (H : Digestif.S) = struct

  let drbg : 'a Mirage_crypto_rng.generator =
    let module M = Mirage_crypto_rng.Hmac_drbg (H) in (module M)

  let z_gen ~key:{ q; x; _ } z =
    let repr = Z_extra.to_octets_be ~size:(Z.numbits q // 8) in
    let g    = Mirage_crypto_rng.create ~strict:true drbg in
    Mirage_crypto_rng.reseed ~g (repr x ^ repr Z.(z mod q));
    Z_extra.gen_r ~g Z.one q

  let generate ~key buf =
    z_gen ~key (Z_extra.of_octets_be ~bits:(Z.numbits key.q) buf)
end

module K_gen_sha256 = K_gen (Digestif.SHA256)

let sign_z ?(mask = `Yes) ?k:k0 ~key:({ p; q; gg; x; _ } as key) z =
  let k = match k0 with Some k -> k | None -> K_gen_sha256.z_gen ~key z in
  let k' = Z.invert k q
  and b, b' = match expand_mask mask with
    | `No -> Z.one, Z.one
    | `Yes g ->
      let m  = Z_extra.gen_r ?g Z.one q in
      m, Z.invert m q
  in
  let r = Z.(powm_sec gg k p mod q) in
  (* normal DSA sign is: s = k^-1 * (z + r * x) mod q *)
  (* we apply blinding where possible and compute:
     s = k^-1 * b^-1 * (b * z + b * r * x) mod q
     see https://github.com/openssl/openssl/pull/6524 for further details *)
  let s =
    let t1 =
      let t11 = Z.(b * x mod q) in
      Z.(t11 * r mod q)
    in
    let t2 = Z.(b * z mod q) in
    let t3 = Z.((t1 + t2) mod q) in
    let t4 = Z.(k' * t3 mod q) in
    Z.(b' * t4 mod q)
  in
  if r = Z.zero || s = Z.zero then invalid_arg "k unsuitable" else (r, s)

let verify_z ~key:({ p; q; gg; y }: pub ) (r, s) z =
  let v () =
    let w  = Z.invert s q in
    let u1 = Z.(z * w mod q)
    and u2 = Z.(r * w mod q) in
    Z.((powm gg u1 p * powm y u2 p) mod p mod q) in
  Z.zero < r && r < q && Z.zero < s && s < q && v () = r

let sign ?mask ?k ~(key : priv) digest =
  let bits   = Z.numbits key.q in
  let size   = bits // 8 in
  let (r, s) = sign_z ?mask ?k ~key (Z_extra.of_octets_be ~bits digest) in
  Z_extra.(to_octets_be ~size r, to_octets_be ~size s)

let verify ~(key : pub) (r, s) digest =
  let z      = Z_extra.of_octets_be ~bits:(Z.numbits key.q) digest
  and (r, s) = Z_extra.(of_octets_be r, of_octets_be s) in
  verify_z ~key (r, s) z

let rec shift_left_inplace buf = function
  | 0 -> ()
  | bits when bits mod 8 = 0 ->
    let off = bits / 8 in
    let to_blit = Bytes.length buf - off in
    Bytes.blit buf off buf 0 to_blit ;
    Bytes.unsafe_fill buf to_blit (Bytes.length buf - to_blit) '\x00'
  | bits when bits < 8 ->
    let foo = 8 - bits in
    for i = 0 to Bytes.length buf - 2 do
      let b1 = Bytes.get_uint8 buf i
      and b2 = Bytes.get_uint8 buf (i + 1) in
      Bytes.set_uint8 buf i ((b1 lsl bits) lor (b2 lsr foo))
    done ;
    Bytes.set_uint8 buf (Bytes.length buf - 1)
      (Bytes.get_uint8 buf (Bytes.length buf - 1) lsl bits)
  | bits ->
    shift_left_inplace buf (8 * (bits / 8)) ;
    shift_left_inplace buf (bits mod 8)

let (lsl) buf bits =
  let buf' = Bytes.of_string buf in
  shift_left_inplace buf' bits;
  Bytes.unsafe_to_string buf'

let massage ~key:({ q; _ }: pub) digest =
  let bits = Z.numbits q in
  if bits >= String.length digest * 8 then
    digest
  else
    let buf = Z_extra.(to_octets_be Z.(of_octets_be digest mod q)) in
    buf lsl ((8 - bits mod 8) mod 8)
