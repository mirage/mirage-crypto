type error = [
  | `Invalid_format
  | `Invalid_length
  | `Invalid_range
  | `Not_on_curve
  | `At_infinity
  | `Low_order
]

let error_to_string = function
  | `Invalid_format -> "invalid format"
  | `Not_on_curve -> "point is not on curve"
  | `At_infinity -> "point is at infinity"
  | `Invalid_length -> "invalid length"
  | `Invalid_range -> "invalid range"
  | `Low_order -> "low order"

let pp_error fmt e =
  Format.fprintf fmt "Cannot parse point: %s" (error_to_string e)

let rev_bytes buf =
  let len = Bytes.length buf in
  let res = Bytes.make len '\000' in
  for i = 0 to len - 1 do
    Bytes.set res (len - 1 - i) (Bytes.get buf i)
  done ; res

exception Message_too_long

let bit_at buf i =
  let byte_num = i / 8 in
  let bit_num = i mod 8 in
  let byte = Char.code (Bytes.get buf byte_num) in
  byte land (1 lsl bit_num) <> 0

module type Dh = sig
  type secret

  val secret_of_cs : ?compress:bool -> Cstruct.t ->
    (secret * Cstruct.t, error) result

  val gen_key : ?compress:bool -> ?g:Mirage_crypto_rng.g -> unit ->
    secret * Cstruct.t

  val key_exchange : secret -> Cstruct.t -> (Cstruct.t, error) result

  val secret_of_bytes : ?compress:bool -> bytes -> (secret * bytes, error) result
  val gen_bytes_key : ?compress:bool -> ?g:Mirage_crypto_rng.g -> unit -> secret * bytes
  val key_bytes_exchange : secret -> bytes -> (bytes, error) result
end

module type Dsa = sig
  type priv

  type pub

  val byte_length : int

  val priv_of_cstruct : Cstruct.t -> (priv, error) result

  val priv_to_cstruct : priv -> Cstruct.t

  val pub_of_cstruct : Cstruct.t -> (pub, error) result

  val pub_to_cstruct : ?compress:bool -> pub -> Cstruct.t

  val pub_of_priv : priv -> pub

  val generate : ?g:Mirage_crypto_rng.g -> unit -> priv * pub

  val sign : key:priv -> ?k:Cstruct.t -> Cstruct.t -> Cstruct.t * Cstruct.t

  val verify : key:pub -> Cstruct.t * Cstruct.t -> Cstruct.t -> bool

  module K_gen (H : Mirage_crypto.Hash.S) : sig

    val generate : key:priv -> Cstruct.t -> Cstruct.t

    val generate_bytes : key:priv -> bytes -> bytes
  end

  val priv_of_bytes : bytes -> (priv, error) result

  val priv_to_bytes : priv -> bytes

  val pub_of_bytes : bytes -> (pub, error) result

  val pub_to_bytes : ?compress:bool -> pub -> bytes

  val sign_bytes : key:priv -> ?k:bytes -> bytes -> bytes * bytes

  val verify_bytes : key:pub -> bytes * bytes -> bytes -> bool
end
module type Dh_dsa = sig
  module Dh : Dh
  module Dsa : Dsa
end

type field_element = bytes

module type Parameters = sig
  val a : field_element
  val b : field_element
  val g_x : field_element
  val g_y : field_element
  val p : field_element
  val n : field_element
  val pident: bytes
  val byte_length : int
  val fe_length : int
  val first_byte_bits : int option
end

type point = { f_x : field_element; f_y : field_element; f_z : field_element }

type scalar = Scalar of bytes

module type Foreign = sig
  val mul : field_element -> field_element -> field_element -> unit
  val sub : field_element -> field_element -> field_element -> unit
  val add : field_element -> field_element -> field_element -> unit
  val to_montgomery : field_element -> unit
  val from_bytes_buf : field_element -> bytes -> unit
  val set_one : field_element -> unit
  val nz : field_element -> bool
  val sqr : field_element -> field_element -> unit
  val from_montgomery : field_element -> unit
  val to_bytes_buf : bytes -> field_element -> unit
  val inv : field_element -> field_element -> unit
  val select_c : field_element -> bool -> field_element -> field_element -> unit

  val double_c : point -> point -> unit
  val add_c : point -> point -> point -> unit
end

module type Field_element = sig
  val create : unit -> field_element

  val copy : field_element -> field_element -> unit

  val one : unit -> field_element

  val to_bytes : bytes -> field_element -> unit

  val from_montgomery : field_element -> unit

  val add : field_element -> field_element -> field_element -> unit

  val sub : field_element -> field_element -> field_element -> unit

  val mul : field_element -> field_element -> field_element -> unit

  val nz : field_element -> bool

  val sqr : field_element -> field_element -> unit

  val inv : field_element -> field_element -> unit

  val from_be_bytes : bytes -> field_element

  val select : bool -> then_:field_element -> else_:field_element -> field_element
end

module Make_field_element (P : Parameters) (F : Foreign) : Field_element = struct
  include F

  let create () = Bytes.make P.fe_length '\000'

  let copy dst src = Bytes.blit src 0 dst 0 (Bytes.length src)

  let checked_buffer buf =
    assert (Bytes.length buf = P.byte_length);
    buf

  let from_bytes fe buf =
    F.from_bytes_buf fe (checked_buffer buf)

  let one () =
    let fe = create () in
    F.set_one fe;
    fe

  let to_bytes cs fe =
    F.to_bytes_buf (checked_buffer cs) fe

  let from_be_bytes buf =
    let buf_rev = rev_bytes buf in
    let fe = create () in
    from_bytes fe buf_rev;
    F.to_montgomery fe;
    fe

  let select bit ~then_ ~else_ =
    let out = create () in
    F.select_c out bit then_ else_;
    out
end

module type Point = sig
  val at_infinity : unit -> point

  val is_infinity : point -> bool

  val add : point -> point -> point

  val double : point -> point

  val of_bytes : bytes -> (point, error) result

  val to_bytes : compress:bool -> point -> bytes

  val to_affine_raw : point -> (field_element * field_element) option

  val x_of_finite_point : point -> bytes

  val params_g : point

  val select : bool -> then_:point -> else_:point -> point
end

module Make_point (P : Parameters) (F : Foreign) : Point = struct
  module Fe = Make_field_element(P)(F)

  let at_infinity () =
    let f_x = Fe.one () in
    let f_y = Fe.one () in
    let f_z = Fe.create () in
    { f_x; f_y; f_z }

  let is_infinity p = not (Fe.nz p.f_z)

  let is_solution_to_curve_equation =
    let a = Fe.from_be_bytes P.a in
    let b = Fe.from_be_bytes P.b in
    fun ~x ~y ->
      let x3 = Fe.create () in
      Fe.mul x3 x x;
      Fe.mul x3 x3 x;
      let ax = Fe.create () in
      Fe.mul ax a x;
      let y2 = Fe.create () in
      Fe.mul y2 y y;
      let sum = Fe.create () in
      Fe.add sum x3 ax;
      Fe.add sum sum b;
      Fe.sub sum sum y2;
      not (Fe.nz sum)

  let check_coordinate buf =
    (* ensure cs < p: *)
    match Eqaf.compare_be_with_len ~len:P.byte_length (Bytes.unsafe_to_string buf) (Bytes.unsafe_to_string P.p) >= 0 with
    | true -> None
    | exception Invalid_argument _ -> None
    | false -> Some (Fe.from_be_bytes buf)

  (** Convert cstruct coordinates to a finite point ensuring:
      - x < p
      - y < p
      - y^2 = ax^3 + ax + b
  *)
  let validate_finite_point ~x ~y =
    match (check_coordinate x, check_coordinate y) with
    | Some f_x, Some f_y ->
      if is_solution_to_curve_equation ~x:f_x ~y:f_y then
        let f_z = Fe.one () in
        Ok { f_x; f_y; f_z }
      else Error `Not_on_curve
    | _ -> Error `Invalid_range

  let to_affine_raw p =
    if is_infinity p then
      None
    else
      let z1 = Fe.create () in
      let z2 = Fe.create () in
      Fe.copy z1 p.f_z;
      Fe.from_montgomery z1;
      Fe.inv z2 z1;
      Fe.sqr z1 z2;
      Fe.from_montgomery z1;
      let x = Fe.create () in
      Fe.copy x p.f_x;
      Fe.mul x x z1;
      let y = Fe.create () in
      Fe.copy y p.f_y;
      Fe.mul z1 z1 z2;
      Fe.mul y y z1;
      Some (x, y)

  let to_affine p =
    match to_affine_raw p with
    | None -> None
    | Some (x, y) ->
      let out_x = Bytes.make P.byte_length '\000' in
      let out_y = Bytes.make P.byte_length '\000' in
      Fe.to_bytes out_x x;
      Fe.to_bytes out_y y;
      Some (out_x, out_y)

  let to_bytes ~compress p =
    let buf =
      match to_affine p with
      | None -> Bytes.make 1 '\000'
      | Some (x, y) ->
        let len_x = Bytes.length x and len_y = Bytes.length y in
        let res = Bytes.make (1 + len_x + len_y) '\000' in
        Bytes.set res 0 '\004' ;
        let rev_x = rev_bytes x and rev_y = rev_bytes y in
        Bytes.blit rev_x 0 res 1 len_x ;
        Bytes.blit rev_y 0 res (1 + len_x) len_y ;
        res
    in
    if compress then
      let out = Bytes.make (P.byte_length + 1) '\000' in
      let ident =
        2 + Char.code ((Bytes.get buf ((P.byte_length * 2) - 1))) land 1
      in
      Bytes.blit buf 1 out 1 P.byte_length;
      Bytes.set out 0 (Char.chr ident);
      out
    else
      buf

  let double p =
    let out = { f_x = Fe.create (); f_y = Fe.create (); f_z = Fe.create () } in
    F.double_c out p;
    out

  let add fe_p fe_q =
    let out = { f_x = Fe.create (); f_y = Fe.create (); f_z = Fe.create () } in
    F.add_c out fe_p fe_q;
    out

  let x_of_finite_point p =
    match to_affine p with None -> assert false | Some (x, _) -> rev_bytes x

  let params_g =
    match validate_finite_point ~x:P.g_x ~y:P.g_y with
    | Ok p -> p
    | Error _ -> assert false

  let select bit ~then_ ~else_ =
    {
      f_x = Fe.select bit ~then_:then_.f_x ~else_:else_.f_x;
      f_y = Fe.select bit ~then_:then_.f_y ~else_:else_.f_y;
      f_z = Fe.select bit ~then_:then_.f_z ~else_:else_.f_z;
    }

  let pow =
    let mult a b =
      let r = Fe.create () in
      Fe.mul r a b;
      r
    in
    let sqr x =
      let r = Fe.create () in
      Fe.sqr r x;
      r
    in
    fun x exp ->
    let r0 = ref (Fe.one ()) in
    let r1 =  ref x in
    for i = P.byte_length * 8 - 1 downto 0 do
      let bit = bit_at exp i in
      let multiplied = mult !r0 !r1 in
      let r0_sqr = sqr !r0 in
      let r1_sqr = sqr !r1 in
      r0 := Fe.select bit ~then_:multiplied ~else_:r0_sqr;
      r1 := Fe.select bit ~then_:r1_sqr ~else_:multiplied;
    done;
    !r0

  let decompress =
  (* When p = 4*k+3, as is the case of NIST-P256, there is an efficient square
     root algorithm to recover the y, as follows:

    Given the compact representation of Q as x,
     y2 = x^3 + a*x + b
     y' = y2^((p+1)/4)
     y = min(y',p-y')
     Q=(x,y) is the canonical representation of the point
  *)
    let pident = P.pident (* (Params.p + 1) / 4*) in
    let a = Fe.from_be_bytes P.a in
    let b = Fe.from_be_bytes P.b in
    let p = Fe.from_be_bytes P.p in
    fun pk ->
      let x = Fe.from_be_bytes (Bytes.sub pk 1 P.byte_length) in
      let x3 = Fe.create () in
      let ax = Fe.create () in
      let sum = Fe.create () in
      Fe.mul x3 x x;
      Fe.mul x3 x3 x; (* x3 *)
      Fe.mul ax a x;  (* ax *)
      Fe.add sum x3 ax;
      Fe.add sum sum b; (* y^2 *)
      let y = pow sum pident in (* https://tools.ietf.org/id/draft-jivsov-ecc-compact-00.xml#sqrt point 4.3*)
      let y' = Fe.create () in
      Fe.sub y' p y;
      let y_struct = Bytes.make (P.byte_length) '\000' in
      Fe.from_montgomery y;
      Fe.to_bytes y_struct y; (* number must not be in montgomery domain*)
      let y_struct = rev_bytes y_struct in
      let y_struct2 = Bytes.make (P.byte_length) '\000' in
      Fe.from_montgomery y';
      Fe.to_bytes y_struct2 y';(* number must not be in montgomery domain*)
      let y_struct2 = rev_bytes y_struct2 in
      let ident = Char.code (Bytes.get pk 0) in
      let signY =
        2 + (Char.code (Bytes.get y_struct (P.byte_length - 2))) land 1
      in
      let res = if Int.equal signY ident then y_struct else y_struct2 in
      let out = Bytes.make ((P.byte_length * 2) + 1) '\000' in
      Bytes.set out 0 '\004';
      Bytes.blit pk 1 out 1 P.byte_length;
      Bytes.blit res 0 out (P.byte_length + 1) P.byte_length;
      out

  let of_bytes buf =
    let len = P.byte_length in
    if Bytes.length buf = 0 then
      Error `Invalid_format
    else
      let of_bytes buf =
        let x = Bytes.sub buf 1 len in
        let y = Bytes.sub buf (1 + len) len in
        validate_finite_point ~x ~y
      in
      match Char.code (Bytes.get buf 0) with
      | 0x00 when Bytes.length buf = 1 -> Ok (at_infinity ())
      | 0x02 | 0x03 when Bytes.length P.pident > 0 ->
        let decompressed = decompress buf in
        of_bytes decompressed
      | 0x04 when Bytes.length buf = 1 + len + len ->
        of_bytes buf
      | 0x00 | 0x04 -> Error `Invalid_length
      | _ -> Error `Invalid_format
end

module type Scalar = sig
  val not_zero : bytes -> bool

  val is_in_range : bytes -> bool

  val of_bytes : bytes -> (scalar, error) result

  val to_bytes : scalar -> bytes

  val scalar_mult : scalar -> point -> point
end

module Make_scalar (Param : Parameters) (P : Point) : Scalar = struct
  let not_zero =
    let zero = String.make Param.byte_length '\000' in
    fun buf -> not (Eqaf.equal (Bytes.unsafe_to_string buf) zero)

  let is_in_range buf =
    not_zero buf
    && Eqaf.compare_be_with_len ~len:Param.byte_length (Bytes.unsafe_to_string Param.n) (Bytes.unsafe_to_string buf) > 0

  let of_bytes buf =
    match is_in_range buf with
    | exception Invalid_argument _ -> Error `Invalid_length
    | true -> Ok (Scalar (rev_bytes buf))
    | false -> Error `Invalid_range

  let to_bytes (Scalar buf) = rev_bytes buf

  let scalar_mult (Scalar s) p =
    let r0 = ref (P.at_infinity ()) in
    let r1 = ref p in
    for i = Param.byte_length * 8 - 1 downto 0 do
      let bit = bit_at s i in
      let sum = P.add !r0 !r1 in
      let r0_double = P.double !r0 in
      let r1_double = P.double !r1 in
      r0 := P.select bit ~then_:sum ~else_:r0_double;
      r1 := P.select bit ~then_:r1_double ~else_:sum
    done;
    !r0
end

module Make_dh (Param : Parameters) (P : Point) (S : Scalar) : Dh = struct
  let point_of_bytes c =
    match P.of_bytes c with
    | Ok p when not (P.is_infinity p) -> Ok p
    | Ok _ -> Error `At_infinity
    | Error _ as e -> e

  let point_to_bytes = P.to_bytes

  type secret = scalar

  let share ?(compress = false) private_key =
    let public_key = S.scalar_mult private_key P.params_g in
    point_to_bytes ~compress public_key

  let secret_of_bytes ?compress s =
    match S.of_bytes s with
    | Ok p -> Ok (p, share ?compress  p)
    | Error _ as e -> e

  let secret_of_cs ?compress s =
    match S.of_bytes (Cstruct.to_bytes s) with
    | Ok p -> Ok (p, Cstruct.of_bytes (share ?compress p))
    | Error _ as e -> e

  let rec generate_private_key ?g () =
    let candidate = Mirage_crypto_rng.generate ?g Param.byte_length in
    match S.of_bytes (Cstruct.to_bytes candidate) with
    | Ok secret -> secret
    | Error _ -> generate_private_key ?g ()

  let gen_bytes_key ?compress ?g () =
    let private_key = generate_private_key ?g () in
    (private_key, share ?compress private_key)

  let gen_key ?compress ?g () =
    let private_key, share = gen_bytes_key ?compress ?g () in
    private_key, Cstruct.of_bytes share

  let key_bytes_exchange secret received =
    match point_of_bytes received with
    | Error _ as err -> err
    | Ok shared -> Ok (P.x_of_finite_point (S.scalar_mult secret shared))

  let key_exchange secret received =
    match key_bytes_exchange secret (Cstruct.to_bytes received) with
    | Error _ as err -> err
    | Ok shared -> Ok (Cstruct.of_bytes shared)
end

module type Foreign_n = sig
  val mul : field_element -> field_element -> field_element -> unit
  val add : field_element -> field_element -> field_element -> unit
  val inv : field_element -> field_element -> unit
  val one : field_element -> unit
  val from_bytes : field_element -> bytes -> unit
  val to_bytes : bytes -> field_element -> unit
  val from_montgomery : field_element -> field_element -> unit
  val to_montgomery : field_element -> field_element -> unit
end

module Make_dsa (Param : Parameters) (F : Foreign_n) (P : Point) (S : Scalar) (H : Mirage_crypto.Hash.S) = struct
  let create () = Bytes.make Param.fe_length '\000'

  type priv = scalar

  let byte_length = Param.byte_length

  let priv_of_bytes= S.of_bytes

  let priv_to_bytes = S.to_bytes

  let priv_of_cstruct cs = priv_of_bytes (Cstruct.to_bytes cs)
  let priv_to_cstruct = fun p -> Cstruct.of_bytes (priv_to_bytes p)

  let padded msg =
    let l = Bytes.length msg in
    let bl = Param.byte_length in
    let first_byte_ok () =
      match Param.first_byte_bits with
      | None -> true
      | Some m -> (Char.code (Bytes.get msg 0)) land (0xFF land (lnot m)) = 0
    in
    if l > bl || (l = bl && not (first_byte_ok ())) then
      raise Message_too_long
    else if l = bl then
      msg
    else
      ( let res = Bytes.make ((bl - l) + (Bytes.length msg)) '\000' in
        Bytes.blit msg 0 res (bl - l) (Bytes.length msg) ;
        res )

  let from_be_bytes v =
    let v' = create () in
    F.from_bytes v' (rev_bytes v);
    v'

  let to_be_bytes v =
    let buf = Bytes.make Param.byte_length '\000' in
    F.to_bytes buf v;
    rev_bytes buf

  (* RFC 6979: compute a deterministic k *)
  module K_gen (H : Mirage_crypto.Hash.S) = struct

    let drbg : 'a Mirage_crypto_rng.generator =
      let module M = Mirage_crypto_rng.Hmac_drbg (H) in (module M)

    let g ~key cs =
      let g = Mirage_crypto_rng.create ~strict:true drbg in
      Mirage_crypto_rng.reseed ~g
        (Cstruct.append (Cstruct.of_bytes (S.to_bytes key)) cs);
      g

    (* take qbit length, and ensure it is suitable for ECDSA (> 0 & < n) *)
    let gen g =
      let rec go () =
        let r = Mirage_crypto_rng.generate ~g Param.byte_length in
        let r = Cstruct.to_bytes r in
        if S.is_in_range r then r else go ()
      in
      go ()

    let generate_bytes ~key buf = gen (g ~key (Cstruct.of_bytes (padded buf)))

    let generate ~key buf =
      let generated = gen (g ~key (Cstruct.of_bytes (padded (Cstruct.to_bytes buf)))) in
      Cstruct.of_bytes generated
  end

  module K_gen_default = K_gen(H)

  type pub = point

  let pub_of_bytes = P.of_bytes

  let pub_to_bytes ?(compress = false) pk = P.to_bytes ~compress pk

  let pub_of_cstruct cs = pub_of_bytes (Cstruct.to_bytes cs)
  let pub_to_cstruct = fun ?(compress = false) p -> Cstruct.of_bytes (pub_to_bytes ~compress p)

  let generate ?g () =
    (* FIPS 186-4 B 4.2 *)
    let d =
      let rec one () =
        match S.of_bytes (Cstruct.to_bytes (Mirage_crypto_rng.generate ?g Param.byte_length)) with
        | Ok x -> x
        | Error _ -> one ()
      in
      one ()
    in
    let q = S.scalar_mult d P.params_g in
    (d, q)

  let x_of_finite_point_mod_n p =
    match P.to_affine_raw p with
    | None -> None
    | Some (x, _) ->
      F.to_montgomery x x;
      let o = create () in
      F.one o;
      F.mul x x o;
      F.from_montgomery x x;
      Some (to_be_bytes x)

  let sign_bytes ~key ?k msg =
    let msg = padded msg in
    let e = from_be_bytes msg in
    let g = K_gen_default.g ~key (Cstruct.of_bytes msg) in
    let rec do_sign g =
      let again () =
        match k with
        | None -> do_sign g
        | Some _ -> invalid_arg "k not suitable"
      in
      let k' = match k with None -> K_gen_default.gen g | Some k -> k in
      let ksc = match S.of_bytes k' with
        | Ok ksc -> ksc
        | Error _ -> invalid_arg "k not in range" (* if no k is provided, this cannot happen since K_gen_*.gen already preserves the Scalar invariants *)
      in
      let point = S.scalar_mult ksc P.params_g in
      match x_of_finite_point_mod_n point with
      | None -> again ()
      | Some r ->
        let r_mon = from_be_bytes r in
        F.to_montgomery r_mon r_mon;
        let kinv = create () in
        let kmon = from_be_bytes k' in
        F.to_montgomery kmon kmon;
        F.inv kinv kmon;
        F.to_montgomery kmon kinv;
        let rd = create () in
        let dmon = from_be_bytes (S.to_bytes key) in
        F.to_montgomery dmon dmon;
        F.mul rd r_mon dmon;
        let cmon = create () in
        let zmon = create () in
        F.to_montgomery zmon e;
        F.add cmon zmon rd;
        let smon = create () in
        F.mul smon kmon cmon;
        let s = create () in
        F.from_montgomery s smon;
        let s = to_be_bytes s in
        if S.not_zero s && S.not_zero r then
          r, s
        else
          again ()
    in
    do_sign g

  let sign ~key ?k msg =
    let r, s = sign_bytes ~key ?k:(Option.map Cstruct.to_bytes k) (Cstruct.to_bytes msg) in
    Cstruct.of_bytes r, Cstruct.of_bytes s

  let pub_of_priv priv = S.scalar_mult priv P.params_g

  let verify_bytes ~key (r, s) msg =
    try
      let r = padded r and s = padded s in
      if not (S.is_in_range r && S.is_in_range s) then
        false
      else
        let msg = padded msg in
        let z = from_be_bytes msg in
        let s_inv = create () in
        let s_mon = from_be_bytes s in
        F.to_montgomery s_mon s_mon;
        F.inv s_inv s_mon;
        let u1 = create () in
        F.to_montgomery s_inv s_inv;
        F.to_montgomery z z;
        F.mul u1 z s_inv;
        let u2 = create () in
        let r_mon = from_be_bytes r in
        F.to_montgomery r_mon r_mon;
        F.mul u2 r_mon s_inv;
        F.from_montgomery u1 u1;
        F.from_montgomery u2 u2;
        match
          S.of_bytes (to_be_bytes u1),
          S.of_bytes (to_be_bytes u2)
        with
        | Ok u1, Ok u2 ->
          let point =
            P.add
              (S.scalar_mult u1 P.params_g)
              (S.scalar_mult u2 key)
          in
          begin match x_of_finite_point_mod_n point with
            | None -> false (* point is infinity *)
            | Some r' -> Bytes.equal r r'
          end
        | Error _, _ | _, Error _ -> false
    with
    | Message_too_long -> false

  let verify ~key (r, s) digest =
    verify_bytes ~key (Cstruct.to_bytes r, Cstruct.to_bytes s) (Cstruct.to_bytes digest)
end

module P224 : Dh_dsa = struct
  module Params = struct
    let a = Cstruct.to_bytes (Cstruct.of_hex "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE")
    let b = Cstruct.to_bytes (Cstruct.of_hex "B4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4")
    let g_x = Cstruct.to_bytes (Cstruct.of_hex "B70E0CBD6BB4BF7F321390B94A03C1D356C21122343280D6115C1D21")
    let g_y = Cstruct.to_bytes (Cstruct.of_hex "BD376388B5F723FB4C22DFE6CD4375A05A07476444D5819985007E34")
    let p = Cstruct.to_bytes (Cstruct.of_hex "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001")
    let n = Cstruct.to_bytes (Cstruct.of_hex "FFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D")
    let pident = Cstruct.to_bytes (Cstruct.empty)
    let byte_length = 28
    let fe_length = if Sys.word_size == 64 then 32 else 28 (* TODO: is this congruent with C code? *)
    let first_byte_bits = None
  end

  module Foreign = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_p224_mul" [@@noalloc]
    external sub : field_element -> field_element -> field_element -> unit = "mc_p224_sub" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_p224_add" [@@noalloc]
    external to_montgomery : field_element -> unit = "mc_p224_to_montgomery" [@@noalloc]
    external from_bytes_buf : field_element -> bytes -> unit = "mc_p224_from_bytes" [@@noalloc]
    external set_one : field_element -> unit = "mc_p224_set_one" [@@noalloc]
    external nz : field_element -> bool = "mc_p224_nz" [@@noalloc]
    external sqr : field_element -> field_element -> unit = "mc_p224_sqr" [@@noalloc]
    external from_montgomery : field_element -> unit = "mc_p224_from_montgomery" [@@noalloc]
    external to_bytes_buf : bytes -> field_element -> unit = "mc_p224_to_bytes" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_p224_inv" [@@noalloc]
    external select_c : field_element -> bool -> field_element -> field_element -> unit = "mc_p224_select" [@@noalloc]

    external double_c : point -> point -> unit = "mc_p224_point_double" [@@noalloc]
    external add_c : point -> point -> point -> unit = "mc_p224_point_add" [@@noalloc]
  end

  module Foreign_n = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_np224_mul" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_np224_add" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_np224_inv" [@@noalloc]
    external one : field_element -> unit = "mc_np224_one" [@@noalloc]
    external from_bytes : field_element -> bytes -> unit = "mc_np224_from_bytes" [@@noalloc]
    external to_bytes : bytes -> field_element -> unit = "mc_np224_to_bytes" [@@noalloc]
    external from_montgomery : field_element -> field_element -> unit = "mc_np224_from_montgomery" [@@noalloc]
    external to_montgomery : field_element -> field_element -> unit = "mc_np224_to_montgomery" [@@noalloc]
  end

  module P = Make_point(Params)(Foreign)
  module S = Make_scalar(Params)(P)
  module Dh = Make_dh(Params)(P)(S)
  module Dsa = Make_dsa(Params)(Foreign_n)(P)(S)(Mirage_crypto.Hash.SHA256)
end

module P256 : Dh_dsa  = struct
  module Params = struct
    let a = Cstruct.to_bytes (Cstruct.of_hex "FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC")
    let b = Cstruct.to_bytes (Cstruct.of_hex "5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B")
    let g_x =
      Cstruct.to_bytes (Cstruct.of_hex "6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296")
    let g_y =
      Cstruct.to_bytes (Cstruct.of_hex "4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5")
    let p = Cstruct.to_bytes (Cstruct.of_hex "FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF")
    let n = Cstruct.to_bytes (Cstruct.of_hex "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551")
    let pident = Cstruct.to_bytes (Cstruct.of_hex "3FFFFFFFC0000000400000000000000000000000400000000000000000000000") |> rev_bytes (* (Params.p + 1) / 4*)
    let byte_length = 32
    let fe_length = 32
    let first_byte_bits = None
  end

  module Foreign = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_p256_mul" [@@noalloc]
    external sub : field_element -> field_element -> field_element -> unit = "mc_p256_sub" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_p256_add" [@@noalloc]
    external to_montgomery : field_element -> unit = "mc_p256_to_montgomery" [@@noalloc]
    external from_bytes_buf : field_element -> bytes -> unit = "mc_p256_from_bytes" [@@noalloc]
    external set_one : field_element -> unit = "mc_p256_set_one" [@@noalloc]
    external nz : field_element -> bool = "mc_p256_nz" [@@noalloc]
    external sqr : field_element -> field_element -> unit = "mc_p256_sqr" [@@noalloc]
    external from_montgomery : field_element -> unit = "mc_p256_from_montgomery" [@@noalloc]
    external to_bytes_buf : bytes -> field_element -> unit = "mc_p256_to_bytes" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_p256_inv" [@@noalloc]
    external select_c : field_element -> bool -> field_element -> field_element -> unit = "mc_p256_select" [@@noalloc]

    external double_c : point -> point -> unit = "mc_p256_point_double" [@@noalloc]
    external add_c : point -> point -> point -> unit = "mc_p256_point_add" [@@noalloc]
  end

  module Foreign_n = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_np256_mul" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_np256_add" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_np256_inv" [@@noalloc]
    external one : field_element -> unit = "mc_np256_one" [@@noalloc]
    external from_bytes : field_element -> bytes -> unit = "mc_np256_from_bytes" [@@noalloc]
    external to_bytes : bytes -> field_element -> unit = "mc_np256_to_bytes" [@@noalloc]
    external from_montgomery : field_element -> field_element -> unit = "mc_np256_from_montgomery" [@@noalloc]
    external to_montgomery : field_element -> field_element -> unit = "mc_np256_to_montgomery" [@@noalloc]
  end

  module P = Make_point(Params)(Foreign)
  module S = Make_scalar(Params)(P)
  module Dh = Make_dh(Params)(P)(S)
  module Dsa = Make_dsa(Params)(Foreign_n)(P)(S)(Mirage_crypto.Hash.SHA256)
end

module P384 : Dh_dsa = struct
  module Params = struct
    let a = Cstruct.to_bytes (Cstruct.of_hex "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFC")
    let b = Cstruct.to_bytes (Cstruct.of_hex "B3312FA7E23EE7E4988E056BE3F82D19181D9C6EFE8141120314088F5013875AC656398D8A2ED19D2A85C8EDD3EC2AEF")
    let g_x =
      Cstruct.to_bytes (Cstruct.of_hex "AA87CA22BE8B05378EB1C71EF320AD746E1D3B628BA79B9859F741E082542A385502F25DBF55296C3A545E3872760AB7")
    let g_y =
      Cstruct.to_bytes (Cstruct.of_hex "3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f")
    let p = Cstruct.to_bytes (Cstruct.of_hex "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFF")
    let n = Cstruct.to_bytes (Cstruct.of_hex "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7634D81F4372DDF581A0DB248B0A77AECEC196ACCC52973")
    let pident = Cstruct.to_bytes (Cstruct.of_hex "3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFFFFFFFC00000000000000040000000") |> rev_bytes (* (Params.p + 1) / 4*)
    let byte_length = 48
    let fe_length = 48
    let first_byte_bits = None
  end

  module Foreign = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_p384_mul" [@@noalloc]
    external sub : field_element -> field_element -> field_element -> unit = "mc_p384_sub" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_p384_add" [@@noalloc]
    external to_montgomery : field_element -> unit = "mc_p384_to_montgomery" [@@noalloc]
    external from_bytes_buf : field_element -> bytes -> unit = "mc_p384_from_bytes" [@@noalloc]
    external set_one : field_element -> unit = "mc_p384_set_one" [@@noalloc]
    external nz : field_element -> bool = "mc_p384_nz" [@@noalloc]
    external sqr : field_element -> field_element -> unit = "mc_p384_sqr" [@@noalloc]
    external from_montgomery : field_element -> unit = "mc_p384_from_montgomery" [@@noalloc]
    external to_bytes_buf : bytes -> field_element -> unit = "mc_p384_to_bytes" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_p384_inv" [@@noalloc]
    external select_c : field_element -> bool -> field_element -> field_element -> unit = "mc_p384_select" [@@noalloc]

    external double_c : point -> point -> unit = "mc_p384_point_double" [@@noalloc]
    external add_c : point -> point -> point -> unit = "mc_p384_point_add" [@@noalloc]
  end

  module Foreign_n = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_np384_mul" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_np384_add" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_np384_inv" [@@noalloc]
    external one : field_element -> unit = "mc_np384_one" [@@noalloc]
    external from_bytes : field_element -> bytes -> unit = "mc_np384_from_bytes" [@@noalloc]
    external to_bytes : bytes -> field_element -> unit = "mc_np384_to_bytes" [@@noalloc]
    external from_montgomery : field_element -> field_element -> unit = "mc_np384_from_montgomery" [@@noalloc]
    external to_montgomery : field_element -> field_element -> unit = "mc_np384_to_montgomery" [@@noalloc]
  end

  module P = Make_point(Params)(Foreign)
  module S = Make_scalar(Params)(P)
  module Dh = Make_dh(Params)(P)(S)
  module Dsa = Make_dsa(Params)(Foreign_n)(P)(S)(Mirage_crypto.Hash.SHA384)
end

module P521 : Dh_dsa = struct
  module Params = struct
    let a = Cstruct.to_bytes (Cstruct.of_hex "01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC")
    let b = Cstruct.to_bytes (Cstruct.of_hex "0051953EB9618E1C9A1F929A21A0B68540EEA2DA725B99B315F3B8B489918EF109E156193951EC7E937B1652C0BD3BB1BF073573DF883D2C34F1EF451FD46B503F00")
    let g_x =
      Cstruct.to_bytes (Cstruct.of_hex "00C6858E06B70404E9CD9E3ECB662395B4429C648139053FB521F828AF606B4D3DBAA14B5E77EFE75928FE1DC127A2FFA8DE3348B3C1856A429BF97E7E31C2E5BD66")
    let g_y =
      Cstruct.to_bytes (Cstruct.of_hex "011839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650")
    let p = Cstruct.to_bytes (Cstruct.of_hex "01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
    let n = Cstruct.to_bytes (Cstruct.of_hex "01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA51868783BF2F966B7FCC0148F709A5D03BB5C9B8899C47AEBB6FB71E91386409")
    let pident = Cstruct.to_bytes (Cstruct.of_hex "017fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff") |> rev_bytes
    let byte_length = 66
    let fe_length = if Sys.word_size == 64 then 72 else 68  (* TODO: is this congruent with C code? *)
    let first_byte_bits = Some 0x01
  end

  module Foreign = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_p521_mul" [@@noalloc]
    external sub : field_element -> field_element -> field_element -> unit = "mc_p521_sub" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_p521_add" [@@noalloc]
    external to_montgomery : field_element -> unit = "mc_p521_to_montgomery" [@@noalloc]
    external from_bytes_buf : field_element -> bytes -> unit = "mc_p521_from_bytes" [@@noalloc]
    external set_one : field_element -> unit = "mc_p521_set_one" [@@noalloc]
    external nz : field_element -> bool = "mc_p521_nz" [@@noalloc]
    external sqr : field_element -> field_element -> unit = "mc_p521_sqr" [@@noalloc]
    external from_montgomery : field_element -> unit = "mc_p521_from_montgomery" [@@noalloc]
    external to_bytes_buf : bytes -> field_element -> unit = "mc_p521_to_bytes" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_p521_inv" [@@noalloc]
    external select_c : field_element -> bool -> field_element -> field_element -> unit = "mc_p521_select" [@@noalloc]

    external double_c : point -> point -> unit = "mc_p521_point_double" [@@noalloc]
    external add_c : point -> point -> point -> unit = "mc_p521_point_add" [@@noalloc]
  end

  module Foreign_n = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_np521_mul" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_np521_add" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_np521_inv" [@@noalloc]
    external one : field_element -> unit = "mc_np521_one" [@@noalloc]
    external from_bytes : field_element -> bytes -> unit = "mc_np521_from_bytes" [@@noalloc]
    external to_bytes : bytes -> field_element -> unit = "mc_np521_to_bytes" [@@noalloc]
    external from_montgomery : field_element -> field_element -> unit = "mc_np521_from_montgomery" [@@noalloc]
    external to_montgomery : field_element -> field_element -> unit = "mc_np521_to_montgomery" [@@noalloc]
  end

  module P = Make_point(Params)(Foreign)
  module S = Make_scalar(Params)(P)
  module Dh = Make_dh(Params)(P)(S)
  module Dsa = Make_dsa(Params)(Foreign_n)(P)(S)(Mirage_crypto.Hash.SHA512)
end

module X25519 = struct
  (* RFC 7748 *)
  external x25519_scalar_mult_generic : bytes -> bytes -> int -> bytes -> int -> unit = "mc_x25519_scalar_mult_generic" [@@noalloc]

  let key_len = 32

  let scalar_mult in_ base =
    let out = Bytes.make key_len '\000' in
    x25519_scalar_mult_generic out
      in_ 0 base 0;
    out

  type secret = bytes

  let basepoint =
    let data = Bytes.make key_len '\000' in
    Bytes.set data 0 (Char.chr 9);
    data

  let public priv = scalar_mult priv basepoint

  let gen_bytes_key ?compress:_ ?g () =
    let secret = Mirage_crypto_rng.generate ?g key_len in
    Cstruct.to_bytes secret, public (Cstruct.to_bytes secret)

  let gen_key ?compress ?g () =
    let secret, public = gen_bytes_key ~compress ?g () in
    secret, Cstruct.of_bytes public

  let secret_of_bytes ?compress:_ s =
    if Bytes.length s = key_len then
      Ok (s, public s)
    else
      Error `Invalid_length

  let secret_of_cs ?compress cs = match secret_of_bytes ~compress (Cstruct.to_bytes cs) with
    | Ok (secret, public) -> Ok (secret, Cstruct.of_bytes public)
    | Error _ as e -> e

  let is_zero =
    let zero = Bytes.make key_len '\000' in
    fun cs -> Bytes.equal zero cs

  let key_bytes_exchange secret public =
    if Bytes.length public = key_len then
      let res = scalar_mult secret public in
      if is_zero res then Error `Low_order else Ok res
    else
      Error `Invalid_length

  let key_exchange secret public =
    match key_bytes_exchange secret (Cstruct.to_bytes public) with
    | Ok shared -> Ok (Cstruct.of_bytes shared)
    | Error _ as e -> e
end

module Ed25519 = struct

  external scalar_mult_base_to_bytes : bytes -> bytes -> unit = "mc_25519_scalar_mult_base" [@@noalloc]
  external reduce_l : bytes -> unit = "mc_25519_reduce_l" [@@noalloc]
  external muladd : bytes -> bytes -> bytes -> bytes -> unit = "mc_25519_muladd" [@@noalloc]
  external double_scalar_mult : bytes -> bytes -> bytes -> bytes -> int -> bool = "mc_25519_double_scalar_mult" [@@noalloc]
  external pub_ok : bytes -> bool = "mc_25519_pub_ok" [@@noalloc]

  type pub = bytes

  type priv = bytes

  (* RFC 8032 *)
  let key_len = 32

  let public secret =
    (* section 5.1.5 *)
    (* step 1 *)
    let h = Mirage_crypto.Hash.SHA512.digest (Cstruct.of_bytes secret) in
    (* step 2 *)
    let s, rest = Cstruct.split h key_len in
    let s, rest = Cstruct.to_bytes s, Cstruct.to_bytes rest in
    Bytes.set s 0 (Char.unsafe_chr (Char.code (Bytes.get s 0) land 248));
    Bytes.set s 31 (Char.unsafe_chr ((Char.code (Bytes.get s 31) land 127) lor 64));
    (* step 3 and 4 *)
    let public = Bytes.make key_len '\000' in
    scalar_mult_base_to_bytes public s;
    public, (s, rest)

  let pub_of_priv secret = fst (public secret)

  let priv_of_bytes buf =
    if Bytes.length buf = key_len then Ok buf else Error `Invalid_length

  let priv_of_cstruct = fun p -> priv_of_bytes (Cstruct.to_bytes p)

  let priv_to_bytes priv = priv

  let priv_to_cstruct = fun p -> Cstruct.of_bytes (priv_to_bytes p)

  let pub_of_bytes buf =
    if Bytes.length buf = key_len then
      let buf_copy = Bytes.copy buf in
      if pub_ok buf_copy then
        Ok buf_copy
      else
        Error `Not_on_curve
    else
      Error `Invalid_length

  let pub_of_cstruct = fun p -> pub_of_bytes (Cstruct.to_bytes p)

  let pub_to_bytes pub = pub

  let pub_to_cstruct = fun p -> Cstruct.of_bytes (pub_to_bytes p)

  let generate ?g () =
    let secret = Mirage_crypto_rng.generate ?g key_len in
    let secret = Cstruct.to_bytes secret in
    secret, pub_of_priv secret

  let sign_bytes ~key msg =
    (* section 5.1.6 *)
    let pub, (s, prefix) = public key in
    let r = Mirage_crypto.Hash.SHA512.digest (Cstruct.of_bytes (Bytes.concat Bytes.empty [ prefix; msg ])) in
    let r = Cstruct.to_bytes r in
    reduce_l r;
    let r_big = Bytes.make key_len '\000' in
    scalar_mult_base_to_bytes r_big r;
    let k = Mirage_crypto.Hash.SHA512.digest (Cstruct.of_bytes (Bytes.concat Bytes.empty [ r_big; pub; msg])) in
    let k = Cstruct.to_bytes k in
    reduce_l k;
    let s_out = Bytes.make key_len '\000' in
    muladd s_out k s r;
    let res = Bytes.make (key_len + key_len) '\000' in
    Bytes.blit r_big 0 res 0 key_len ;
    Bytes.blit s_out 0 res key_len key_len ;
    res

  let sign ~key msg = Cstruct.of_bytes (sign_bytes ~key (Cstruct.to_bytes msg))

  let verify_bytes ~key signature ~msg =
    (* section 5.1.7 *)
    if Bytes.length signature = 2 * key_len then
      let r, s = Cstruct.split (Cstruct.of_bytes signature) key_len in
      let r, s = Cstruct.to_bytes r, Cstruct.to_bytes s in
      let s_smaller_l =
        (* check s within 0 <= s < L *)
        let s' = Bytes.make (key_len * 2) '\000' in
        Bytes.blit s 0 s' 0 key_len;
        reduce_l s';
        let s'' = Bytes.concat Bytes.empty [ s; Bytes.make key_len '\000' ] in
        Bytes.equal s'' s'
      in
      if s_smaller_l then begin
        let k =
          Mirage_crypto.Hash.SHA512.digest (Cstruct.of_bytes (Bytes.concat Bytes.empty [ r ; key ; msg ]))
        in
        let k = Cstruct.to_bytes k in
        reduce_l k;
        let r' = Bytes.make key_len '\000' in
        let success = double_scalar_mult r' k key s 0 in
        success && Bytes.equal r r'
      end else
        false
    else
      false

  let verify ~key signature ~msg =
    verify_bytes ~key (Cstruct.to_bytes signature) ~msg:(Cstruct.to_bytes msg)
end
