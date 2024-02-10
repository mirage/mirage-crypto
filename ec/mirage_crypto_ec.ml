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

let rev_string buf =
  let len = String.length buf in
  let res = Bytes.make len '\000' in
  for i = 0 to len - 1 do
    Bytes.set res (len - 1 - i) (String.get buf i)
  done ;
  Bytes.unsafe_to_string res

exception Message_too_long

let bit_at buf i =
  let byte_num = i / 8 in
  let bit_num = i mod 8 in
  let byte = String.get_uint8 buf byte_num in
  byte land (1 lsl bit_num) <> 0

module type Dh = sig
  type secret

  val secret_of_cs : ?compress:bool -> Cstruct.t ->
    (secret * Cstruct.t, error) result

  val gen_key : ?compress:bool -> ?g:Mirage_crypto_rng.g -> unit ->
    secret * Cstruct.t

  val key_exchange : secret -> Cstruct.t -> (Cstruct.t, error) result
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
  end
end
module type Dh_dsa = sig
  module Dh : Dh
  module Dsa : Dsa
end

type field_element = string

type out_field_element = bytes

let out_fe_to_fe = Bytes.unsafe_to_string

module type Parameters = sig
  val a : field_element
  val b : field_element
  val g_x : field_element
  val g_y : field_element
  val p : field_element
  val n : field_element
  val pident: string
  val byte_length : int
  val fe_length : int
  val first_byte_bits : int option
end

type point = { f_x : field_element; f_y : field_element; f_z : field_element }

type out_point = { m_f_x : out_field_element; m_f_y : out_field_element; m_f_z : out_field_element }

let out_p_to_p p = {
  f_x = out_fe_to_fe p.m_f_x ;
  f_y = out_fe_to_fe p.m_f_y ;
  f_z = out_fe_to_fe p.m_f_z ;
}

type scalar = Scalar of string

module type Foreign = sig
  val mul : out_field_element -> field_element -> field_element -> unit
  val sub : out_field_element -> field_element -> field_element -> unit
  val add : out_field_element -> field_element -> field_element -> unit
  val to_montgomery : out_field_element -> unit
  val from_bytes_buf : out_field_element -> string -> unit
  val set_one : out_field_element -> unit
  val nz : field_element -> bool
  val sqr : out_field_element -> field_element -> unit
  val from_montgomery : out_field_element -> unit
  val to_bytes_buf : bytes -> field_element -> unit
  val inv : out_field_element -> field_element -> unit
  val select_c : out_field_element -> bool -> field_element -> field_element -> unit

  val double_c : out_point -> point -> unit
  val add_c : out_point -> point -> point -> unit
end

module type Field_element = sig
  val create : unit -> out_field_element

  val copy : out_field_element -> field_element -> unit

  val one : unit -> out_field_element

  val to_bytes : bytes -> field_element -> unit

  val from_montgomery : out_field_element -> unit

  val add : out_field_element -> field_element -> field_element -> unit

  val sub : out_field_element -> field_element -> field_element -> unit

  val mul : out_field_element -> field_element -> field_element -> unit

  val nz : field_element -> bool

  val sqr : out_field_element -> field_element -> unit

  val inv : out_field_element -> field_element -> unit

  val from_be_bytes : string -> field_element

  val select : bool -> then_:field_element -> else_:field_element -> field_element
end

module Make_field_element (P : Parameters) (F : Foreign) : Field_element = struct
  include F

  let create () = Bytes.make P.fe_length '\000'

  let copy dst src = Bytes.blit_string src 0 dst 0 (String.length src)

  let from_bytes fe buf =
    if String.length buf = P.byte_length then
      F.from_bytes_buf fe buf
    else
      invalid_arg "buffer not of required byte length"

  let one () =
    let fe = create () in
    F.set_one fe;
    fe

  let to_bytes buf fe =
    if Bytes.length buf = P.byte_length then
      F.to_bytes_buf buf fe
    else
      invalid_arg "buffer not of required byte length"

  let from_be_bytes buf =
    let buf_rev = rev_string buf in
    let fe = create () in
    from_bytes fe buf_rev;
    F.to_montgomery fe;
    out_fe_to_fe fe

  let select bit ~then_ ~else_ =
    let out = create () in
    F.select_c out bit then_ else_;
    out_fe_to_fe out
end

module type Point = sig
  module Fe : Field_element

  val at_infinity : unit -> out_point

  val is_infinity : point -> bool

  val add : point -> point -> point

  val double : point -> point

  val of_bytes : string -> (point, error) result

  val to_bytes : compress:bool -> point -> string

  val to_affine_raw : point -> (field_element * field_element) option

  val x_of_finite_point : point -> bytes

  val params_g : point

  val select : bool -> then_:point -> else_:point -> point
end

module Make_point (P : Parameters) (F : Foreign) : Point = struct
  module Fe = Make_field_element(P)(F)

  let at_infinity () =
    let m_f_x = Fe.one () in
    let m_f_y = Fe.one () in
    let m_f_z = Fe.create () in
    { m_f_x; m_f_y; m_f_z }

  let is_infinity (p : point) = not (Fe.nz p.f_z)

  let is_solution_to_curve_equation =
    let a = Fe.from_be_bytes P.a in
    let b = Fe.from_be_bytes P.b in
    fun ~x ~y ->
      let x3 = Fe.create () in
      Fe.mul x3 x x;
      Fe.mul x3 (out_fe_to_fe x3) x;
      let ax = Fe.create () in
      Fe.mul ax a x;
      let y2 = Fe.create () in
      Fe.mul y2 y y;
      let sum = Fe.create () in
      Fe.add sum (out_fe_to_fe x3) (out_fe_to_fe ax);
      Fe.add sum (out_fe_to_fe sum) b;
      Fe.sub sum (out_fe_to_fe sum) (out_fe_to_fe y2);
      not (Fe.nz (out_fe_to_fe sum))

  let check_coordinate buf =
    (* ensure buf < p: *)
    match Eqaf.compare_be_with_len ~len:P.byte_length buf P.p >= 0 with
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
        let f_z = out_fe_to_fe (Fe.one ()) in
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
      Fe.inv z2 (out_fe_to_fe z1);
      Fe.sqr z1 (out_fe_to_fe z2);
      Fe.from_montgomery z1;
      let x = Fe.create () in
      Fe.copy x p.f_x;
      Fe.mul x (out_fe_to_fe x) (out_fe_to_fe z1);
      let y = Fe.create () in
      Fe.copy y p.f_y;
      Fe.mul z1 (out_fe_to_fe z1) (out_fe_to_fe z2);
      Fe.mul y (out_fe_to_fe y) (out_fe_to_fe z1);
      Some (out_fe_to_fe x, out_fe_to_fe y)

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
      | None -> String.make 1 '\000'
      | Some (x, y) ->
        let len_x = Bytes.length x and len_y = Bytes.length y in
        let res = Bytes.make (1 + len_x + len_y) '\000' in
        Bytes.set res 0 '\004' ;
        let rev_x = rev_bytes x and rev_y = rev_bytes y in
        Bytes.blit rev_x 0 res 1 len_x ;
        Bytes.blit rev_y 0 res (1 + len_x) len_y ;
        Bytes.unsafe_to_string res
    in
    if compress then
      let out = Bytes.make (P.byte_length + 1) '\000' in
      let ident =
        2 + (String.get_uint8 buf ((P.byte_length * 2) - 1)) land 1
      in
      Bytes.blit_string buf 1 out 1 P.byte_length;
      Bytes.set_uint8 out 0 ident;
      Bytes.unsafe_to_string out
    else
      buf

  let double p =
    let out = { m_f_x = Fe.create (); m_f_y = Fe.create (); m_f_z = Fe.create () } in
    F.double_c out p;
    out_p_to_p out

  let add fe_p fe_q =
    let out = { m_f_x = Fe.create (); m_f_y = Fe.create (); m_f_z = Fe.create () } in
    F.add_c out fe_p fe_q;
    out_p_to_p out

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
      out_fe_to_fe r
    in
    let sqr x =
      let r = Fe.create () in
      Fe.sqr r x;
      out_fe_to_fe r
    in
    fun x exp ->
    let r0 = ref (out_fe_to_fe (Fe.one ())) in
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
      let x = Fe.from_be_bytes (String.sub pk 1 P.byte_length) in
      let x3 = Fe.create () in
      let ax = Fe.create () in
      let sum = Fe.create () in
      Fe.mul x3 x x;
      Fe.mul x3 (out_fe_to_fe x3) x; (* x3 *)
      Fe.mul ax a x;  (* ax *)
      Fe.add sum (out_fe_to_fe x3) (out_fe_to_fe ax);
      Fe.add sum (out_fe_to_fe sum) b; (* y^2 *)
      let y = pow (out_fe_to_fe sum) pident in (* https://tools.ietf.org/id/draft-jivsov-ecc-compact-00.xml#sqrt point 4.3*)
      let y' = Fe.create () in
      Fe.sub y' p y;
      let y_struct = Bytes.make (P.byte_length) '\000' in
      let y =
        let tmp = Fe.create () in
        Fe.copy tmp y;
        tmp
      in
      Fe.from_montgomery y;
      Fe.to_bytes y_struct (out_fe_to_fe y); (* number must not be in montgomery domain*)
      let y_struct = rev_bytes y_struct in
      let y_struct2 = Bytes.make (P.byte_length) '\000' in
      Fe.from_montgomery y';
      Fe.to_bytes y_struct2 (out_fe_to_fe y');(* number must not be in montgomery domain*)
      let y_struct2 = rev_bytes y_struct2 in
      let ident = String.get_uint8 pk 0 in
      let signY =
        2 + (Bytes.get_uint8 y_struct (P.byte_length - 2)) land 1
      in
      let res = if Int.equal signY ident then y_struct else y_struct2 in
      let out = Bytes.make ((P.byte_length * 2) + 1) '\000' in
      Bytes.set out 0 '\004';
      Bytes.blit_string pk 1 out 1 P.byte_length;
      Bytes.blit res 0 out (P.byte_length + 1) P.byte_length;
      out_fe_to_fe out

  let of_bytes buf =
    let len = P.byte_length in
    if String.length buf = 0 then
      Error `Invalid_format
    else
      let of_bytes buf =
        let x = String.sub buf 1 len in
        let y = String.sub buf (1 + len) len in
        validate_finite_point ~x ~y
      in
      match String.get_uint8 buf 0 with
      | 0x00 when String.length buf = 1 ->
        Ok (out_p_to_p (at_infinity ()))
      | 0x02 | 0x03 when String.length P.pident > 0 ->
        let decompressed = decompress buf in
        of_bytes decompressed
      | 0x04 when String.length buf = 1 + len + len ->
        of_bytes buf
      | 0x00 | 0x04 -> Error `Invalid_length
      | _ -> Error `Invalid_format
end

module type Scalar = sig
  val not_zero : string -> bool

  val is_in_range : string -> bool

  val of_bytes : string -> (scalar, error) result

  val to_bytes : scalar -> string

  val scalar_mult : scalar -> point -> point
end

module Make_scalar (Param : Parameters) (P : Point) : Scalar = struct
  let not_zero =
    let zero = String.make Param.byte_length '\000' in
    fun buf -> not (Eqaf.equal buf zero)

  let is_in_range buf =
    not_zero buf
    && Eqaf.compare_be_with_len ~len:Param.byte_length Param.n buf > 0

  let of_bytes buf =
    match is_in_range buf with
    | exception Invalid_argument _ -> Error `Invalid_length
    | true -> Ok (Scalar (rev_string buf))
    | false -> Error `Invalid_range

  let to_bytes (Scalar buf) = rev_string buf

  let scalar_mult (Scalar s) p =
    let r0 = ref (out_p_to_p (P.at_infinity ())) in
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
    Result.map (fun (p, share) -> p, Cstruct.of_string share)
      (secret_of_bytes ?compress (Cstruct.to_string s))

  let rec generate_private_key ?g () =
    let candidate = Mirage_crypto_rng.generate ?g Param.byte_length in
    match S.of_bytes (Cstruct.to_string candidate) with
    | Ok secret -> secret
    | Error _ -> generate_private_key ?g ()

  let gen_bytes_key ?compress ?g () =
    let private_key = generate_private_key ?g () in
    (private_key, share ?compress private_key)

  let gen_key ?compress ?g () =
    let private_key, share = gen_bytes_key ?compress ?g () in
    private_key, Cstruct.of_string share

  let key_bytes_exchange secret received =
    match point_of_bytes received with
    | Error _ as err -> err
    | Ok shared -> Ok (P.x_of_finite_point (S.scalar_mult secret shared))

  let key_exchange secret received =
    match key_bytes_exchange secret (Cstruct.to_string received) with
    | Error _ as err -> err
    | Ok shared -> Ok (Cstruct.of_bytes shared)
end

module type Foreign_n = sig
  val mul : out_field_element -> field_element -> field_element -> unit
  val add : out_field_element -> field_element -> field_element -> unit
  val inv : out_field_element -> field_element -> unit
  val one : out_field_element -> unit
  val from_bytes : out_field_element -> string -> unit
  val to_bytes : bytes -> field_element -> unit
  val from_montgomery : out_field_element -> field_element -> unit
  val to_montgomery : out_field_element -> field_element -> unit
end

module Make_dsa (Param : Parameters) (F : Foreign_n) (P : Point) (S : Scalar) (H : Mirage_crypto.Hash.S) = struct
  let create () = Bytes.make Param.fe_length '\000'

  type priv = scalar

  let byte_length = Param.byte_length

  let priv_of_bytes= S.of_bytes

  let priv_to_bytes = S.to_bytes

  let priv_of_cstruct cs = priv_of_bytes (Cstruct.to_string cs)
  let priv_to_cstruct p = Cstruct.of_string (priv_to_bytes p)

  let padded msg =
    let l = String.length msg in
    let bl = Param.byte_length in
    let first_byte_ok () =
      match Param.first_byte_bits with
      | None -> true
      | Some m -> (String.get_uint8 msg 0) land (0xFF land (lnot m)) = 0
    in
    if l > bl || (l = bl && not (first_byte_ok ())) then
      raise Message_too_long
    else if l = bl then
      msg
    else
      ( let res = Bytes.make ((bl - l) + (String.length msg)) '\000' in
        Bytes.blit_string msg 0 res (bl - l) (String.length msg) ;
        Bytes.unsafe_to_string res )

  let padded_cs msg =
    let l = Cstruct.length msg in
    let bl = Param.byte_length in
    let first_byte_ok () =
      match Param.first_byte_bits with
      | None -> true
      | Some m -> (Cstruct.get_uint8 msg 0) land (0xFF land (lnot m)) = 0
    in
    if l > bl || (l = bl && not (first_byte_ok ())) then
      raise Message_too_long
    else if l = bl then
      msg
    else
      Cstruct.append (Cstruct.create (bl - l)) msg

  let from_be_bytes v =
    let v' = create () in
    F.from_bytes v' (rev_string v);
    v'

  let to_be_bytes v =
    let buf = Bytes.make Param.byte_length '\000' in
    F.to_bytes buf v;
    Bytes.unsafe_to_string (rev_bytes buf)

  (* RFC 6979: compute a deterministic k *)
  module K_gen (H : Mirage_crypto.Hash.S) = struct

    let drbg : 'a Mirage_crypto_rng.generator =
      let module M = Mirage_crypto_rng.Hmac_drbg (H) in (module M)

    let g ~key cs =
      let g = Mirage_crypto_rng.create ~strict:true drbg in
      Mirage_crypto_rng.reseed ~g
        (Cstruct.append (Cstruct.of_string (S.to_bytes key)) cs);
      g

    (* take qbit length, and ensure it is suitable for ECDSA (> 0 & < n) *)
    let gen g =
      let rec go () =
        let r = Mirage_crypto_rng.generate ~g Param.byte_length in
        let r = Cstruct.to_string r in
        if S.is_in_range r then r else go ()
      in
      go ()

    (* let generate_bytes ~key buf = gen (g ~key (Cstruct.of_bytes (padded buf))) *)

    let generate ~key buf =
      Cstruct.of_string (gen (g ~key (padded_cs buf)))
  end

  module K_gen_default = K_gen(H)

  type pub = point

  let pub_of_bytes = P.of_bytes

  let pub_to_bytes ?(compress = false) pk = P.to_bytes ~compress pk

  let pub_of_cstruct cs = pub_of_bytes (Cstruct.to_string cs)
  let pub_to_cstruct ?compress p =
    Cstruct.of_string (pub_to_bytes ?compress p)

  let generate ?g () =
    (* FIPS 186-4 B 4.2 *)
    let d =
      let rec one () =
        match S.of_bytes (Cstruct.to_string (Mirage_crypto_rng.generate ?g Param.byte_length)) with
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
      let x =
        let tmp = P.Fe.create () in
        P.Fe.copy tmp x;
        tmp
      in
      F.to_montgomery x (out_fe_to_fe x);
      let o = create () in
      F.one o;
      F.mul x (out_fe_to_fe x) (out_fe_to_fe o);
      F.from_montgomery x (out_fe_to_fe x);
      Some (to_be_bytes (out_fe_to_fe x))

  let sign_bytes ~key ?k msg =
    let msg = padded msg in
    let e = out_fe_to_fe (from_be_bytes msg) in
    let g = K_gen_default.g ~key (Cstruct.of_string msg) in
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
        F.to_montgomery r_mon (out_fe_to_fe r_mon);
        let kinv = create () in
        let kmon = from_be_bytes k' in
        F.to_montgomery kmon (out_fe_to_fe kmon);
        F.inv kinv (out_fe_to_fe kmon);
        F.to_montgomery kmon (out_fe_to_fe kinv);
        let rd = create () in
        let dmon = from_be_bytes (S.to_bytes key) in
        F.to_montgomery dmon (out_fe_to_fe dmon);
        F.mul rd (out_fe_to_fe r_mon) (out_fe_to_fe dmon);
        let cmon = create () in
        let zmon = create () in
        F.to_montgomery zmon e;
        F.add cmon (out_fe_to_fe zmon) (out_fe_to_fe rd);
        let smon = create () in
        F.mul smon (out_fe_to_fe kmon) (out_fe_to_fe cmon);
        let s = create () in
        F.from_montgomery s (out_fe_to_fe smon);
        let s = to_be_bytes (out_fe_to_fe s) in
        if S.not_zero s && S.not_zero r then
          r, s
        else
          again ()
    in
    do_sign g

  let sign ~key ?k msg =
    let r, s = sign_bytes ~key ?k:(Option.map Cstruct.to_string k) (Cstruct.to_string msg) in
    Cstruct.of_string r, Cstruct.of_string s

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
        F.to_montgomery s_mon (out_fe_to_fe s_mon);
        F.inv s_inv (out_fe_to_fe s_mon);
        let u1 = create () in
        F.to_montgomery s_inv (out_fe_to_fe s_inv);
        F.to_montgomery z (out_fe_to_fe z);
        F.mul u1 (out_fe_to_fe z) (out_fe_to_fe s_inv);
        let u2 = create () in
        let r_mon = from_be_bytes r in
        F.to_montgomery r_mon (out_fe_to_fe r_mon);
        F.mul u2 (out_fe_to_fe r_mon) (out_fe_to_fe s_inv);
        F.from_montgomery u1 (out_fe_to_fe u1);
        F.from_montgomery u2 (out_fe_to_fe u2);
        match
          S.of_bytes (to_be_bytes (out_fe_to_fe u1)),
          S.of_bytes (to_be_bytes (out_fe_to_fe u2))
        with
        | Ok u1, Ok u2 ->
          let point =
            P.add
              (S.scalar_mult u1 P.params_g)
              (S.scalar_mult u2 key)
          in
          begin match x_of_finite_point_mod_n point with
            | None -> false (* point is infinity *)
            | Some r' -> String.equal r r'
          end
        | Error _, _ | _, Error _ -> false
    with
    | Message_too_long -> false

  let verify ~key (r, s) digest =
    verify_bytes ~key (Cstruct.to_string r, Cstruct.to_string s) (Cstruct.to_string digest)
end

module P224 : Dh_dsa = struct
  module Params = struct
    let a = "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFE\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFE"
    let b = "\xB4\x05\x0A\x85\x0C\x04\xB3\xAB\xF5\x41\x32\x56\x50\x44\xB0\xB7\xD7\xBF\xD8\xBA\x27\x0B\x39\x43\x23\x55\xFF\xB4"
    let g_x = "\xB7\x0E\x0C\xBD\x6B\xB4\xBF\x7F\x32\x13\x90\xB9\x4A\x03\xC1\xD3\x56\xC2\x11\x22\x34\x32\x80\xD6\x11\x5C\x1D\x21"
    let g_y = "\xBD\x37\x63\x88\xB5\xF7\x23\xFB\x4C\x22\xDF\xE6\xCD\x43\x75\xA0\x5A\x07\x47\x64\x44\xD5\x81\x99\x85\x00\x7E\x34"
    let p = "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01"
    let n = "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\x16\xA2\xE0\xB8\xF0\x3E\x13\xDD\x29\x45\x5C\x5C\x2A\x3D"
    let pident = ""
    let byte_length = 28
    let fe_length = if Sys.word_size == 64 then 32 else 28 (* TODO: is this congruent with C code? *)
    let first_byte_bits = None
  end

  module Foreign = struct
    external mul : out_field_element -> field_element -> field_element -> unit = "mc_p224_mul" [@@noalloc]
    external sub : out_field_element -> field_element -> field_element -> unit = "mc_p224_sub" [@@noalloc]
    external add : out_field_element -> field_element -> field_element -> unit = "mc_p224_add" [@@noalloc]
    external to_montgomery : out_field_element -> unit = "mc_p224_to_montgomery" [@@noalloc]
    external from_bytes_buf : out_field_element -> string -> unit = "mc_p224_from_bytes" [@@noalloc]
    external set_one : out_field_element -> unit = "mc_p224_set_one" [@@noalloc]
    external nz : field_element -> bool = "mc_p224_nz" [@@noalloc]
    external sqr : out_field_element -> field_element -> unit = "mc_p224_sqr" [@@noalloc]
    external from_montgomery : out_field_element -> unit = "mc_p224_from_montgomery" [@@noalloc]
    external to_bytes_buf : bytes -> field_element -> unit = "mc_p224_to_bytes" [@@noalloc]
    external inv : out_field_element -> field_element -> unit = "mc_p224_inv" [@@noalloc]
    external select_c : out_field_element -> bool -> field_element -> field_element -> unit = "mc_p224_select" [@@noalloc]

    external double_c : out_point -> point -> unit = "mc_p224_point_double" [@@noalloc]
    external add_c : out_point -> point -> point -> unit = "mc_p224_point_add" [@@noalloc]
  end

  module Foreign_n = struct
    external mul : out_field_element -> field_element -> field_element -> unit = "mc_np224_mul" [@@noalloc]
    external add : out_field_element -> field_element -> field_element -> unit = "mc_np224_add" [@@noalloc]
    external inv : out_field_element -> field_element -> unit = "mc_np224_inv" [@@noalloc]
    external one : out_field_element -> unit = "mc_np224_one" [@@noalloc]
    external from_bytes : out_field_element -> string -> unit = "mc_np224_from_bytes" [@@noalloc]
    external to_bytes : bytes -> field_element -> unit = "mc_np224_to_bytes" [@@noalloc]
    external from_montgomery : out_field_element -> field_element -> unit = "mc_np224_from_montgomery" [@@noalloc]
    external to_montgomery : out_field_element -> field_element -> unit = "mc_np224_to_montgomery" [@@noalloc]
  end

  module P = Make_point(Params)(Foreign)
  module S = Make_scalar(Params)(P)
  module Dh = Make_dh(Params)(P)(S)
  module Dsa = Make_dsa(Params)(Foreign_n)(P)(S)(Mirage_crypto.Hash.SHA256)
end

module P256 : Dh_dsa  = struct
  module Params = struct
    let a = "\xFF\xFF\xFF\xFF\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFC"
    let b = "\x5A\xC6\x35\xD8\xAA\x3A\x93\xE7\xB3\xEB\xBD\x55\x76\x98\x86\xBC\x65\x1D\x06\xB0\xCC\x53\xB0\xF6\x3B\xCE\x3C\x3E\x27\xD2\x60\x4B"
    let g_x = "\x6B\x17\xD1\xF2\xE1\x2C\x42\x47\xF8\xBC\xE6\xE5\x63\xA4\x40\xF2\x77\x03\x7D\x81\x2D\xEB\x33\xA0\xF4\xA1\x39\x45\xD8\x98\xC2\x96"
    let g_y = "\x4F\xE3\x42\xE2\xFE\x1A\x7F\x9B\x8E\xE7\xEB\x4A\x7C\x0F\x9E\x16\x2B\xCE\x33\x57\x6B\x31\x5E\xCE\xCB\xB6\x40\x68\x37\xBF\x51\xF5"
    let p = "\xFF\xFF\xFF\xFF\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
    let n = "\xFF\xFF\xFF\xFF\x00\x00\x00\x00\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xBC\xE6\xFA\xAD\xA7\x17\x9E\x84\xF3\xB9\xCA\xC2\xFC\x63\x25\x51"
    let pident = "\x3F\xFF\xFF\xFF\xC0\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" |> rev_string (* (Params.p + 1) / 4*)
    let byte_length = 32
    let fe_length = 32
    let first_byte_bits = None
  end

  module Foreign = struct
    external mul : out_field_element -> field_element -> field_element -> unit = "mc_p256_mul" [@@noalloc]
    external sub : out_field_element -> field_element -> field_element -> unit = "mc_p256_sub" [@@noalloc]
    external add : out_field_element -> field_element -> field_element -> unit = "mc_p256_add" [@@noalloc]
    external to_montgomery : out_field_element -> unit = "mc_p256_to_montgomery" [@@noalloc]
    external from_bytes_buf : out_field_element -> string -> unit = "mc_p256_from_bytes" [@@noalloc]
    external set_one : out_field_element -> unit = "mc_p256_set_one" [@@noalloc]
    external nz : field_element -> bool = "mc_p256_nz" [@@noalloc]
    external sqr : out_field_element -> field_element -> unit = "mc_p256_sqr" [@@noalloc]
    external from_montgomery : out_field_element -> unit = "mc_p256_from_montgomery" [@@noalloc]
    external to_bytes_buf : bytes -> field_element -> unit = "mc_p256_to_bytes" [@@noalloc]
    external inv : out_field_element -> field_element -> unit = "mc_p256_inv" [@@noalloc]
    external select_c : out_field_element -> bool -> field_element -> field_element -> unit = "mc_p256_select" [@@noalloc]

    external double_c : out_point -> point -> unit = "mc_p256_point_double" [@@noalloc]
    external add_c : out_point -> point -> point -> unit = "mc_p256_point_add" [@@noalloc]
  end

  module Foreign_n = struct
    external mul : out_field_element -> field_element -> field_element -> unit = "mc_np256_mul" [@@noalloc]
    external add : out_field_element -> field_element -> field_element -> unit = "mc_np256_add" [@@noalloc]
    external inv : out_field_element -> field_element -> unit = "mc_np256_inv" [@@noalloc]
    external one : out_field_element -> unit = "mc_np256_one" [@@noalloc]
    external from_bytes : out_field_element -> string -> unit = "mc_np256_from_bytes" [@@noalloc]
    external to_bytes : bytes -> field_element -> unit = "mc_np256_to_bytes" [@@noalloc]
    external from_montgomery : out_field_element -> field_element -> unit = "mc_np256_from_montgomery" [@@noalloc]
    external to_montgomery : out_field_element -> field_element -> unit = "mc_np256_to_montgomery" [@@noalloc]
  end

  module P = Make_point(Params)(Foreign)
  module S = Make_scalar(Params)(P)
  module Dh = Make_dh(Params)(P)(S)
  module Dsa = Make_dsa(Params)(Foreign_n)(P)(S)(Mirage_crypto.Hash.SHA256)
end

module P384 : Dh_dsa = struct
  module Params = struct
    let a = "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFE\xFF\xFF\xFF\xFF\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\xFF\xFC"
    let b = "\xB3\x31\x2F\xA7\xE2\x3E\xE7\xE4\x98\x8E\x05\x6B\xE3\xF8\x2D\x19\x18\x1D\x9C\x6E\xFE\x81\x41\x12\x03\x14\x08\x8F\x50\x13\x87\x5A\xC6\x56\x39\x8D\x8A\x2E\xD1\x9D\x2A\x85\xC8\xED\xD3\xEC\x2A\xEF"
    let g_x = "\xAA\x87\xCA\x22\xBE\x8B\x05\x37\x8E\xB1\xC7\x1E\xF3\x20\xAD\x74\x6E\x1D\x3B\x62\x8B\xA7\x9B\x98\x59\xF7\x41\xE0\x82\x54\x2A\x38\x55\x02\xF2\x5D\xBF\x55\x29\x6C\x3A\x54\x5E\x38\x72\x76\x0A\xB7"
    let g_y =
"\x36\x17\xde\x4a\x96\x26\x2c\x6f\x5d\x9e\x98\xbf\x92\x92\xdc\x29\xf8\xf4\x1d\xbd\x28\x9a\x14\x7c\xe9\xda\x31\x13\xb5\xf0\xb8\xc0\x0a\x60\xb1\xce\x1d\x7e\x81\x9d\x7a\x43\x1d\x7c\x90\xea\x0e\x5f"
    let p = "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFE\xFF\xFF\xFF\xFF\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\xFF\xFF"
    let n = "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xC7\x63\x4D\x81\xF4\x37\x2D\xDF\x58\x1A\x0D\xB2\x48\xB0\xA7\x7A\xEC\xEC\x19\x6A\xCC\xC5\x29\x73"
    let pident = "\x3F\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xBF\xFF\xFF\xFF\xC0\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00" |> rev_string (* (Params.p + 1) / 4*)
    let byte_length = 48
    let fe_length = 48
    let first_byte_bits = None
  end

  module Foreign = struct
    external mul : out_field_element -> field_element -> field_element -> unit = "mc_p384_mul" [@@noalloc]
    external sub : out_field_element -> field_element -> field_element -> unit = "mc_p384_sub" [@@noalloc]
    external add : out_field_element -> field_element -> field_element -> unit = "mc_p384_add" [@@noalloc]
    external to_montgomery : out_field_element -> unit = "mc_p384_to_montgomery" [@@noalloc]
    external from_bytes_buf : out_field_element -> string -> unit = "mc_p384_from_bytes" [@@noalloc]
    external set_one : out_field_element -> unit = "mc_p384_set_one" [@@noalloc]
    external nz : field_element -> bool = "mc_p384_nz" [@@noalloc]
    external sqr : out_field_element -> field_element -> unit = "mc_p384_sqr" [@@noalloc]
    external from_montgomery : out_field_element -> unit = "mc_p384_from_montgomery" [@@noalloc]
    external to_bytes_buf : bytes -> field_element -> unit = "mc_p384_to_bytes" [@@noalloc]
    external inv : out_field_element -> field_element -> unit = "mc_p384_inv" [@@noalloc]
    external select_c : out_field_element -> bool -> field_element -> field_element -> unit = "mc_p384_select" [@@noalloc]

    external double_c : out_point -> point -> unit = "mc_p384_point_double" [@@noalloc]
    external add_c : out_point -> point -> point -> unit = "mc_p384_point_add" [@@noalloc]
  end

  module Foreign_n = struct
    external mul : out_field_element -> field_element -> field_element -> unit = "mc_np384_mul" [@@noalloc]
    external add : out_field_element -> field_element -> field_element -> unit = "mc_np384_add" [@@noalloc]
    external inv : out_field_element -> field_element -> unit = "mc_np384_inv" [@@noalloc]
    external one : out_field_element -> unit = "mc_np384_one" [@@noalloc]
    external from_bytes : out_field_element -> string -> unit = "mc_np384_from_bytes" [@@noalloc]
    external to_bytes : bytes -> field_element -> unit = "mc_np384_to_bytes" [@@noalloc]
    external from_montgomery : out_field_element -> field_element -> unit = "mc_np384_from_montgomery" [@@noalloc]
    external to_montgomery : out_field_element -> field_element -> unit = "mc_np384_to_montgomery" [@@noalloc]
  end

  module P = Make_point(Params)(Foreign)
  module S = Make_scalar(Params)(P)
  module Dh = Make_dh(Params)(P)(S)
  module Dsa = Make_dsa(Params)(Foreign_n)(P)(S)(Mirage_crypto.Hash.SHA384)
end

module P521 : Dh_dsa = struct
  module Params = struct
    let a = "\x01\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFC"
    let b = "\x00\x51\x95\x3E\xB9\x61\x8E\x1C\x9A\x1F\x92\x9A\x21\xA0\xB6\x85\x40\xEE\xA2\xDA\x72\x5B\x99\xB3\x15\xF3\xB8\xB4\x89\x91\x8E\xF1\x09\xE1\x56\x19\x39\x51\xEC\x7E\x93\x7B\x16\x52\xC0\xBD\x3B\xB1\xBF\x07\x35\x73\xDF\x88\x3D\x2C\x34\xF1\xEF\x45\x1F\xD4\x6B\x50\x3F\x00"
    let g_x =
"\x00\xC6\x85\x8E\x06\xB7\x04\x04\xE9\xCD\x9E\x3E\xCB\x66\x23\x95\xB4\x42\x9C\x64\x81\x39\x05\x3F\xB5\x21\xF8\x28\xAF\x60\x6B\x4D\x3D\xBA\xA1\x4B\x5E\x77\xEF\xE7\x59\x28\xFE\x1D\xC1\x27\xA2\xFF\xA8\xDE\x33\x48\xB3\xC1\x85\x6A\x42\x9B\xF9\x7E\x7E\x31\xC2\xE5\xBD\x66"
    let g_y =
"\x01\x18\x39\x29\x6a\x78\x9a\x3b\xc0\x04\x5c\x8a\x5f\xb4\x2c\x7d\x1b\xd9\x98\xf5\x44\x49\x57\x9b\x44\x68\x17\xaf\xbd\x17\x27\x3e\x66\x2c\x97\xee\x72\x99\x5e\xf4\x26\x40\xc5\x50\xb9\x01\x3f\xad\x07\x61\x35\x3c\x70\x86\xa2\x72\xc2\x40\x88\xbe\x94\x76\x9f\xd1\x66\x50"
    let p = "\x01\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
    let n = "\x01\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFA\x51\x86\x87\x83\xBF\x2F\x96\x6B\x7F\xCC\x01\x48\xF7\x09\xA5\xD0\x3B\xB5\xC9\xB8\x89\x9C\x47\xAE\xBB\x6F\xB7\x1E\x91\x38\x64\x09"
    let pident = "\x01\x7f\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" |> rev_string
    let byte_length = 66
    let fe_length = if Sys.word_size == 64 then 72 else 68  (* TODO: is this congruent with C code? *)
    let first_byte_bits = Some 0x01
  end

  module Foreign = struct
    external mul : out_field_element -> field_element -> field_element -> unit = "mc_p521_mul" [@@noalloc]
    external sub : out_field_element -> field_element -> field_element -> unit = "mc_p521_sub" [@@noalloc]
    external add : out_field_element -> field_element -> field_element -> unit = "mc_p521_add" [@@noalloc]
    external to_montgomery : out_field_element -> unit = "mc_p521_to_montgomery" [@@noalloc]
    external from_bytes_buf : out_field_element -> string -> unit = "mc_p521_from_bytes" [@@noalloc]
    external set_one : out_field_element -> unit = "mc_p521_set_one" [@@noalloc]
    external nz : field_element -> bool = "mc_p521_nz" [@@noalloc]
    external sqr : out_field_element -> field_element -> unit = "mc_p521_sqr" [@@noalloc]
    external from_montgomery : out_field_element -> unit = "mc_p521_from_montgomery" [@@noalloc]
    external to_bytes_buf : bytes -> field_element -> unit = "mc_p521_to_bytes" [@@noalloc]
    external inv : out_field_element -> field_element -> unit = "mc_p521_inv" [@@noalloc]
    external select_c : out_field_element -> bool -> field_element -> field_element -> unit = "mc_p521_select" [@@noalloc]

    external double_c : out_point -> point -> unit = "mc_p521_point_double" [@@noalloc]
    external add_c : out_point -> point -> point -> unit = "mc_p521_point_add" [@@noalloc]
  end

  module Foreign_n = struct
    external mul : out_field_element -> field_element -> field_element -> unit = "mc_np521_mul" [@@noalloc]
    external add : out_field_element -> field_element -> field_element -> unit = "mc_np521_add" [@@noalloc]
    external inv : out_field_element -> field_element -> unit = "mc_np521_inv" [@@noalloc]
    external one : out_field_element -> unit = "mc_np521_one" [@@noalloc]
    external from_bytes : out_field_element -> string -> unit = "mc_np521_from_bytes" [@@noalloc]
    external to_bytes : bytes -> field_element -> unit = "mc_np521_to_bytes" [@@noalloc]
    external from_montgomery : out_field_element -> field_element -> unit = "mc_np521_from_montgomery" [@@noalloc]
    external to_montgomery : out_field_element -> field_element -> unit = "mc_np521_to_montgomery" [@@noalloc]
  end

  module P = Make_point(Params)(Foreign)
  module S = Make_scalar(Params)(P)
  module Dh = Make_dh(Params)(P)(S)
  module Dsa = Make_dsa(Params)(Foreign_n)(P)(S)(Mirage_crypto.Hash.SHA512)
end

module X25519 = struct
  (* RFC 7748 *)
  external x25519_scalar_mult_generic : bytes -> string -> string -> unit = "mc_x25519_scalar_mult_generic" [@@noalloc]

  let key_len = 32

  let scalar_mult in_ base =
    let out = Bytes.make key_len '\000' in
    x25519_scalar_mult_generic out in_ base;
    Bytes.unsafe_to_string out

  type secret = string

  let basepoint =
    String.init key_len (function 0 -> '\009' | _ -> '\000')

  let public priv = scalar_mult priv basepoint

  let gen_octets_key ?compress:_ ?g () =
    let secret = Mirage_crypto_rng.generate ?g key_len in
    Cstruct.to_string secret, public (Cstruct.to_string secret)

  let gen_key ?compress ?g () =
    let secret, public = gen_octets_key ~compress ?g () in
    secret, Cstruct.of_string public

  let secret_of_octets ?compress:_ s =
    if String.length s = key_len then
      Ok (s, public s)
    else
      Error `Invalid_length

  let secret_of_cs ?compress cs =
    Result.map (fun (secret, public) -> secret, Cstruct.of_string public)
      (secret_of_octets ~compress (Cstruct.to_string cs))

  let is_zero =
    let zero = String.make key_len '\000' in
    fun buf -> String.equal zero buf

  let key_octets_exchange secret public =
    if String.length public = key_len then
      let res = scalar_mult secret public in
      if is_zero res then Error `Low_order else Ok res
    else
      Error `Invalid_length

  let key_exchange secret public =
    Result.map Cstruct.of_string
      (key_octets_exchange secret (Cstruct.to_string public))
end

module Ed25519 = struct

  external scalar_mult_base_to_bytes : bytes -> string -> unit = "mc_25519_scalar_mult_base" [@@noalloc]
  external reduce_l : bytes -> unit = "mc_25519_reduce_l" [@@noalloc]
  external muladd : bytes -> string -> string -> string -> unit = "mc_25519_muladd" [@@noalloc]
  external double_scalar_mult : bytes -> string -> string -> string -> bool = "mc_25519_double_scalar_mult" [@@noalloc]
  external pub_ok : string -> bool = "mc_25519_pub_ok" [@@noalloc]

  type pub = string

  type priv = string

  (* RFC 8032 *)
  let key_len = 32

  let public secret =
    (* section 5.1.5 *)
    (* step 1 *)
    let h = Mirage_crypto.Hash.SHA512.digest (Cstruct.of_string secret) in
    (* step 2 *)
    let s, rest = Cstruct.split h key_len in
    let s, rest = Cstruct.to_bytes s, Cstruct.to_string rest in
    Bytes.set_uint8 s 0 ((Bytes.get_uint8 s 0) land 248);
    Bytes.set_uint8 s 31 (((Bytes.get_uint8 s 31) land 127) lor 64);
    let s = Bytes.unsafe_to_string s in
    (* step 3 and 4 *)
    let public = Bytes.make key_len '\000' in
    scalar_mult_base_to_bytes public s;
    let public = Bytes.unsafe_to_string public in
    public, (s, rest)

  let pub_of_priv secret = fst (public secret)

  let priv_of_octets buf =
    if String.length buf = key_len then Ok buf else Error `Invalid_length

  let priv_of_cstruct p = priv_of_octets (Cstruct.to_string p)

  let priv_to_octets priv = priv

  let priv_to_cstruct p = Cstruct.of_string (priv_to_octets p)

  let pub_of_octets buf =
    if String.length buf = key_len then
      if pub_ok buf then
        Ok buf
      else
        Error `Not_on_curve
    else
      Error `Invalid_length

  let pub_of_cstruct p = pub_of_octets (Cstruct.to_string p)

  let pub_to_octets pub = pub

  let pub_to_cstruct p = Cstruct.of_string (pub_to_octets p)

  let generate ?g () =
    let secret = Mirage_crypto_rng.generate ?g key_len in
    let secret = Cstruct.to_string secret in
    secret, pub_of_priv secret

  let sign ~key msg =
    (* section 5.1.6 *)
    let pub, (s, prefix) = public key in
    let r = Mirage_crypto.Hash.SHA512.digest (Cstruct.of_string (String.concat "" [ prefix; msg ])) in
    let r = Cstruct.to_bytes r in
    reduce_l r;
    let r = Bytes.unsafe_to_string r in
    let r_big = Bytes.make key_len '\000' in
    scalar_mult_base_to_bytes r_big r;
    let r_big = Bytes.unsafe_to_string r_big in
    let k = Mirage_crypto.Hash.SHA512.digest (Cstruct.of_string (String.concat "" [ r_big; pub; msg])) in
    let k = Cstruct.to_bytes k in
    reduce_l k;
    let k = Bytes.unsafe_to_string k in
    let s_out = Bytes.make key_len '\000' in
    muladd s_out k s r;
    let res = Bytes.make (key_len + key_len) '\000' in
    Bytes.blit_string r_big 0 res 0 key_len ;
    Bytes.blit s_out 0 res key_len key_len ;
    Bytes.unsafe_to_string res

  let sign ~key msg = Cstruct.of_string (sign ~key (Cstruct.to_string msg))

  let verify ~key signature ~msg =
    (* section 5.1.7 *)
    if String.length signature = 2 * key_len then
      let r, s =
        String.sub signature 0 key_len,
        String.sub signature key_len key_len
      in
      let s_smaller_l =
        (* check s within 0 <= s < L *)
        let s' = Bytes.make (key_len * 2) '\000' in
        Bytes.blit_string s 0 s' 0 key_len;
        reduce_l s';
        let s' = Bytes.unsafe_to_string s' in
        let s'' = String.concat "" [ s; String.make key_len '\000' ] in
        String.equal s'' s'
      in
      if s_smaller_l then begin
        let k =
          let data_to_hash = String.concat "" [ r ; key ; msg ] in
          Mirage_crypto.Hash.SHA512.digest (Cstruct.of_string data_to_hash)
        in
        let k = Cstruct.to_bytes k in
        reduce_l k;
        let k = Bytes.unsafe_to_string k in
        let r' = Bytes.make key_len '\000' in
        let success = double_scalar_mult r' k key s in
        let r' = Bytes.unsafe_to_string r' in
        success && String.equal r r'
      end else
        false
    else
      false

  let verify ~key signature ~msg =
    verify ~key (Cstruct.to_string signature) ~msg:(Cstruct.to_string msg)
end
