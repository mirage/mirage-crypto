module type S = sig
  type 'a iter = 'a Uncommon.iter

  type t
  val mac_size : int

  val empty : key:string -> t
  val feed : t -> string -> t
  val feedi : t -> string iter -> t
  val get : t -> string

  val mac : key:string -> string -> string
  val maci : key:string -> string iter -> string
  val macl : key:string -> string list -> string
end

module It : S = struct
  type 'a iter = 'a Uncommon.iter

  module P = Native.Poly1305
  let mac_size = P.mac_size ()

  type t = bytes

  let dup = Bytes.copy

  let empty ~key =
    let ctx = Bytes.create (P.ctx_size ()) in
    if String.length key <> 32 then invalid_arg "Poly1305 key must be 32 bytes" ;
    P.init ctx key ;
    ctx

  let update ctx data =
    P.update ctx data (String.length data)

  let feed ctx cs =
    let t = dup ctx in
    update t cs ;
    t

  let feedi ctx iter =
    let t = dup ctx in
    iter (update t) ;
    t

  let final ctx =
    let res = Bytes.create mac_size in
    P.finalize ctx res;
    Bytes.unsafe_to_string res

  let get ctx = final (dup ctx)

  let mac ~key data = feed (empty ~key) data |> final

  let maci ~key iter = feedi (empty ~key) iter |> final

  let macl ~key datas =
    let ctx = empty ~key in
    List.iter (update ctx) datas;
    final ctx
end
