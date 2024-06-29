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
  val mac_into : key:string -> (string * int * int) list -> bytes -> dst_off:int -> unit
  val unsafe_mac_into : key:string -> (string * int * int) list -> bytes -> dst_off:int -> unit
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
    P.update ctx data 0 (String.length data)

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
    P.finalize ctx res 0;
    Bytes.unsafe_to_string res

  let get ctx = final (dup ctx)

  let mac ~key data = feed (empty ~key) data |> final

  let maci ~key iter = feedi (empty ~key) iter |> final

  let unsafe_mac_into ~key datas dst ~dst_off =
    let ctx = empty ~key in
    List.iter (fun (d, off, len) -> P.update ctx d off len) datas;
    P.finalize ctx dst dst_off

  let mac_into ~key datas dst ~dst_off =
    if Bytes.length dst - dst_off < mac_size then
      Uncommon.invalid_arg "Poly1305: dst length %u - off %u < len %u"
        (Bytes.length dst) dst_off mac_size;
    if dst_off < 0 then
      Uncommon.invalid_arg "Poly1305: dst_off %u < 0" dst_off;
    let ctx = empty ~key in
    List.iter (fun (d, off, len) ->
        if off < 0 then
          Uncommon.invalid_arg "Poly1305: d off %u < 0" off;
        if String.length d - off < len then
          Uncommon.invalid_arg "Poly1305: d length %u - off %u < len %u"
            (String.length d) off len;
        P.update ctx d off len)
      datas;
    P.finalize ctx dst dst_off
end
