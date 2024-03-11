(** [Uncommon] is a [Common], now with less name clashes. *)

let kasprintf k fmt =
  Format.(kfprintf (fun _ -> k (flush_str_formatter ())) str_formatter fmt)

let invalid_arg fmt = kasprintf invalid_arg ("Mirage_crypto: " ^^ fmt)
let failwith fmt = kasprintf failwith ("Mirage_crypto: " ^^ fmt)

let (//) x y =
  if y < 1 then raise Division_by_zero else
    if x > 0 then 1 + ((x - 1) / y) else 0 [@@inline]

let imin (a : int) b = if a < b then a else b
let imax (a : int) b = if a < b then b else a

type 'a iter = ('a -> unit) -> unit

let iter2 a b   f = f a; f b
let iter3 a b c f = f a; f b; f c

let xor_into src dst n =
  Native.xor_into_bytes src 0 dst 0 n

let xor a b =
  assert (String.length a = String.length b);
  let b' = Bytes.of_string b in
  xor_into a b' (Bytes.length b');
  Bytes.unsafe_to_string b'

module Cs = struct

  open Cstruct

  let (<+>) = append

  let clone ?len cs =
    let len = match len with None -> cs.len | Some x -> x in
    let cs' = create_unsafe len in
    ( blit cs 0 cs' 0 len ; cs' )

  let xor_into src dst n =
    if n > imin (length src) (length dst) then
      invalid_arg "Uncommon.Cs.xor_into: buffers to small (need %d)" n
    else Native.xor_into src.buffer src.off dst.buffer dst.off n

  let xor cs1 cs2 =
    let len = imin (length cs1) (length cs2) in
    let cs  = clone ~len cs2 in
    ( xor_into cs1 cs len ; cs )

  let split3 cs l1 l2 =
    let l12 = l1 + l2 in
    (sub cs 0 l1, sub cs l1 l2, sub cs l12 (length cs - l12))

  let rpad cs size x =
    let l = length cs and cs' = Cstruct.create_unsafe size in
    if size < l then invalid_arg "Uncommon.Cs.rpad: size < len";
    blit cs 0 cs' 0 l ;
    memset (sub cs' l (size - l)) x ;
    cs'

  let lpad cs size x =
    let l = length cs and cs' = Cstruct.create_unsafe size in
    if size < l then invalid_arg "Uncommon.Cs.lpad: size < len";
    blit cs 0 cs' (size - l) l ;
    memset (sub cs' 0 (size - l)) x ;
    cs'

  let of_bytes xs =
    let cs = Cstruct.create_unsafe @@ List.length xs in
    List.iteri (fun i x -> set_uint8 cs i x) xs;
    cs

  let b x =
    let cs = Cstruct.create_unsafe 1 in ( set_uint8 cs 0 x ; cs )

end
