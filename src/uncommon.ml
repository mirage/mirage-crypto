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

let xor_into src ?(src_off = 0) dst ?(dst_off = 0) n =
  Native.xor_into_bytes src src_off dst dst_off n

let xor a b =
  assert (String.length a = String.length b);
  let b' = Bytes.of_string b in
  xor_into a b' (Bytes.length b');
  Bytes.unsafe_to_string b'

(* revise once OCaml 4.13 is the lower bound *)
let string_get_uint8 buf idx =
    Bytes.get_uint8 (Bytes.unsafe_of_string buf) idx
