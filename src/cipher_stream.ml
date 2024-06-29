open Uncommon

module type Stream = sig
  type key
  type result = { message : string ; key : key }
  val of_secret : string -> key
  val encrypt : key:key -> string -> result
  val decrypt : key:key -> string -> result
end

module ARC4 = struct

  type key = int * int * int array

  type result = { message : string ; key : key }

  let of_secret buf =
    let len = String.length buf in
    if len < 1 || len > 256 then invalid_arg "ARC4.of_secret: key size %d" len;
    let s = Array.init 256 (fun x -> x) in
    let rec loop j = function
      | 256 -> ()
      | i ->
          let x = String.get_uint8 buf (i mod len) in
          let si = s.(i) in
          let j = (j + si + x) land 0xff in
          let sj = s.(j) in
          s.(i) <- sj ; s.(j) <- si ;
          (loop [@tailcall]) j (succ i)
    in
    ( loop 0 0 ; (0, 0, s) )

  let encrypt ~key:(i, j, s') buf =
    let s   = Array.copy s'
    and len = String.length buf in
    let res = Bytes.create len in
    let rec mix i j = function
      | n when n = len -> (i, j, s)
      | n ->
          let i  = succ i land 0xff in
          let si = s.(i) in
          let j  = (j + si) land 0xff in
          let sj = s.(j) in
          s.(i) <- sj ; s.(j) <- si ;
          let k  = s.((si + sj) land 0xff) in
          Bytes.set_uint8 res n (k lxor String.get_uint8 buf n);
          (mix [@tailcall]) i j (succ n)
    in
    let key' = mix i j 0 in
    { key = key' ; message = Bytes.unsafe_to_string res }

  let decrypt = encrypt

end
