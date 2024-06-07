module type AEAD = sig
  val tag_size : int
  type key
  val of_secret : string -> key
  val authenticate_encrypt : key:key -> nonce:string -> ?adata:string ->
    string -> string
  val authenticate_decrypt : key:key -> nonce:string -> ?adata:string ->
    string -> string option
  val authenticate_encrypt_tag : key:key -> nonce:string -> ?adata:string ->
    string -> string * string
  val authenticate_decrypt_tag : key:key -> nonce:string -> ?adata:string ->
    tag:string -> string -> string option
  val authenticate_encrypt_into : key:key -> nonce:string ->
    ?adata:string -> string -> src_off:int -> bytes -> dst_off:int ->
    tag_off:int -> int -> unit
  val authenticate_decrypt_into : key:key -> nonce:string ->
    ?adata:string -> string -> src_off:int -> tag_off:int -> bytes ->
    dst_off:int -> int -> bool
  val unsafe_authenticate_encrypt_into : key:key -> nonce:string ->
    ?adata:string -> string -> src_off:int -> bytes -> dst_off:int ->
    tag_off:int -> int -> unit
  val unsafe_authenticate_decrypt_into : key:key -> nonce:string ->
    ?adata:string -> string -> src_off:int -> tag_off:int -> bytes ->
    dst_off:int -> int -> bool
end
