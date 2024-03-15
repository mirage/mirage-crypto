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
end
