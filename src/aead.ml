module type AEAD = sig
  val tag_size : int
  type key
  val of_secret : Cstruct.t -> key
  val authenticate_encrypt : key:key -> nonce:Cstruct.t -> ?adata:Cstruct.t ->
    Cstruct.t -> Cstruct.t
  val authenticate_decrypt : key:key -> nonce:Cstruct.t -> ?adata:Cstruct.t ->
    Cstruct.t -> Cstruct.t option
  val authenticate_encrypt_tag : key:key -> nonce:Cstruct.t ->
    ?adata:Cstruct.t -> Cstruct.t -> Cstruct.t * Cstruct.t
  val authenticate_decrypt_tag : key:key -> nonce:Cstruct.t -> ?adata:Cstruct.t ->
    tag:Cstruct.t -> Cstruct.t -> Cstruct.t option
end
