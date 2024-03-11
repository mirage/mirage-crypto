
open Stdlib.Bigarray

let buffer = Array1.create char c_layout


type buffer = (char, int8_unsigned_elt, c_layout) Array1.t

type off    = int
type size   = int
type secret = buffer
type key    = buffer
type ctx    = bytes


module AES = struct
  external enc      : buffer -> off -> buffer -> off -> key -> int -> size -> unit = "mc_aes_enc_bc" "mc_aes_enc" [@@noalloc]
  external dec      : buffer -> off -> buffer -> off -> key -> int -> size -> unit = "mc_aes_dec_bc" "mc_aes_dec" [@@noalloc]
  external derive_e : secret -> off -> key -> int -> unit = "mc_aes_derive_e_key" [@@noalloc]
  external derive_d : secret -> off -> key -> int -> key option -> unit = "mc_aes_derive_d_key" [@@noalloc]
  external rk_s     : int  -> int = "mc_aes_rk_size" [@@noalloc]
  external mode     : unit -> int = "mc_aes_mode" [@@noalloc]
end

module DES = struct
  external ddes    : buffer -> off -> buffer -> off -> int -> unit = "mc_des_ddes" [@@noalloc]
  external des3key : secret -> off -> int -> unit = "mc_des_des3key" [@@noalloc]
  external cp3key  : key -> unit = "mc_des_cp3key" [@@noalloc]
  external use3key : key -> unit = "mc_des_use3key" [@@noalloc]
  external k_s     : unit -> int = "mc_des_key_size" [@@noalloc]
end

module Chacha = struct
  external round : int -> bytes -> bytes -> off -> unit = "mc_chacha_round" [@@noalloc]
end

module Poly1305 = struct
  external init     : ctx -> string -> unit = "mc_poly1305_init" [@@noalloc]
  external update   : ctx -> string -> size -> unit = "mc_poly1305_update" [@@noalloc]
  external finalize : ctx -> bytes -> unit = "mc_poly1305_finalize" [@@noalloc]
  external ctx_size : unit -> int = "mc_poly1305_ctx_size" [@@noalloc]
  external mac_size : unit -> int = "mc_poly1305_mac_size" [@@noalloc]
end

module GHASH = struct
  external keysize : unit -> int = "mc_ghash_key_size" [@@noalloc]
  external keyinit : buffer -> off -> bytes -> unit = "mc_ghash_init_key" [@@noalloc]
  external ghash : bytes -> bytes -> buffer -> off -> size -> unit = "mc_ghash" [@@noalloc]
  external mode : unit -> int = "mc_ghash_mode" [@@noalloc]
end

(* XXX TODO
 * Unsolved: bounds-checked XORs are slowing things down considerably... *)
external xor_into : buffer -> off -> buffer -> off -> size -> unit = "mc_xor_into" [@@noalloc]

external xor_into_bytes : string -> off -> bytes -> off -> size -> unit = "mc_xor_into_bytes" [@@noalloc]

external count8be   : bytes -> buffer -> off -> blocks:size -> unit = "mc_count_8_be"    [@@noalloc]
external count16be  : bytes -> buffer -> off -> blocks:size -> unit = "mc_count_16_be"   [@@noalloc]
external count16be4 : bytes -> buffer -> off -> blocks:size -> unit = "mc_count_16_be_4" [@@noalloc]

external blit : buffer -> off -> buffer -> off -> size -> unit = "caml_blit_bigstring_to_bigstring" [@@noalloc]

external misc_mode : unit -> int = "mc_misc_mode" [@@noalloc]

external _detect_cpu_features : unit -> unit = "mc_detect_cpu_features" [@@noalloc]
external _detect_entropy : unit -> unit = "mc_entropy_detect"

let () =
  _detect_cpu_features ();
  _detect_entropy ()
