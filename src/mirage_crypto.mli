(** Simpler crypto

    Mirage-crypto is a cryptographic library.

    The overarching API principle is simply mapping inputs to outputs, wherever
    feasible.

    Similar algorithms in the same class (like {{!Hash}hashes} or
    {{!Cipher_block}block ciphers}) are presented as distinct modules sharing
    the same signature.

    The opam package mirage-crypto-rng provides a cryptographically secure
    pseudo-random number generator, the package mirage-crypto-pk provides
    public key cryptography.
*)

(**/**)

(** A treasure-trove of random utilities.

    This is largely an internal API used in related sub-libraries or tests. As
    such, it is prone to breakage. *)
module Uncommon : sig

  val (//) : int -> int -> int
  (** [x // y] is the ceiling division [ceil (x / y)].

      [x // y] is [0] for any non-positive [x].

      @raise Division_by_zero when [y < 1]. *)

  val imin : int -> int -> int
  val imax : int -> int -> int
  val iter2 : 'a -> 'a -> ('a -> unit) -> unit
  val iter3 : 'a -> 'a -> 'a -> ('a -> unit) -> unit

  val xor : string -> string -> string
  val xor_into : string -> ?src_off:int -> bytes -> ?dst_off:int -> int -> unit

  val invalid_arg : ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a
  val failwith : ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a
end

(**/**)

(** The poly1305 message authentication code *)
module Poly1305 : sig
  type mac = string

  type 'a iter = ('a -> unit) -> unit

  type t
  (** Represents a running mac computation, suitable for appending inputs. *)

  val mac_size : int
  (** [mac_size] is the size of the output. *)

  val empty : key:string -> t
  (** [empty] is the empty context with the given [key].

      @raise Invalid_argument if key is not 32 bytes. *)

  val feed : t -> string -> t
  (** [feed t msg] adds the information in [msg] to [t]. *)

  val feedi : t -> string iter -> t
  (** [feedi t iter] feeds iter into [t]. *)

  val get : t -> mac
  (** [get t] is the mac corresponding to [t]. *)

  val mac : key:string -> string -> mac
  (** [mac ~key msg] is the all-in-one mac computation:
      [get (feed (empty ~key) msg)]. *)

  val maci : key:string -> string iter -> mac
  (** [maci ~key iter] is the all-in-one mac computation:
      [get (feedi (empty ~key) iter)]. *)

  val macl : key:string -> string list -> mac
  (** [macl ~key datas] computes the [mac] of [datas]. *)
end

(** {1 Symmetric-key cryptography} *)

(** Authenticated encryption with associated data.

    This defines a uniform interface of symmetrics cryptographic algorithms
    which encrypt, and also protect the integrity of the data. Additional data,
    only used for integrity protection, not encrypted and not part of the
    ciphertext, can be passed in optionally. This prevents the same ciphertext
    being used at a different location. See
    {{:https://tools.ietf.org/html/rfc5116}RFC 5116} for further description.
*)
module type AEAD = sig

  val tag_size : int
  (** The size of the authentication tag. *)

  type key
  (** The abstract type for the key. *)

  val of_secret : string -> key
  (** [of_secret secret] constructs the encryption key corresponding to
      [secret].

      @raise Invalid_argument if the length of [secret] is not a valid key size.
  *)

  (** {1 Authenticated encryption and decryption with inline tag} *)

  val authenticate_encrypt : key:key -> nonce:string -> ?adata:string ->
    string -> string
  (** [authenticate_encrypt ~key ~nonce ~adata msg] encrypts [msg] with [key]
      and [nonce], and appends an authentication tag computed over the encrypted
      [msg], using [key], [nonce], and [adata].

      @raise Invalid_argument if [nonce] is not of the right size. *)

  val authenticate_decrypt : key:key -> nonce:string -> ?adata:string ->
    string -> string option
  (** [authenticate_decrypt ~key ~nonce ~adata msg] splits [msg] into encrypted
      data and authentication tag, computes the authentication tag using [key],
      [nonce], and [adata], and decrypts the encrypted data. If the
      authentication tags match, the decrypted data is returned.

      @raise Invalid_argument if [nonce] is not of the right size. *)

  (** {1 Authenticated encryption and decryption with tag provided separately} *)

  val authenticate_encrypt_tag : key:key -> nonce:string ->
    ?adata:string -> string -> string * string
  (** [authenticate_encrypt_tag ~key ~nonce ~adata msg] encrypts [msg] with [key]
      and [nonce]. The computed authentication tag is returned separately as
      second part of the tuple.

      @raise Invalid_argument if [nonce] is not of the right size. *)

  val authenticate_decrypt_tag : key:key -> nonce:string ->
    ?adata:string -> tag:string -> string -> string option
  (** [authenticate_decrypt ~key ~nonce ~adata ~tag msg] computes the
      authentication tag using [key], [nonce], and [adata], and decrypts the
      encrypted data. If the authentication tags match, the decrypted data is
      returned.

      @raise Invalid_argument if [nonce] is not of the right size. *)
end

(** Block ciphers.

    Each algorithm, and each mode of operation, is contained in its own separate
    module. *)
module Cipher_block : sig

  (** Module types for various block cipher modes of operation. *)
  module S : sig

    (** Raw block cipher in all its glory.

        Make absolutely sure to check the arguments. Behavior is unspecified on
        invalid inputs. *)
    (* module type Core = sig *)

    (*   type ekey *)
    (*   type dkey *)

    (*   val of_secret   : string -> ekey * dkey *)
    (*   val e_of_secret : string -> ekey *)
    (*   val d_of_secret : string -> dkey *)

    (*   val key   : int array *)
    (*   val block : int *)

    (*   val encrypt : key:ekey -> blocks:int -> Native.buffer -> int -> Native.buffer -> int -> unit *)
    (*   val decrypt : key:dkey -> blocks:int -> Native.buffer -> int -> Native.buffer -> int -> unit *)
    (* end *)

    (** Modes of operation: *)

    (** {e Electronic Codebook} "mode". *)
    module type ECB = sig

      type key
      val of_secret : string -> key

      val key_sizes  : int array
      val block_size : int
      val encrypt : key:key -> string -> string
      val decrypt : key:key -> string -> string
    end

    (** {e Cipher-block chaining} mode. *)
    module type CBC = sig

      type key

      val of_secret : string -> key
      (** Construct the encryption key corresponding to [secret].

          @raise Invalid_argument if the length of [secret] is not in
          {{!key_sizes}[key_sizes]}. *)

      val key_sizes : int array
      (** Key sizes allowed with this cipher. *)

      val block_size : int
      (** The size of a single block. *)

      val encrypt : key:key -> iv:string -> string -> string
      (** [encrypt ~key ~iv msg] is [msg] encrypted under [key], using [iv] as
          the CBC initialization vector.

          @raise Invalid_argument if [iv] is not [block_size], or [msg] is not
          [k * block_size] long. *)

      val decrypt : key:key -> iv:string -> string -> string
      (** [decrypt ~key ~iv msg] is the inverse of [encrypt].

          @raise Invalid_argument if [iv] is not [block_size], or [msg] is not
          [k * block_size] long. *)

      val next_iv : iv:string -> string -> string
      (** [next_iv ~iv ciphertext] is the first [iv] {e following} the
          encryption that used [iv] to produce [ciphertext].

          For protocols which perform inter-message chaining, this is the [iv]
          for the next message.

          It is either [iv], when [len ciphertext = 0], or the last block of
          [ciphertext]. Note that

{[encrypt ~iv msg1 || encrypt ~iv:(next_iv ~iv (encrypt ~iv msg1)) msg2
  == encrypt ~iv (msg1 || msg2)]}

          @raise Invalid_argument if the length of [iv] is not [block_size], or
          the length of [ciphertext] is not [k * block_size] for some [k]. *)
    end

    (** {e Counter} mode. *)
    module type CTR = sig

      type key

      val of_secret : string -> key
      (** Construct the encryption key corresponding to [secret].

          @raise Invalid_argument if the length of [secret] is not in
          {{!key_sizes}[key_sizes]}. *)

      val key_sizes : int array
      (** Key sizes allowed with this cipher. *)

      val block_size : int
      (** The size of a single block. *)

      type ctr

      val stream : key:key -> ctr:ctr -> int -> string
      (** [stream ~key ~ctr n] is the raw keystream.

          Keystream is the concatenation of successive encrypted counter states.
          If [E(x)] is the single block [x] encrypted under [key], then keystream
          is the first [n] bytes of
          [E(ctr) || E(add ctr 1) || E(add ctr 2) || ...].

          Note that

{[stream ~key ~ctr (k * block_size) || stream ~key ~ctr:(add ctr k) x
  == stream ~key ~ctr (k * block_size + x)]}

          In other words, it is possible to restart a keystream at [block_size]
          boundaries by manipulating the counter. *)

      val encrypt : key:key -> ctr:ctr -> string -> string
      (** [encrypt ~key ~ctr msg] is
          [stream ~key ~ctr ~off (len msg) lxor msg]. *)

      val decrypt : key:key -> ctr:ctr -> string -> string
      (** [decrypt] is [encrypt]. *)

      val add_ctr : ctr -> int64 -> ctr
      (** [add_ctr ctr n] adds [n] to [ctr]. *)

      val next_ctr : ctr:ctr -> string -> ctr
      (** [next_ctr ~ctr msg] is the state of the counter after encrypting or
          decrypting [msg] with the counter [ctr].

          For protocols which perform inter-message chaining, this is the
          counter for the next message.

          It is computed as [C.add ctr (ceil (len msg / block_size))]. Note that
          if [len msg1 = k * block_size],

{[encrypt ~ctr msg1 || encrypt ~ctr:(next_ctr ~ctr msg1) msg2
  == encrypt ~ctr (msg1 || msg2)]}

          *)

      val ctr_of_octets : string -> ctr
      (** [ctr_of_octets buf] converts the value of [buf] into a counter. *)
    end

    (** {e Galois/Counter Mode}. *)
    module type GCM = sig

      include AEAD

      val key_sizes  : int array
      (** Key sizes allowed with this cipher. *)

      val block_size : int
      (** The size of a single block. *)
    end

    (** {e Counter with CBC-MAC} mode. *)
    module type CCM16 = sig

      include AEAD

      val key_sizes  : int array
      (** Key sizes allowed with this cipher. *)

      val block_size : int
      (** The size of a single block. *)
    end
  end

  module AES : sig
(*     module Core : S.Core *)
    module ECB  : S.ECB
    module CBC  : S.CBC
    module CTR  : S.CTR with type ctr = int64 * int64
    module GCM  : S.GCM
    module CCM16  : S.CCM16
  end

  module DES : sig
(*     module Core : S.Core *)
    module ECB  : S.ECB
    module CBC  : S.CBC
    module CTR  : S.CTR with type ctr = int64
  end

  val accelerated : [`XOR | `AES | `GHASH] list
  (** Operations using non-portable, hardware-dependent implementation in
      this build of the library. *)
end

(** The ChaCha20 cipher proposed by D.J. Bernstein. *)
module Chacha20 : sig
  include AEAD

  val crypt : key:key -> nonce:string -> ?ctr:int64 -> string -> string
  (** [crypt ~key ~nonce ~ctr data] generates a ChaCha20 key stream using
      the [key], and [nonce]. The [ctr] defaults to 0. The generated key
      stream is of the same length as [data], and the output is the XOR
      of the key stream and [data]. This implements, depending on the size
      of the [nonce] (8 or 12 bytes) both the original specification (where
      the counter is 8 byte, same as the nonce) and the IETF RFC 8439
      specification (where nonce is 12 bytes, and counter 4 bytes).

      @raise Invalid_argument if invalid parameters are provided. Valid
      parameters are: [key] must be 32 bytes and [nonce] 12 bytes for the
      IETF mode (and counter fit into 32 bits), or [key] must be either 16
      bytes or 32 bytes and [nonce] 8 bytes.
  *)
end

(** Streaming ciphers. *)
module Cipher_stream : sig

  (** General stream cipher type. *)
  module type S = sig
    type key
    type result = { message : string ; key : key }
    val of_secret : string -> key
    val encrypt : key:key -> string -> result
    val decrypt : key:key -> string -> result
  end

  (** {e Alleged Rivest Cipher 4}. *)
  module ARC4 : S
end
