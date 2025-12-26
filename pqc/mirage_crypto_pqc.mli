(** Post-Quantum Cryptography

    This module provides post-quantum key encapsulation mechanisms.

    Currently implements ML-KEM-768 (NIST FIPS 203). *)

(** ML-KEM-768 Key Encapsulation Mechanism

    ML-KEM-768 provides IND-CCA2 security against both classical and
    quantum adversaries. It is based on the Module Learning with Errors
    (MLWE) problem.

    Parameter set ML-KEM-768 targets approximately NIST security level 3
    (equivalent to AES-192).

    Reference: NIST FIPS 203 *)
module ML_KEM_768 : sig
  (** {1 Key Sizes} *)

  val ek_len : int
  (** Encapsulation key length in bytes (1184) *)

  val dk_len : int
  (** Decapsulation key length in bytes (2400) *)

  val ct_len : int
  (** Ciphertext length in bytes (1088) *)

  val ss_len : int
  (** Shared secret length in bytes (32) *)

  (** {1 Key Generation} *)

  val generate : ?g:Mirage_crypto_rng.g -> unit -> string * string
  (** [generate ?g ()] generates a fresh ML-KEM-768 key pair.

      Returns [(ek, dk)] where [ek] is the encapsulation (public) key
      and [dk] is the decapsulation (secret) key.

      @param g Optional random number generator. Uses default if not provided. *)

  (** {1 Encapsulation} *)

  val encapsulate : ?g:Mirage_crypto_rng.g -> string -> (string * string, string) result
  (** [encapsulate ?g ek] encapsulates a fresh shared secret using the
      encapsulation key [ek].

      Returns [Ok (ct, ss)] where [ct] is the ciphertext and [ss] is
      the shared secret on success.

      Returns [Error msg] if [ek] has incorrect length.

      @param g Optional random number generator for fresh randomness. *)

  (** {1 Decapsulation} *)

  val decapsulate : string -> string -> (string, string) result
  (** [decapsulate dk ct] decapsulates the ciphertext [ct] using the
      decapsulation key [dk].

      Returns [Ok ss] where [ss] is the shared secret on success.

      Returns [Error msg] if [dk] or [ct] have incorrect lengths.

      Note: ML-KEM has implicit rejection - if decapsulation fails
      internally, a pseudorandom value is returned instead of an error,
      to prevent chosen-ciphertext attacks. *)
end
