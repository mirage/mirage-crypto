(*
 * Copyright (c) 2014 Hannes Mehnert
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014-2016 David Kaloper Meršinjak
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

module type S = sig
  type g = Mirage_crypto_rng.g
  (** A generator (PRNG) with its state. *)

  (** Entropy sources and collection *)
  module Entropy :
    sig
      (** Entropy sources. *)
      type source = Mirage_crypto_rng.Entropy.source

      val sources : unit -> source list
      (** [sources ()] returns the list of available sources. *)

      val pp_source : Format.formatter -> source -> unit
      (** [pp_source ppf source] pretty-prints the entropy [source] on [ppf]. *)

      val register_source : string -> source
      (** [register_source name] registers [name] as entropy source. *)
    end

  val generate_into : ?g:g -> bytes -> ?off:int -> int -> unit
  (** [generate_into ~g buf ~off len] invokes
      {{!Generator.generate_into}generate_into} on [g] or
      {{!generator}default generator}. The random data is put into [buf] starting
      at [off] (defaults to 0) with [len] bytes. *)

  val generate : ?g:g -> int -> string
  (** Invoke {!generate_into} on [g] or {{!generator}default generator} and a
      freshly allocated string. *)

  val accumulate : g option -> Entropy.source -> [`Acc of string -> unit]
  (** [accumulate g source] is a function [data -> unit] to feed entropy to the
      RNG. This is useful if your system has a special entropy source. *)
end

module Make (T : Mirage_time.S) (M : Mirage_clock.MCLOCK) : sig
  include S

  val initialize :
    ?g:'a -> ?sleep:int64 -> 'a Mirage_crypto_rng.generator -> unit Lwt.t
  (** [initialize ~g ~sleep generator] sets the default generator to the
      [generator] and sets up periodic entropy feeding for that rng. This
      function fails ([Lwt.fail]) if it is called a second time. The argument
      [~sleep] is measured in ns, and used as sleep between cpu assisted random
      number collection. It defaults to one second. *)
end
