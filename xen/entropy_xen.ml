(*
 * Copyright (c) 2014, Hannes Mehnert
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 David Kaloper
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

open Lwt

type id = unit

type 'a io  = 'a Lwt.t
type buffer = Cstruct.t
type error  = [ `No_entropy_device of string ]

type handler = source:int -> buffer -> unit

type t = { mutable handler : handler option }

let connect () =
  Random.self_init ();
  print_endline "Entropy_xen_weak: using a weak entropy source seeded only from time.";
  return (`Ok { handler = None })

let disconnect _ = return_unit

let id _ = ()

let chunk = 16

let refeed t =
  match t.handler with
  | None   -> ()
  | Some f ->
      let s  = Random.int 256
      and cs = Cstruct.create chunk in
      for i = 0 to chunk - 1 do
        Cstruct.set_uint8 cs i Random.(int 256)
      done ;
      f ~source:s cs

let handler t f =
  t.handler <- Some f ; refeed t ; return_unit

