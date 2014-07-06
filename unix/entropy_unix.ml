(*
 * Copyright (c) 2014, Hannes Mehnert
 * Copyright (c) 2014, David Kaloper
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
open Lwt_unix

module Make (T : V1_LWT.TIME) = struct

  type id = unit

  type 'a io  = 'a Lwt.t
  type buffer = Cstruct.t
  type error  = [ `No_entropy_device of string ]

  type handler = source:int -> buffer -> unit

  type t = {
    fd : file_descr ;
    mutable handler : handler option ;
  }

  let period = 600.
  and chunk  = 16
  and source = "/dev/urandom"

  let connect _ =
    try
      openfile source[ Unix.O_RDONLY ] 0 >|= fun fd ->
      `Ok { fd ; handler = None }
    with _ -> return (`Error (`No_entropy_device "failed to open /dev/urandom"))

  let disconnect t =
    t.handler <- None ; close t.fd

  let id _ = ()

  let refeed t =
    match t.handler with
    | None   -> return_unit
    | Some f ->
        let cs = Cstruct.create (chunk + 1) in
        Lwt_cstruct.(complete (read t.fd) cs) >|= fun _ ->
          f ~source:(Cstruct.get_uint8 cs 0) (Cstruct.sub cs 1 chunk)

  (*
   * Registering a `handler` spins up a read-loop.
   *
   * XXX The loop should be terminated on `disconnect`.
   *
   * XXX There should be no loop in the first place; `refeed` should piggyback
   * on other activity going on in the system.
   *)
  let handler t f =
    let rec loop t = refeed t >> T.sleep period >> loop t in
    t.handler <- Some f ;
    refeed t >|= fun _ -> async (fun () -> loop t)

end
