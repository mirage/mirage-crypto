(*
 * Copyright (c) 2014, Hannes Mehnert
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 David Kaloper
 * Copyright (c) 2015 Citrix Systems Inc
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

let period = 600. (* refeed every <period> seconds *)
let chunk = 16    (* read <chunk> bytes of entropy every <period> *)

open Lwt

type 'a io  = 'a Lwt.t
type buffer = Cstruct.t
type error  = [ `No_entropy_device of string ]

type handler = source:int -> buffer -> unit

type id = [ `FromHost | `Weak ]

type implementation =
| RandomSelfInit (* Implementation of `Weak *)
| EntropyConsole of Console_xen.t (* Implementation of `FromHost *)

type t = {
  implementation: implementation;
  mutable ev: Lwt_engine.event option;
}

let id t = match t.implementation with
| RandomSelfInit -> `Weak
| EntropyConsole _ -> `FromHost

(* These are defined in xentropy/doc/protocol.md *)
let console_name = "org.openmirage.entropy.1"
let handshake_message =
  let string = "Hello, may I have some entropy?\r\n" in
  let buffer = Cstruct.create (String.length string) in
  Cstruct.blit_from_string string 0 buffer 0 (String.length string);
  buffer
let handshake_response = "You may treat everything following this message as entropy.\r\n"

let (>>|=) x f = x >>= function
| `Ok x -> f x
| `Eof ->
  print_endline "Received an EOF from the entropy console";
  return (`Error (`No_entropy_device console_name))
| `Error (`Invalid_console x) ->
  Printf.printf "Invalid_console %s\n%!" x;
  return (`Error (`No_entropy_device console_name))
| `Error _ ->
  Printf.printf "Unknown console device failure\n%!";
  return (`Error (`No_entropy_device console_name))

let connect = function
| `Weak ->
  Random.self_init ();
  print_endline "Entropy_xen: using a weak entropy source seeded only from time.";
  return (`Ok { implementation = RandomSelfInit; ev = None })
| `FromHost ->
  Printf.printf "Entropy_xen: attempting to connect to Xen entropy source %s\n%!" console_name;
  Console_xen.connect console_name
  >>|= fun device ->
  Console_xen.write device handshake_message
  >>|= fun () ->
  Console_xen.read device
  >>|= fun buffer ->
  let string = Cstruct.to_string buffer in
  if string <> handshake_response then begin
    Printf.printf "Entropy_xen: received [%s](%d bytes) instead of expected handshake message"
      (String.escaped string) (String.length string);
    return (`Error (`No_entropy_device console_name))
  end else begin
    print_endline "Entropy_xen: connected to Xen entropy source";
    return (`Ok { implementation = EntropyConsole device; ev = None })
  end

let disconnect _ = return_unit

let refeed t f = match t.implementation with
| EntropyConsole console ->
  ( Console_xen.read console
    >>= function
    | `Ok cs ->
      return cs
    | _ ->
      Printf.printf "ERROR: reading from entropy device. We have no more entropy\n%!";
      (* Abort the unikernel *)
      failwith "Unable to read from entropy device"
  ) >>= fun cs ->
  f ~source:(Cstruct.get_uint8 cs 0) (Cstruct.shift cs 1);
  return_unit
| RandomSelfInit ->
  let s  = Random.int 256
  and cs = Cstruct.create chunk in
  for i = 0 to chunk - 1 do
    Cstruct.set_uint8 cs i Random.(int 256)
  done ;
  f ~source:s cs;
  return_unit

let stop t =
  match t.ev with
  | None    -> ()
  | Some ev -> Lwt_engine.stop_event ev ; t.ev <- None

(*
 * Registering a `handler` spins up a recurrent timer.
 *
 * XXX There should be no timer in the first place; `refeed` should piggyback
 * on other activity going on in the system. We should trigger refeeding every
 * time the engine fires some _other_ callback, throttling the frequency. That
 * way, refeeding would be broadly aligned with the general activity of the
 * system instead of forcing the `period`.
 *)

let handler t f : unit Lwt.t =
  let trigger _ = async (fun () -> refeed t f) in
  stop t;
  refeed t f
  >>= fun () ->
  let ev = Lwt_engine.on_timer period true trigger in
  t.ev <- Some ev;
  return_unit

