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

module Cpu_native = struct

  external cycles     : unit -> int  = "caml_cycle_counter" "noalloc"
  external random     : unit -> int  = "caml_cpu_random" "noalloc"
  external rng_type   : unit -> int  = "caml_cpu_rng_type" "noalloc"
  external detect     : unit -> unit = "caml_entropy_xen_detect"

  let () = detect ()

  let cpu_rng =
    match rng_type () with
    | 0 -> None
    | 1 -> Some `Rdrand
    | 2 -> Some `Rdseed
    | _ -> assert false
end

open Lwt

type 'a io   = 'a Lwt.t
type buffer  = Cstruct.t
type handler = source:int -> buffer -> unit

type t = {
  mutable handlers : handler list ;
  inits : ((Cstruct.t -> unit) -> unit Lwt.t) list
}

type source = [
    `Timer
  | `Rdseed
  | `Rdrand
  | `Xentropyd
]

let sources _ =
  `Timer ::
  match Cpu_native.cpu_rng with
  | Some x -> [x]
  | None   -> []

(* Note:
 * `bootstrap` is not a simple feedback loop. It attempts to exploit CPU-level
 * data races that lead to execution-time variability of identical instructions.
 * See Whirlwind RNG:
 *   http://www.ieee-security.org/TC/SP2014/papers/Not-So-RandomNumbersinVirtualizedLinuxandtheWhirlwindRNG.pdf
 *)
let bootstrap f =
  let outer     = 100
  and inner_max = 1024
  and a         = ref 0
  and cs        = Cstruct.create 2 in
  for i = 0 to outer - 1 do
    let tsc = Cpu_native.cycles () in
    let ()  = Cstruct.LE.set_uint16 cs 0 tsc ; f cs in
    for j = 1 to tsc mod inner_max do
      a := tsc / j - !a * i + 1
    done
  done ;
  return_unit

let interrupt_hook () =
  match Cpu_native.cpu_rng with
  | None ->
      let buf = Cstruct.create 4 in fun () ->
        let a = Cpu_native.cycles () in
        Cstruct.LE.set_uint32 buf 0 (Int32.of_int a) ;
        buf
  | Some _ ->
      let buf = Cstruct.create 12 in fun () ->
        let a = Cpu_native.cycles ()
        and b = Cpu_native.random () in
        Cstruct.LE.set_uint32 buf 0 (Int32.of_int a) ;
        Cstruct.LE.set_uint64 buf 4 (Int64.of_int b) ;
        buf

(* XXX TODO
 *
 * Xentropyd. Detect its presence here, make it feed into `t.handlers` as
 * `~source:1` and add a function providing initial entropy burst to
 * `t.inits`.
 *
 * Compile-time entropy. A function returning it could go into `t.inits`.
 *)
let connect () =
  let t    = { handlers = [] ; inits = [ bootstrap ] }
  and hook = interrupt_hook () in
  OS.Main.at_enter_iter (fun () ->
    match t.handlers with
    | [] -> ()
    | xs -> let e = hook () in List.iter (fun h -> h ~source:0 e) xs) ;
  return t

let add_handler t f =
  t.handlers <- f :: t.handlers ;
  Lwt_list.iteri_p (fun i boot -> boot (f ~source:i)) t.inits

let disconnect t =
  t.handlers <- [] ;
  return_unit


(*
 * Xentropyd code:
 *
 * - Needs to be able to detect if xentropyd is running and connect to it
 *   conditionally.
 * - Needs to be able to handle xentropyd disappearing and survive.
 *)


(*
let period = 600. (* refeed every <period> seconds *)
let chunk = 16    (* read <chunk> bytes of entropy every <period> *)

module Protocol = struct
  (* These are defined in xentropy/doc/protocol.md *)
  let console_name = "org.openmirage.entropy.1"
  let handshake_message =
    let string = "Hello, may I have some entropy?\r\n" in
    let buffer = Cstruct.create (String.length string) in
    Cstruct.blit_from_string string 0 buffer 0 (String.length string);
    buffer
  let handshake_response = "You may treat everything following this message as entropy.\r\n"
end

open Lwt

let (>>|=) x f = x >>= function
| `Ok x -> f x
| `Eof ->
  print_endline "Received an EOF from the entropy console";
  return (`Error (`No_entropy_device Protocol.console_name))
| `Error (`Invalid_console x) ->
  Printf.printf "Invalid_console %s\n%!" x;
  return (`Error (`No_entropy_device Protocol.console_name))
| `Error _ ->
  Printf.printf "Unknown console device failure\n%!";
  return (`Error (`No_entropy_device Protocol.console_name))

module BufferedConsole = struct
  type t = {
    device: Console_xen.t;
    mutable unconsumed: Cstruct.t;
  }
  (* Buffered reading on top of a console *)

  let create device =
    let unconsumed = Cstruct.create 0 in
    { device; unconsumed }

  (* read n bytes directly into [results] and return any extra
     received bytes which we'll buffer *)
  let rec read_n t results = match Cstruct.len results with
  | 0 -> return (`Ok (Cstruct.create 0))
  | n ->
    Console_xen.read t.device
    >>|= fun buffer ->
    let needed = min n (Cstruct.len buffer) in
    Cstruct.blit buffer 0 results 0 needed;
    if n = needed
    then return (`Ok (Cstruct.shift buffer needed))
    else read_n t (Cstruct.shift results needed)

  let read t n =
    let results = Cstruct.create n in
    (* first look in our unconsumed buffer *)
    let needed = min n (Cstruct.len t.unconsumed) in
    Cstruct.blit t.unconsumed 0 results 0 needed;
    t.unconsumed <- Cstruct.shift t.unconsumed needed;
    if n = needed
    then return (`Ok results)
    else
      (* that wasn't enough, so read another chunk *)
      read_n t (Cstruct.shift results needed)
      >>|= fun extra ->
      t.unconsumed <- extra;
      return (`Ok results)
end
*)

(* module Make(T : V1_LWT.TIME) = struct *)

(*
type id = [ `From_host | `Weak ]

type implementation =
| RandomSelfInit (* Implementation of `Weak *)
| EntropyConsole of BufferedConsole.t (* Implementation of `From_host *)

type t = {
  implementation: implementation;
  mutable ev: Lwt_engine.event option;
}

let id t = match t.implementation with
| RandomSelfInit -> `Weak
| EntropyConsole _ -> `From_host

let connect = function
| `Weak ->
  Random.self_init ();
  print_endline "Entropy_xen: using a weak entropy source seeded only from time.";
  return (`Ok {
    implementation = RandomSelfInit;
    ev = None;
  })
| `From_host ->
  Printf.printf "Entropy_xen: attempting to connect to Xen entropy source %s\n%!" Protocol.console_name;
  Console_xen.connect Protocol.console_name
  >>|= fun device ->
  Console_xen.write device Protocol.handshake_message
  >>|= fun () ->
  let bc = BufferedConsole.create device in
  BufferedConsole.read bc (String.length Protocol.handshake_response)
  >>|= fun response ->
  let response = Cstruct.to_string response in

  if response <> Protocol.handshake_response then begin
    Printf.printf "Entropy_xen: received [%s](%d bytes) instead of expected handshake message"
      (String.escaped response) (String.length response);
    return (`Error (`No_entropy_device Protocol.console_name))
  end else begin
    print_endline "Entropy_xen: connected to Xen entropy source";
    return (`Ok {
      implementation = EntropyConsole bc;
      ev = None;
    })
  end

let disconnect _ = return_unit

let refeed t f = match t.implementation with
| EntropyConsole console ->
  ( BufferedConsole.read console chunk
    >>= function
    | `Ok cs ->
      return cs
    | _ ->
      Printf.printf "ERROR: reading from entropy device. We have no more entropy\n%!";
      |+ Abort the unikernel +|
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

|+
 * Registering a `handler` spins up a recurrent timer.
 *
 * XXX There should be no timer in the first place; `refeed` should piggyback
 * on other activity going on in the system. We should trigger refeeding every
 * time the engine fires some _other_ callback, throttling the frequency. That
 * way, refeeding would be broadly aligned with the general activity of the
 * system instead of forcing the `period`.
 +|

let handler t f : unit Lwt.t =
  refeed t f
  >>= fun () ->
  let rec loop_forever () =
    T.sleep period
    >>= fun () ->
    refeed t f
    >>= fun () ->
    loop_forever () in
  let (_: [ `Never_returns] Lwt.t) = loop_forever () in
  return_unit *)

(* end *)
