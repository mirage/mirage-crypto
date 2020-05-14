(*
 * Copyright (c) 2014 Hannes Mehnert
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014-2016 David Kaloper Meršinjak
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

  external cycles : unit -> int  = "caml_cycle_counter" [@@noalloc]
  external rdseed : unit -> int  = "caml_cpu_rdseed" [@@noalloc]
  external rdrand : unit -> int  = "caml_cpu_rdrand" [@@noalloc]
  external rng_type : unit -> int  = "caml_cpu_rng_type" [@@noalloc]

  let cpu_rng =
    match rng_type () with
    | 0 -> []
    | 1 -> [ `Rdrand ]
    | 2 -> [ `Rdseed ]
    | 3 -> [ `Rdrand ; `Rdseed ]
    | _ -> assert false
end

type source = [
  | `Timer
  | `Rdseed
  | `Rdrand
  | `Getrandom
]

let pp_source ppf s =
  let str = match s with
    | `Timer -> "timer"
    | `Rdseed -> "rdseed"
    | `Rdrand -> "rdrand"
    | `Getrandom -> "getrandom"
  in
  Format.pp_print_string ppf str

let source_id = function
  | `Timer -> 0
  | `Rdrand -> 1
  | `Rdseed -> 2
  | `Getrandom -> 3

let _sources : source list ref = ref Cpu_native.cpu_rng

let add_source s = _sources := s :: !_sources

let sources () = !_sources

let cpu_rng = function
  | `Rdseed -> Cpu_native.rdseed
  | `Rdrand -> Cpu_native.rdrand

let random prefered =
  match Cpu_native.cpu_rng with
  | [] -> None
  | xs when List.mem prefered xs -> Some prefered
  | y::_ -> Some y

let write_header source data =
  Cstruct.set_uint8 data 0 source;
  Cstruct.set_uint8 data 1 (Cstruct.len data - 2)

let header source data =
  let hdr = Cstruct.create 2 in
  let buf = Cstruct.append hdr data in
  write_header (source_id source) buf;
  buf

(* Note:
 * `bootstrap` is not a simple feedback loop. It attempts to exploit CPU-level
 * data races that lead to execution-time variability of identical instructions.
 * See Whirlwind RNG:
 *   http://www.ieee-security.org/TC/SP2014/papers/Not-So-RandomNumbersinVirtualizedLinuxandtheWhirlwindRNG.pdf
*)
let whirlwind_bootstrap id =
  let outer     = 100
  and inner_max = 1024
  and a         = ref 0
  in
  let cs        = Cstruct.create (outer * 2 + 2) in
  for i = 0 to outer - 1 do
    let tsc = Cpu_native.cycles () in
    Cstruct.LE.set_uint16 cs ((i + 1) * 2) tsc;
    for j = 1 to tsc mod inner_max do
      a := tsc / j - !a * i + 1
    done
  done;
  write_header id cs;
  cs

let cpu_rng_bootstrap id =
  match random `Rdseed with
  | None -> failwith "expected a CPU rng"
  | Some insn ->
    let cs = Cstruct.create 10 in
    Cstruct.LE.set_uint64 cs 2 (Int64.of_int ((cpu_rng insn) ()));
    write_header id cs;
    cs

let bootstrap id =
  try cpu_rng_bootstrap id with Failure _ -> whirlwind_bootstrap id

let interrupt_hook () =
  let buf = Cstruct.create 4 in fun () ->
    let a = Cpu_native.cycles () in
    Cstruct.LE.set_uint32 buf 0 (Int32.of_int a) ;
    buf

let timer_accumulator g =
  let `Acc handle = Rng.accumulate (Some g) ~source:(source_id `Timer) in
  let hook = interrupt_hook () in
  add_source `Timer;
  (fun () -> handle (hook ()))

let feed_pools g source f =
  let `Acc handle = Rng.accumulate (Some g) ~source:(source_id source) in
  for _i = 0 to pred (Rng.pools (Some g)) do
    let cs = f () in
    handle cs
  done

let cpu_rng g =
  match random `Rdrand with
  | None -> ()
  | Some insn ->
    let randomf = cpu_rng insn in
    let f () =
      let cs = Cstruct.create 8 in
      Cstruct.LE.set_uint64 cs 0 (Int64.of_int (randomf ()));
      cs
    in
    feed_pools g insn f
