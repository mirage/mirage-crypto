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

let src = Logs.Src.create "mirage-crypto-rng-entropy" ~doc:"Mirage crypto RNG Entropy"
module Log = (val Logs.src_log src : Logs.LOG)

let rdrand_calls = Atomic.make 0
let rdrand_failures = Atomic.make 0
let rdseed_calls = Atomic.make 0
let rdseed_failures = Atomic.make 0

module Cpu_native = struct

  external cycles : unit -> int  = "mc_cycle_counter" [@@noalloc]
  external rdseed : bytes -> int -> bool  = "mc_cpu_rdseed" [@@noalloc]
  external rdrand : bytes -> int -> bool  = "mc_cpu_rdrand" [@@noalloc]
  external rng_type : unit -> int  = "mc_cpu_rng_type" [@@noalloc]

  let cpu_rng =
    match rng_type () with
    | 0 -> []
    | 1 -> [ `Rdrand ]
    | 2 -> [ `Rdseed ]
    | 3 -> [ `Rdrand ; `Rdseed ]
    | _ -> assert false
end

module S = Set.Make(struct
    type t = int * string
    (* only the name is relevant for comparison - the idx not *)
    let compare ((_a, an) : int * string) ((_b, bn) : int * string) =
      String.compare an bn
  end)

let _sources = Atomic.make S.empty

type source = Rng.source

let register_source name =
  let rec set () =
    let sources = Atomic.get _sources in
    let n = S.cardinal sources in
    let source = (n, name) in
    if Atomic.compare_and_set _sources sources (S.add source sources) then
      source
    else
      set ()
  in
  set ()

let id (idx, _) = idx

let sources () = S.elements (Atomic.get _sources)

let pp_source ppf (idx, name) = Format.fprintf ppf "[%d] %s" idx name

let cpu_rng isn buf off = match isn with
  | `Rdseed ->
    Atomic.incr rdseed_calls;
    let success = Cpu_native.rdseed buf off in
    if not success then Atomic.incr rdseed_failures;
    success
  | `Rdrand ->
    Atomic.incr rdrand_calls;
    let success = Cpu_native.rdrand buf off in
    if not success then Atomic.incr rdrand_failures;
    success

let random preferred =
  match Cpu_native.cpu_rng with
  | [] -> None
  | xs when List.mem preferred xs -> Some preferred
  | y::_ -> Some y

let write_header source data =
  Bytes.set_uint8 data 0 source;
  Bytes.set_uint8 data 1 (Bytes.length data - 2)

let header source data =
  let hdr = Bytes.create (2 + String.length data) in
  Bytes.unsafe_blit_string data 0 hdr 2 (String.length data);
  write_header source hdr;
  Bytes.unsafe_to_string hdr

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
  let buf       = Bytes.create (outer * 2 + 2) in
  for i = 0 to outer - 1 do
    let tsc = Cpu_native.cycles () in
    Bytes.set_uint16_le buf ((i + 1) * 2) tsc;
    for j = 1 to tsc mod inner_max do
      a := tsc / j - !a * i + 1
    done
  done;
  write_header id buf;
  Bytes.unsafe_to_string buf

let cpu_rng_bootstrap =
  let rdrand_bootstrap id =
    let rec go acc = function
      | 0 -> acc
      | n ->
        let buf = Bytes.create 10 in
        let r = cpu_rng `Rdrand buf 2 in
        write_header id buf;
        if not r then
          go acc (pred n)
        else
          go (Bytes.unsafe_to_string buf :: acc) (pred n)
    in
    let result = go [] 512 |> String.concat "" in
    if String.length result = 0 then
      failwith "Too many RDRAND failures"
    else
      result
  in
  match random `Rdseed with
  | None -> Error `Not_supported
  | Some `Rdseed ->
    let cpu_rng_bootstrap id =
      let buf = Bytes.create 10 in
      let r = cpu_rng `Rdseed buf 2 in
      write_header id buf;
      if not r then
        if List.mem `Rdrand Cpu_native.cpu_rng then
          rdrand_bootstrap id
        else
          failwith "RDSEED failed, and RDRAND not available"
      else
        Bytes.unsafe_to_string buf
    in
    Ok cpu_rng_bootstrap
  | Some `Rdrand -> Ok rdrand_bootstrap

let bootstrap id =
  match cpu_rng_bootstrap with
  | Error `Not_supported -> whirlwind_bootstrap id
  | Ok cpu_rng_bootstrap ->
    try cpu_rng_bootstrap id with
    | Failure f ->
      Log.err (fun m -> m "CPU RNG bootstrap failed: %s, using whirlwind" f);
      whirlwind_bootstrap id

let interrupt_hook () =
  let buf = Bytes.create 4 in
  let a = Cpu_native.cycles () in
  Bytes.set_int32_le buf 0 (Int32.of_int a) ;
  Bytes.unsafe_to_string buf

let timer_accumulator g =
  let g = match g with None -> Some (Rng.default_generator ()) | Some g -> Some g in
  let source = register_source "timer" in
  let `Acc handle = Rng.accumulate g source in
  (fun () -> handle (interrupt_hook ()))

let feed_pools g source f =
  let g = match g with None -> Some (Rng.default_generator ()) | Some g -> Some g in
  let `Acc handle = Rng.accumulate g source in
  for _i = 0 to pred (Rng.pools g) do
    match f () with
    | Ok data -> handle data
    | Error `No_random_available ->
      (* should we log a message? *)
      ()
  done

let cpu_rng =
  match random `Rdrand with
  | None -> Error `Not_supported
  | Some insn ->
    let cpu_rng g =
      let randomf = cpu_rng insn
      and source =
        let s = match insn with `Rdrand -> "rdrand" | `Rdseed -> "rdseed" in
        register_source s
      in
      let f () =
        let buf = Bytes.create 8 in
        if randomf buf 0 then
          Ok (Bytes.unsafe_to_string buf)
        else
          Error `No_random_available
      in
      fun () -> feed_pools g source f
    in
    Ok cpu_rng

let rdrand_calls () = Atomic.get rdrand_calls
let rdrand_failures () = Atomic.get rdrand_failures
let rdseed_calls () = Atomic.get rdseed_calls
let rdseed_failures () = Atomic.get rdseed_failures
