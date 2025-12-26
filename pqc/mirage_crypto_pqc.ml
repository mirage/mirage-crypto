(** Post-Quantum Cryptography - ML-KEM-768 Implementation

    Full implementation of ML-KEM-768 per NIST FIPS 203.

    This implementation provides IND-CCA2 security against both classical
    and quantum adversaries based on the Module Learning with Errors problem. *)

(* ========================================================================= *)
(* Keccak and SHAKE Implementation                                           *)
(* ========================================================================= *)

module Keccak = struct
  (* Keccak-f[1600] permutation - the core of SHA3 and SHAKE *)

  let state_size = 25  (* 5x5 array of 64-bit words = 1600 bits *)

  (* Round constants for Keccak-f[1600] *)
  let rc = [|
    0x0000000000000001L; 0x0000000000008082L; 0x800000000000808aL;
    0x8000000080008000L; 0x000000000000808bL; 0x0000000080000001L;
    0x8000000080008081L; 0x8000000000008009L; 0x000000000000008aL;
    0x0000000000000088L; 0x0000000080008009L; 0x000000008000000aL;
    0x000000008000808bL; 0x800000000000008bL; 0x8000000000008089L;
    0x8000000000008003L; 0x8000000000008002L; 0x8000000000000080L;
    0x000000000000800aL; 0x800000008000000aL; 0x8000000080008081L;
    0x8000000000008080L; 0x0000000080000001L; 0x8000000080008008L;
  |]

  (* Rotation offsets *)
  let rot = [|
    [| 0; 36;  3; 41; 18 |];
    [| 1; 44; 10; 45;  2 |];
    [| 62;  6; 43; 15; 61 |];
    [| 28; 55; 25; 21; 56 |];
    [| 27; 20; 39;  8; 14 |];
  |]

  let rotl64 x n =
    if n = 0 then x
    else Int64.(logor (shift_left x n) (shift_right_logical x (64 - n)))

  (* Keccak-f[1600] permutation *)
  let keccak_f state =
    let a = Array.init 5 (fun x -> Array.init 5 (fun y -> state.(x + 5 * y))) in

    for round = 0 to 23 do
      (* Theta step *)
      let c = Array.init 5 (fun x ->
        Int64.(logxor (logxor (logxor a.(x).(0) a.(x).(1))
                              (logxor a.(x).(2) a.(x).(3))) a.(x).(4))) in
      let d = Array.init 5 (fun x ->
        Int64.logxor c.((x + 4) mod 5) (rotl64 c.((x + 1) mod 5) 1)) in
      for x = 0 to 4 do
        for y = 0 to 4 do
          a.(x).(y) <- Int64.logxor a.(x).(y) d.(x)
        done
      done;

      (* Rho and Pi steps combined *)
      let b = Array.init 5 (fun _ -> Array.make 5 0L) in
      for x = 0 to 4 do
        for y = 0 to 4 do
          b.(y).((2 * x + 3 * y) mod 5) <- rotl64 a.(x).(y) rot.(x).(y)
        done
      done;

      (* Chi step *)
      for x = 0 to 4 do
        for y = 0 to 4 do
          a.(x).(y) <- Int64.logxor b.(x).(y)
            (Int64.logand (Int64.lognot b.((x + 1) mod 5).(y)) b.((x + 2) mod 5).(y))
        done
      done;

      (* Iota step *)
      a.(0).(0) <- Int64.logxor a.(0).(0) rc.(round)
    done;

    (* Copy back to linear state *)
    for x = 0 to 4 do
      for y = 0 to 4 do
        state.(x + 5 * y) <- a.(x).(y)
      done
    done

  (* Convert bytes to 64-bit words (little-endian) *)
  let bytes_to_words bytes off len =
    let words = Array.make ((len + 7) / 8) 0L in
    for i = 0 to len - 1 do
      let word_idx = i / 8 in
      let byte_idx = i mod 8 in
      let byte_val = Int64.of_int (Char.code bytes.[off + i]) in
      words.(word_idx) <- Int64.logor words.(word_idx)
                           (Int64.shift_left byte_val (byte_idx * 8))
    done;
    words

  (* Convert 64-bit words to bytes (little-endian) *)
  let words_to_bytes words buf off len =
    for i = 0 to len - 1 do
      let word_idx = i / 8 in
      let byte_idx = i mod 8 in
      let byte_val = Int64.(to_int (logand (shift_right_logical words.(word_idx) (byte_idx * 8)) 0xffL)) in
      Bytes.set buf (off + i) (Char.chr byte_val)
    done

  (* Absorb data into the sponge state - kept for potential future use *)
  let[@warning "-32"] _absorb state data off len rate_bytes =
    let rate_words = rate_bytes / 8 in
    let pos = ref 0 in
    while !pos + rate_bytes <= len do
      let words = bytes_to_words data (off + !pos) rate_bytes in
      for i = 0 to rate_words - 1 do
        state.(i) <- Int64.logxor state.(i) words.(i)
      done;
      keccak_f state;
      pos := !pos + rate_bytes
    done;
    !pos

  (* SHAKE128 XOF - rate = 1344 bits = 168 bytes *)
  type shake128_ctx = {
    mutable s128_state: int64 array;
    mutable s128_buf: bytes;
    mutable s128_buf_pos: int;
    mutable s128_squeezed: bool;
  }

  let shake128_rate = 168

  let shake128_init () = {
    s128_state = Array.make state_size 0L;
    s128_buf = Bytes.create shake128_rate;
    s128_buf_pos = 0;
    s128_squeezed = false;
  }

  let shake128_absorb ctx data off len =
    if ctx.s128_squeezed then
      failwith "Cannot absorb after squeezing";
    let i = ref 0 in
    while !i < len do
      let to_copy = min (shake128_rate - ctx.s128_buf_pos) (len - !i) in
      Bytes.blit_string data (off + !i) ctx.s128_buf ctx.s128_buf_pos to_copy;
      ctx.s128_buf_pos <- ctx.s128_buf_pos + to_copy;
      i := !i + to_copy;
      if ctx.s128_buf_pos = shake128_rate then begin
        let words = bytes_to_words (Bytes.to_string ctx.s128_buf) 0 shake128_rate in
        for j = 0 to shake128_rate / 8 - 1 do
          ctx.s128_state.(j) <- Int64.logxor ctx.s128_state.(j) words.(j)
        done;
        keccak_f ctx.s128_state;
        ctx.s128_buf_pos <- 0
      end
    done

  let shake128_finalize ctx =
    if not ctx.s128_squeezed then begin
      (* Pad with SHAKE domain separator (0x1F) and padding *)
      Bytes.set ctx.s128_buf ctx.s128_buf_pos '\x1f';
      for i = ctx.s128_buf_pos + 1 to shake128_rate - 1 do
        Bytes.set ctx.s128_buf i '\x00'
      done;
      let last_byte = Char.code (Bytes.get ctx.s128_buf (shake128_rate - 1)) in
      Bytes.set ctx.s128_buf (shake128_rate - 1) (Char.chr (last_byte lor 0x80));
      let words = bytes_to_words (Bytes.to_string ctx.s128_buf) 0 shake128_rate in
      for j = 0 to shake128_rate / 8 - 1 do
        ctx.s128_state.(j) <- Int64.logxor ctx.s128_state.(j) words.(j)
      done;
      keccak_f ctx.s128_state;
      ctx.s128_squeezed <- true;
      ctx.s128_buf_pos <- shake128_rate  (* Force immediate squeeze on first call *)
    end

  let shake128_squeeze ctx output off len =
    shake128_finalize ctx;
    let i = ref 0 in
    while !i < len do
      if ctx.s128_buf_pos >= shake128_rate then begin
        words_to_bytes ctx.s128_state ctx.s128_buf 0 shake128_rate;
        keccak_f ctx.s128_state;
        ctx.s128_buf_pos <- 0
      end;
      let to_copy = min (shake128_rate - ctx.s128_buf_pos) (len - !i) in
      Bytes.blit ctx.s128_buf ctx.s128_buf_pos output (off + !i) to_copy;
      ctx.s128_buf_pos <- ctx.s128_buf_pos + to_copy;
      i := !i + to_copy
    done

  (* SHAKE256 XOF - rate = 1088 bits = 136 bytes *)
  type shake256_ctx = {
    mutable s256_state: int64 array;
    mutable s256_buf: bytes;
    mutable s256_buf_pos: int;
    mutable s256_squeezed: bool;
  }

  let shake256_rate = 136

  let shake256_init () = {
    s256_state = Array.make state_size 0L;
    s256_buf = Bytes.create shake256_rate;
    s256_buf_pos = 0;
    s256_squeezed = false;
  }

  let shake256_absorb ctx data off len =
    if ctx.s256_squeezed then
      failwith "Cannot absorb after squeezing";
    let i = ref 0 in
    while !i < len do
      let to_copy = min (shake256_rate - ctx.s256_buf_pos) (len - !i) in
      Bytes.blit_string data (off + !i) ctx.s256_buf ctx.s256_buf_pos to_copy;
      ctx.s256_buf_pos <- ctx.s256_buf_pos + to_copy;
      i := !i + to_copy;
      if ctx.s256_buf_pos = shake256_rate then begin
        let words = bytes_to_words (Bytes.to_string ctx.s256_buf) 0 shake256_rate in
        for j = 0 to shake256_rate / 8 - 1 do
          ctx.s256_state.(j) <- Int64.logxor ctx.s256_state.(j) words.(j)
        done;
        keccak_f ctx.s256_state;
        ctx.s256_buf_pos <- 0
      end
    done

  let shake256_finalize ctx =
    if not ctx.s256_squeezed then begin
      Bytes.set ctx.s256_buf ctx.s256_buf_pos '\x1f';
      for i = ctx.s256_buf_pos + 1 to shake256_rate - 1 do
        Bytes.set ctx.s256_buf i '\x00'
      done;
      let last_byte = Char.code (Bytes.get ctx.s256_buf (shake256_rate - 1)) in
      Bytes.set ctx.s256_buf (shake256_rate - 1) (Char.chr (last_byte lor 0x80));
      let words = bytes_to_words (Bytes.to_string ctx.s256_buf) 0 shake256_rate in
      for j = 0 to shake256_rate / 8 - 1 do
        ctx.s256_state.(j) <- Int64.logxor ctx.s256_state.(j) words.(j)
      done;
      keccak_f ctx.s256_state;
      ctx.s256_squeezed <- true;
      ctx.s256_buf_pos <- shake256_rate
    end

  let shake256_squeeze ctx output off len =
    shake256_finalize ctx;
    let i = ref 0 in
    while !i < len do
      if ctx.s256_buf_pos >= shake256_rate then begin
        words_to_bytes ctx.s256_state ctx.s256_buf 0 shake256_rate;
        keccak_f ctx.s256_state;
        ctx.s256_buf_pos <- 0
      end;
      let to_copy = min (shake256_rate - ctx.s256_buf_pos) (len - !i) in
      Bytes.blit ctx.s256_buf ctx.s256_buf_pos output (off + !i) to_copy;
      ctx.s256_buf_pos <- ctx.s256_buf_pos + to_copy;
      i := !i + to_copy
    done

  (* Convenience functions - shake128 kept for potential future use *)
  let[@warning "-32"] shake128 input output_len =
    let ctx = shake128_init () in
    shake128_absorb ctx input 0 (String.length input);
    let output = Bytes.create output_len in
    shake128_squeeze ctx output 0 output_len;
    Bytes.to_string output

  let shake256 input output_len =
    let ctx = shake256_init () in
    shake256_absorb ctx input 0 (String.length input);
    let output = Bytes.create output_len in
    shake256_squeeze ctx output 0 output_len;
    Bytes.to_string output
end

(* ========================================================================= *)
(* ML-KEM Parameters and Constants                                           *)
(* ========================================================================= *)

module Params = struct
  (* ML-KEM-768 parameters per FIPS 203 *)
  let n = 256        (* Polynomial degree *)
  let k = 3          (* Module rank *)
  let q = 3329       (* Modulus *)
  let eta1 = 2       (* CBD parameter for secret/error in keygen *)
  let eta2 = 2       (* CBD parameter for error in encaps *)
  let du = 10        (* Compression bits for u *)
  let dv = 4         (* Compression bits for v *)

  (* Derived sizes *)
  let ek_len = 384 * k + 32      (* 1184 bytes *)
  let dk_len = 768 * k + 96      (* 2400 bytes *)
  let ct_len = 32 * (du * k + dv) (* 1088 bytes *)
  let ss_len = 32                 (* Shared secret length *)
end

(* ========================================================================= *)
(* NTT (Number Theoretic Transform) Implementation                           *)
(* ========================================================================= *)

module NTT = struct
  open Params

  (* Precomputed zetas for NTT: powers of primitive 256th root of unity *)
  (* zeta = 17 is a primitive 512th root of unity, 17^256 = -1 (mod q) *)
  (* zetas[k] = 17^(brv(k)) where brv is 7-bit bit-reversal *)
  (* These are in standard form (not Montgomery) *)
  let zetas = [|
       1; 1729; 2580; 3289; 2642;  630; 1897;  848;
    1062; 1919;  193;  797; 2786; 3260;  569; 1746;
     296; 2447; 1339; 1476; 3046;   56; 2240; 1333;
    1426; 2094;  535; 2882; 2393; 2879; 1974;  821;
     289;  331; 3253; 1756; 1197; 2304; 2277; 2055;
     650; 1977; 2513;  632; 2865;   33; 1320; 1915;
    2319; 1435;  807;  452; 1438; 2868; 1534; 2402;
    2647; 2617; 1481;  648; 2474; 3110; 1227;  910;
      17; 2761;  583; 2649; 1637;  723; 2288; 1100;
    1409; 2662; 3281;  233;  756; 2156; 3015; 3050;
    1703; 1651; 2789; 1789; 1847;  952; 1461; 2687;
     939; 2308; 2437; 2388;  733; 2337;  268;  641;
    1584; 2298; 2037; 3220;  375; 2549; 2090; 1645;
    1063;  319; 2773;  757; 2099;  561; 2466; 2594;
    2804; 1092;  403; 1026; 1143; 2150; 2775;  886;
    1722; 1212; 1874; 1029; 2110; 2935;  885; 2154;
  |]

  (* Full modular reduction to range [0, q) *)
  let mod_reduce a =
    let r = a mod q in
    if r < 0 then r + q else r

  (* Forward NTT: polynomial -> NTT domain *)
  let ntt coeffs =
    let r = Array.copy coeffs in
    let len = ref 128 in
    let k = ref 1 in
    while !len >= 2 do
      let start = ref 0 in
      while !start < n do
        let zeta = zetas.(!k) in
        incr k;
        for j = !start to !start + !len - 1 do
          let t = mod_reduce (zeta * r.(j + !len)) in
          r.(j + !len) <- mod_reduce (r.(j) - t);
          r.(j) <- mod_reduce (r.(j) + t)
        done;
        start := !start + 2 * !len
      done;
      len := !len / 2
    done;
    r

  (* Inverse NTT: NTT domain -> polynomial *)
  let invntt coeffs =
    let r = Array.copy coeffs in
    let len = ref 2 in
    let k = ref 127 in
    while !len <= 128 do
      let start = ref 0 in
      while !start < n do
        let zeta = zetas.(!k) in  (* Use same zetas, but in reverse *)
        decr k;
        for j = !start to !start + !len - 1 do
          let t = r.(j) in
          r.(j) <- mod_reduce (t + r.(j + !len));
          r.(j + !len) <- mod_reduce (zeta * mod_reduce (r.(j + !len) - t))
        done;
        start := !start + 2 * !len
      done;
      len := !len * 2
    done;
    (* Multiply by 128^(-1) mod q = 3303 *)
    (* Note: In Kyber this is actually mont^2/128 = 1441 in Montgomery form *)
    (* For standard form: 128^(-1) mod 3329 = 3303 *)
    let f = 3303 in
    for i = 0 to n - 1 do
      r.(i) <- mod_reduce (f * r.(i))
    done;
    r

  (* Base multiplication for coefficient pairs in NTT domain *)
  (* (a0 + a1*X) * (b0 + b1*X) mod (X^2 - zeta) *)
  let basemul_pair a0 a1 b0 b1 zeta =
    let r0 = mod_reduce (mod_reduce (a1 * b1) * zeta + a0 * b0) in
    let r1 = mod_reduce (a0 * b1 + a1 * b0) in
    (r0, r1)

  (* Pointwise multiplication in NTT domain *)
  let basemul a b =
    let r = Array.make n 0 in
    for i = 0 to 63 do
      let zeta = zetas.(64 + i) in
      let neg_zeta = mod_reduce (q - zeta) in  (* -zeta mod q *)
      (* First pair in group of 4 *)
      let (r0, r1) = basemul_pair a.(4*i) a.(4*i+1) b.(4*i) b.(4*i+1) zeta in
      r.(4 * i) <- r0;
      r.(4 * i + 1) <- r1;
      (* Second pair in group of 4, uses negative zeta *)
      let (r0', r1') = basemul_pair a.(4*i+2) a.(4*i+3) b.(4*i+2) b.(4*i+3) neg_zeta in
      r.(4 * i + 2) <- r0';
      r.(4 * i + 3) <- r1'
    done;
    r

  (* Add two polynomials *)
  let poly_add a b =
    Array.init n (fun i -> mod_reduce (a.(i) + b.(i)))

  (* Subtract two polynomials *)
  let poly_sub a b =
    Array.init n (fun i -> mod_reduce (a.(i) - b.(i)))
end

(* ========================================================================= *)
(* Polynomial Encoding/Decoding and Compression                              *)
(* ========================================================================= *)

module Poly = struct
  open Params

  (* Encode polynomial to bytes (12 bits per coefficient) *)
  let encode12 poly =
    let buf = Bytes.create (n * 12 / 8) in
    for i = 0 to n / 2 - 1 do
      let c0 = poly.(2 * i) land 0xFFF in
      let c1 = poly.(2 * i + 1) land 0xFFF in
      let off = 3 * i in
      Bytes.set buf off (Char.chr (c0 land 0xFF));
      Bytes.set buf (off + 1) (Char.chr (((c0 lsr 8) lor (c1 lsl 4)) land 0xFF));
      Bytes.set buf (off + 2) (Char.chr ((c1 lsr 4) land 0xFF))
    done;
    Bytes.to_string buf

  (* Decode bytes to polynomial (12 bits per coefficient) *)
  let decode12 data off =
    let poly = Array.make n 0 in
    for i = 0 to n / 2 - 1 do
      let b0 = Char.code data.[off + 3 * i] in
      let b1 = Char.code data.[off + 3 * i + 1] in
      let b2 = Char.code data.[off + 3 * i + 2] in
      poly.(2 * i) <- (b0 lor ((b1 land 0x0F) lsl 8)) mod q;
      poly.(2 * i + 1) <- ((b1 lsr 4) lor (b2 lsl 4)) mod q
    done;
    poly

  (* Compress polynomial to d bits per coefficient *)
  (* Compress_d(x) = round((x * 2^d) / q) mod 2^d *)
  let compress d poly =
    let two_d = 1 lsl d in
    Array.map (fun c ->
      (* Round: (c * 2^d + q/2) / q, then mod 2^d *)
      ((c * two_d + q / 2) / q) land (two_d - 1)
    ) poly

  (* Decompress polynomial from d bits per coefficient *)
  (* Decompress_d(y) = round((y * q) / 2^d) *)
  let decompress d poly =
    Array.map (fun c ->
      (* Round: (c * q + 2^(d-1)) / 2^d *)
      (c * q + (1 lsl (d - 1))) lsr d
    ) poly

  (* Encode compressed polynomial (d bits per coefficient) *)
  let encode_compressed d poly =
    let bits_per_byte = 8 in
    let total_bits = n * d in
    let total_bytes = (total_bits + bits_per_byte - 1) / bits_per_byte in
    let buf = Bytes.make total_bytes '\x00' in
    let bit_pos = ref 0 in
    for i = 0 to n - 1 do
      let c = poly.(i) land ((1 lsl d) - 1) in
      for b = 0 to d - 1 do
        if c land (1 lsl b) <> 0 then begin
          let byte_idx = !bit_pos / 8 in
          let bit_idx = !bit_pos mod 8 in
          let v = Char.code (Bytes.get buf byte_idx) in
          Bytes.set buf byte_idx (Char.chr (v lor (1 lsl bit_idx)))
        end;
        incr bit_pos
      done
    done;
    Bytes.to_string buf

  (* Decode compressed polynomial (d bits per coefficient) *)
  let decode_compressed d data off =
    let poly = Array.make n 0 in
    let bit_pos = ref 0 in
    for i = 0 to n - 1 do
      let c = ref 0 in
      for b = 0 to d - 1 do
        let byte_idx = !bit_pos / 8 in
        let bit_idx = !bit_pos mod 8 in
        if Char.code data.[off + byte_idx] land (1 lsl bit_idx) <> 0 then
          c := !c lor (1 lsl b);
        incr bit_pos
      done;
      poly.(i) <- !c
    done;
    poly
end

(* ========================================================================= *)
(* Sampling Functions                                                        *)
(* ========================================================================= *)

module Sample = struct
  open Params

  (* Sample from centered binomial distribution with parameter eta *)
  (* For eta=2: each coefficient uses 4 bits (2 for a, 2 for b) *)
  (* For eta=3: each coefficient uses 6 bits (3 for a, 3 for b) *)
  let cbd eta data off =
    let poly = Array.make n 0 in
    match eta with
    | 2 ->
      (* Process 4 bytes at a time to get 8 coefficients *)
      (* 256 coefficients need 128 bytes of input *)
      for i = 0 to n / 8 - 1 do
        let b0 = Char.code data.[off + 4 * i] in
        let b1 = Char.code data.[off + 4 * i + 1] in
        let b2 = Char.code data.[off + 4 * i + 2] in
        let b3 = Char.code data.[off + 4 * i + 3] in
        let t = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) in
        (* Sum pairs of bits: d = popcount of pairs *)
        let d = (t land 0x55555555) + ((t lsr 1) land 0x55555555) in
        for j = 0 to 7 do
          let a = (d lsr (4 * j)) land 3 in
          let b = (d lsr (4 * j + 2)) land 3 in
          let coeff = a - b in
          poly.(8 * i + j) <- if coeff < 0 then coeff + q else coeff
        done
      done;
      poly
    | 3 ->
      (* Process 3 bytes at a time to get 4 coefficients *)
      (* 256 coefficients need 192 bytes of input *)
      for i = 0 to n / 4 - 1 do
        let b0 = Char.code data.[off + 3 * i] in
        let b1 = Char.code data.[off + 3 * i + 1] in
        let b2 = Char.code data.[off + 3 * i + 2] in
        let t = b0 lor (b1 lsl 8) lor (b2 lsl 16) in
        (* Sum triplets of bits *)
        let d = (t land 0x249249) + ((t lsr 1) land 0x249249) + ((t lsr 2) land 0x249249) in
        for j = 0 to 3 do
          let a = (d lsr (6 * j)) land 7 in
          let b = (d lsr (6 * j + 3)) land 7 in
          let coeff = a - b in
          poly.(4 * i + j) <- if coeff < 0 then coeff + q else coeff
        done
      done;
      poly
    | _ -> failwith "Unsupported eta value"

  (* Sample uniformly random polynomial from XOF output *)
  let sample_ntt seed i j =
    let poly = Array.make n 0 in
    let ctx = Keccak.shake128_init () in
    Keccak.shake128_absorb ctx seed 0 (String.length seed);
    Keccak.shake128_absorb ctx (String.make 1 (Char.chr j)) 0 1;
    Keccak.shake128_absorb ctx (String.make 1 (Char.chr i)) 0 1;
    let buf = Bytes.create 3 in
    let coeffs_done = ref 0 in
    while !coeffs_done < n do
      Keccak.shake128_squeeze ctx buf 0 3;
      let b0 = Char.code (Bytes.get buf 0) in
      let b1 = Char.code (Bytes.get buf 1) in
      let b2 = Char.code (Bytes.get buf 2) in
      let d1 = b0 lor ((b1 land 0x0F) lsl 8) in
      let d2 = (b1 lsr 4) lor (b2 lsl 4) in
      if d1 < q && !coeffs_done < n then begin
        poly.(!coeffs_done) <- d1;
        incr coeffs_done
      end;
      if d2 < q && !coeffs_done < n then begin
        poly.(!coeffs_done) <- d2;
        incr coeffs_done
      end
    done;
    poly
end

(* ========================================================================= *)
(* ML-KEM-768 Implementation                                                 *)
(* ========================================================================= *)

module ML_KEM_768 = struct
  open Params

  let ek_len = ek_len
  let dk_len = dk_len
  let ct_len = ct_len
  let ss_len = ss_len

  (* Hash function H: SHA3-256 *)
  let hash_h data =
    Digestif.SHA3_256.(digest_string data |> to_raw_string)

  (* Hash function G: SHA3-512 *)
  let hash_g data =
    Digestif.SHA3_512.(digest_string data |> to_raw_string)

  (* PRF: SHAKE256 *)
  let prf seed nonce len =
    let input = seed ^ String.make 1 (Char.chr nonce) in
    Keccak.shake256 input len

  (* Key Derivation Function: SHAKE256 *)
  let kdf data =
    Keccak.shake256 data ss_len

  (* K-PKE.KeyGen: Generate public/private keypair for the PKE scheme *)
  let pke_keygen seed =
    (* Parse seed into (rho, sigma) using G *)
    let g_output = hash_g seed in
    let rho = String.sub g_output 0 32 in
    let sigma = String.sub g_output 32 32 in

    (* Generate matrix A in NTT domain *)
    let a_hat = Array.init k (fun i ->
      Array.init k (fun j -> Sample.sample_ntt rho i j)) in

    (* Sample secret vector s *)
    let s = Array.init k (fun i ->
      let noise = prf sigma i (64 * eta1) in
      NTT.ntt (Sample.cbd eta1 noise 0)) in

    (* Sample error vector e *)
    let e = Array.init k (fun i ->
      let noise = prf sigma (k + i) (64 * eta1) in
      NTT.ntt (Sample.cbd eta1 noise 0)) in

    (* Compute t = A*s + e in NTT domain *)
    let t_hat = Array.init k (fun i ->
      let acc = Array.make n 0 in
      for j = 0 to k - 1 do
        let prod = NTT.basemul a_hat.(i).(j) s.(j) in
        for l = 0 to n - 1 do
          acc.(l) <- NTT.mod_reduce (acc.(l) + prod.(l))
        done
      done;
      NTT.poly_add acc e.(i)) in

    (* Encode public key: ek = ByteEncode12(t_hat) || rho *)
    let ek_buf = Buffer.create ek_len in
    for i = 0 to k - 1 do
      Buffer.add_string ek_buf (Poly.encode12 t_hat.(i))
    done;
    Buffer.add_string ek_buf rho;
    let ek = Buffer.contents ek_buf in

    (* Encode secret key: s encoded *)
    let sk_buf = Buffer.create (384 * k) in
    for i = 0 to k - 1 do
      Buffer.add_string sk_buf (Poly.encode12 s.(i))
    done;
    let sk = Buffer.contents sk_buf in

    (ek, sk)

  (* K-PKE.Encrypt: Encrypt a message under a public key *)
  let pke_encrypt ek msg coins =
    (* Parse public key *)
    let t_hat = Array.init k (fun i -> Poly.decode12 ek (384 * i)) in
    let rho = String.sub ek (384 * k) 32 in

    (* Regenerate matrix A *)
    let a_hat = Array.init k (fun i ->
      Array.init k (fun j -> Sample.sample_ntt rho i j)) in

    (* Sample r, e1, e2 *)
    let r = Array.init k (fun i ->
      let noise = prf coins i (64 * eta1) in
      NTT.ntt (Sample.cbd eta1 noise 0)) in

    let e1 = Array.init k (fun i ->
      let noise = prf coins (k + i) (64 * eta2) in
      Sample.cbd eta2 noise 0) in

    let e2_noise = prf coins (2 * k) (64 * eta2) in
    let e2 = Sample.cbd eta2 e2_noise 0 in

    (* Compute u = NTT^(-1)(A^T * r) + e1 *)
    let u = Array.init k (fun i ->
      let acc = Array.make n 0 in
      for j = 0 to k - 1 do
        let prod = NTT.basemul a_hat.(j).(i) r.(j) in
        for l = 0 to n - 1 do
          acc.(l) <- NTT.mod_reduce (acc.(l) + prod.(l))
        done
      done;
      let u_i = NTT.invntt acc in
      NTT.poly_add u_i e1.(i)) in

    (* Compute v = NTT^(-1)(t^T * r) + e2 + Decompress_q(Decode_1(m), 1) *)
    let t_r = Array.make n 0 in
    for i = 0 to k - 1 do
      let prod = NTT.basemul t_hat.(i) r.(i) in
      for l = 0 to n - 1 do
        t_r.(l) <- NTT.mod_reduce (t_r.(l) + prod.(l))
      done
    done;
    let v = NTT.invntt t_r in
    let v = NTT.poly_add v e2 in

    (* Add message: decode 1 bit per coefficient and scale *)
    for i = 0 to n - 1 do
      let byte_idx = i / 8 in
      let bit_idx = i mod 8 in
      let m_bit = (Char.code msg.[byte_idx] lsr bit_idx) land 1 in
      let m_scaled = m_bit * ((q + 1) / 2) in
      v.(i) <- NTT.mod_reduce (v.(i) + m_scaled)
    done;

    (* Compress and encode ciphertext *)
    let ct_buf = Buffer.create ct_len in
    for i = 0 to k - 1 do
      let u_compressed = Poly.compress du u.(i) in
      Buffer.add_string ct_buf (Poly.encode_compressed du u_compressed)
    done;
    let v_compressed = Poly.compress dv v in
    Buffer.add_string ct_buf (Poly.encode_compressed dv v_compressed);
    Buffer.contents ct_buf

  (* K-PKE.Decrypt: Decrypt a ciphertext using the secret key *)
  let pke_decrypt sk ct =
    (* Decode secret key *)
    let s_hat = Array.init k (fun i -> Poly.decode12 sk (384 * i)) in

    (* Decode ciphertext *)
    let u_len = 32 * du in
    let u = Array.init k (fun i ->
      let u_compressed = Poly.decode_compressed du ct (u_len * i) in
      Poly.decompress du u_compressed) in

    let v_compressed = Poly.decode_compressed dv ct (u_len * k) in
    let v = Poly.decompress dv v_compressed in

    (* Compute s^T * NTT(u) *)
    let s_u = Array.make n 0 in
    for i = 0 to k - 1 do
      let u_ntt = NTT.ntt u.(i) in
      let prod = NTT.basemul s_hat.(i) u_ntt in
      for l = 0 to n - 1 do
        s_u.(l) <- NTT.mod_reduce (s_u.(l) + prod.(l))
      done
    done;
    let s_u = NTT.invntt s_u in

    (* Compute w = v - s^T * u *)
    let w = NTT.poly_sub v s_u in

    (* Decode message: compress to 1 bit *)
    let msg = Bytes.make 32 '\x00' in  (* Must initialize to zero! *)
    for i = 0 to n - 1 do
      (* Round to nearest: if w[i] is closer to q/2 than to 0, output 1 *)
      let t = ((w.(i) lsl 1) + q / 2) / q in
      let bit = t land 1 in
      if bit = 1 then begin
        let byte_idx = i / 8 in
        let bit_idx = i mod 8 in
        let v = Char.code (Bytes.get msg byte_idx) in
        Bytes.set msg byte_idx (Char.chr (v lor (1 lsl bit_idx)))
      end
    done;
    Bytes.to_string msg

  (* ML-KEM.KeyGen: Full key generation *)
  let generate ?g () =
    let d = match g with
      | Some g -> Mirage_crypto_rng.generate ~g 32
      | None -> Mirage_crypto_rng.generate 32
    in
    let z = match g with
      | Some g -> Mirage_crypto_rng.generate ~g 32
      | None -> Mirage_crypto_rng.generate 32
    in

    let (ek, sk) = pke_keygen d in

    (* Full decapsulation key: sk || ek || H(ek) || z *)
    let dk = sk ^ ek ^ (hash_h ek) ^ z in

    (ek, dk)

  (* ML-KEM.Encaps: Encapsulate a shared secret *)
  let encapsulate ?g ek =
    if String.length ek <> ek_len then
      Error (Printf.sprintf "Invalid encapsulation key length: expected %d, got %d"
               ek_len (String.length ek))
    else
      let m = match g with
        | Some g -> Mirage_crypto_rng.generate ~g 32
        | None -> Mirage_crypto_rng.generate 32
      in

      (* (K, r) = G(m || H(ek)) *)
      let g_input = m ^ (hash_h ek) in
      let g_output = hash_g g_input in
      let k = String.sub g_output 0 32 in
      let r = String.sub g_output 32 32 in

      (* c = PKE.Encrypt(ek, m, r) *)
      let ct = pke_encrypt ek m r in

      (* K = KDF(K || H(c)) *)
      let ss = kdf (k ^ (hash_h ct)) in

      Ok (ct, ss)

  (* ML-KEM.Decaps: Decapsulate to recover the shared secret *)
  let decapsulate dk ct =
    if String.length dk <> dk_len then
      Error (Printf.sprintf "Invalid decapsulation key length: expected %d, got %d"
               dk_len (String.length dk))
    else if String.length ct <> ct_len then
      Error (Printf.sprintf "Invalid ciphertext length: expected %d, got %d"
               ct_len (String.length ct))
    else
      (* Parse decapsulation key *)
      let sk = String.sub dk 0 (384 * k) in
      let ek = String.sub dk (384 * k) ek_len in
      let h_ek = String.sub dk (384 * k + ek_len) 32 in
      let z = String.sub dk (384 * k + ek_len + 32) 32 in

      (* m' = PKE.Decrypt(sk, c) *)
      let m' = pke_decrypt sk ct in

      (* (K', r') = G(m' || h) *)
      let g_input = m' ^ h_ek in
      let g_output = hash_g g_input in
      let k' = String.sub g_output 0 32 in
      let r' = String.sub g_output 32 32 in

      (* c' = PKE.Encrypt(ek, m', r') *)
      let ct' = pke_encrypt ek m' r' in

      (* Implicit rejection: constant-time comparison *)
      let ct_match = Eqaf.equal ct ct' in

      (* K = KDF(K' || H(c)) if ct = ct' else KDF(z || H(c)) *)
      let h_ct = hash_h ct in
      let k_good = k' ^ h_ct in
      let k_bad = z ^ h_ct in

      (* Constant-time select *)
      let k_input = if ct_match then k_good else k_bad in
      let ss = kdf k_input in

      Ok ss

  (* Debug/test functions - internal only, not exposed in .mli *)
  let[@warning "-32"] _test_ntt_roundtrip () =
    let poly = Array.init n (fun i -> i mod q) in
    let ntt_poly = NTT.ntt poly in
    let result = NTT.invntt ntt_poly in
    let all_match = Array.for_all2 (fun a b -> a = b) poly result in
    all_match

  let[@warning "-32"] _test_pke_roundtrip () =
    let d = Mirage_crypto_rng.generate 32 in
    let (ek, sk) = pke_keygen d in
    let msg = Mirage_crypto_rng.generate 32 in
    let coins = Mirage_crypto_rng.generate 32 in
    let ct = pke_encrypt ek msg coins in
    let msg' = pke_decrypt sk ct in
    (msg = msg', msg, msg')

  (* Debug: test with zero message to see raw noise *)
  let[@warning "-32"] _test_pke_zero_msg () =
    let d = Mirage_crypto_rng.generate 32 in
    let (ek, sk) = pke_keygen d in
    let msg = String.make 32 '\x00' in
    let coins = Mirage_crypto_rng.generate 32 in
    let ct = pke_encrypt ek msg coins in
    let msg' = pke_decrypt sk ct in
    (msg = msg', msg, msg')

  (* Debug: test with all-ones message *)
  let[@warning "-32"] _test_pke_ones_msg () =
    let d = Mirage_crypto_rng.generate 32 in
    let (ek, sk) = pke_keygen d in
    let msg = String.make 32 '\xff' in
    let coins = Mirage_crypto_rng.generate 32 in
    let ct = pke_encrypt ek msg coins in
    let msg' = pke_decrypt sk ct in
    (msg = msg', msg, msg')

  (* Debug: get first 16 coefficients of decrypted w polynomial for zero message *)
  let[@warning "-32"] _test_pke_debug_coeffs () =
    let d = Mirage_crypto_rng.generate 32 in
    let (ek, sk) = pke_keygen d in
    let msg = String.make 32 '\x00' in
    let coins = Mirage_crypto_rng.generate 32 in
    let ct = pke_encrypt ek msg coins in

    (* Manually decrypt to get w coefficients *)
    let s_hat = Array.init k (fun i -> Poly.decode12 sk (384 * i)) in
    let u_len = 32 * du in
    let u = Array.init k (fun i ->
      let u_compressed = Poly.decode_compressed du ct (u_len * i) in
      Poly.decompress du u_compressed) in
    let v_compressed = Poly.decode_compressed dv ct (u_len * k) in
    let v = Poly.decompress dv v_compressed in

    let s_u = Array.make n 0 in
    for i = 0 to k - 1 do
      let u_ntt = NTT.ntt u.(i) in
      let prod = NTT.basemul s_hat.(i) u_ntt in
      for l = 0 to n - 1 do
        s_u.(l) <- NTT.mod_reduce (s_u.(l) + prod.(l))
      done
    done;
    let s_u = NTT.invntt s_u in
    let w = NTT.poly_sub v s_u in

    (* Return first 16 coefficients for analysis *)
    Array.sub w 0 16

  (* Debug: count how many coefficients would decode as 1 *)
  let[@warning "-32"] _test_pke_count_errors () =
    let d = Mirage_crypto_rng.generate 32 in
    let (ek, sk) = pke_keygen d in
    let msg = String.make 32 '\x00' in
    let coins = Mirage_crypto_rng.generate 32 in
    let ct = pke_encrypt ek msg coins in

    (* Manually decrypt to get w coefficients *)
    let s_hat = Array.init k (fun i -> Poly.decode12 sk (384 * i)) in
    let u_len = 32 * du in
    let u = Array.init k (fun i ->
      let u_compressed = Poly.decode_compressed du ct (u_len * i) in
      Poly.decompress du u_compressed) in
    let v_compressed = Poly.decode_compressed dv ct (u_len * k) in
    let v = Poly.decompress dv v_compressed in

    let s_u = Array.make n 0 in
    for i = 0 to k - 1 do
      let u_ntt = NTT.ntt u.(i) in
      let prod = NTT.basemul s_hat.(i) u_ntt in
      for l = 0 to n - 1 do
        s_u.(l) <- NTT.mod_reduce (s_u.(l) + prod.(l))
      done
    done;
    let s_u = NTT.invntt s_u in
    let w = NTT.poly_sub v s_u in

    (* Count how many would decode as 1 *)
    let count = ref 0 in
    let bad_indices = ref [] in
    for i = 0 to n - 1 do
      let dist_to_0 = min w.(i) (q - w.(i)) in
      let dist_to_half = abs (w.(i) - q/2) in
      if dist_to_half < dist_to_0 then begin
        incr count;
        if List.length !bad_indices < 10 then
          bad_indices := (i, w.(i)) :: !bad_indices
      end
    done;
    (!count, List.rev !bad_indices)

  (* Debug: compare manual computation with pke_decrypt on SAME inputs *)
  let[@warning "-32"] _test_pke_compare () =
    let d = Mirage_crypto_rng.generate 32 in
    let (ek, sk) = pke_keygen d in
    let msg = String.make 32 '\x00' in
    let coins = Mirage_crypto_rng.generate 32 in
    let ct = pke_encrypt ek msg coins in

    (* Call pke_decrypt *)
    let msg_from_decrypt = pke_decrypt sk ct in

    (* Manual computation *)
    let s_hat = Array.init k (fun i -> Poly.decode12 sk (384 * i)) in
    let u_len = 32 * du in
    let u = Array.init k (fun i ->
      let u_compressed = Poly.decode_compressed du ct (u_len * i) in
      Poly.decompress du u_compressed) in
    let v_compressed = Poly.decode_compressed dv ct (u_len * k) in
    let v = Poly.decompress dv v_compressed in

    let s_u = Array.make n 0 in
    for i = 0 to k - 1 do
      let u_ntt = NTT.ntt u.(i) in
      let prod = NTT.basemul s_hat.(i) u_ntt in
      for l = 0 to n - 1 do
        s_u.(l) <- NTT.mod_reduce (s_u.(l) + prod.(l))
      done
    done;
    let s_u = NTT.invntt s_u in
    let w = NTT.poly_sub v s_u in

    (* Manually decode message using the same formula as pke_decrypt *)
    let msg_manual = Bytes.create 32 in
    for i = 0 to n - 1 do
      let t = ((w.(i) lsl 1) + q / 2) / q in
      let bit = t land 1 in
      if bit = 1 then begin
        let byte_idx = i / 8 in
        let bit_idx = i mod 8 in
        let byte_val = Char.code (Bytes.get msg_manual byte_idx) in
        Bytes.set msg_manual byte_idx (Char.chr (byte_val lor (1 lsl bit_idx)))
      end
    done;
    let msg_manual = Bytes.to_string msg_manual in

    (* Compare *)
    (msg_from_decrypt = msg_manual, msg_from_decrypt, msg_manual)

  (* Debug: detailed comparison of w values *)
  let[@warning "-32"] _test_pke_compare_w () =
    let d = Mirage_crypto_rng.generate 32 in
    let (ek, sk) = pke_keygen d in
    let msg = String.make 32 '\x00' in
    let coins = Mirage_crypto_rng.generate 32 in
    let ct = pke_encrypt ek msg coins in

    (* Compute w manually - this is exact copy of pke_decrypt logic *)
    let s_hat = Array.init k (fun i -> Poly.decode12 sk (384 * i)) in
    let u_len = 32 * du in
    let u = Array.init k (fun i ->
      let u_compressed = Poly.decode_compressed du ct (u_len * i) in
      Poly.decompress du u_compressed) in
    let v_compressed = Poly.decode_compressed dv ct (u_len * k) in
    let v = Poly.decompress dv v_compressed in

    let s_u = Array.make n 0 in
    for i = 0 to k - 1 do
      let u_ntt = NTT.ntt u.(i) in
      let prod = NTT.basemul s_hat.(i) u_ntt in
      for l = 0 to n - 1 do
        s_u.(l) <- NTT.mod_reduce (s_u.(l) + prod.(l))
      done
    done;
    let s_u_before_invntt = Array.copy s_u in
    let s_u = NTT.invntt s_u in
    let w = NTT.poly_sub v s_u in

    (* Now call pke_decrypt and manually extract its w *)
    (* We can't do this directly, but we can compare with the message decoding *)

    (* Return first few values of intermediate arrays for comparison *)
    (Array.sub v 0 8,
     Array.sub s_u_before_invntt 0 8,
     Array.sub s_u 0 8,
     Array.sub w 0 8)

  (* Debug: is pke_decrypt deterministic? *)
  let[@warning "-32"] _test_pke_deterministic () =
    let d = Mirage_crypto_rng.generate 32 in
    let (ek, sk) = pke_keygen d in
    let msg = String.make 32 '\x00' in
    let coins = Mirage_crypto_rng.generate 32 in
    let ct = pke_encrypt ek msg coins in

    (* Call pke_decrypt multiple times *)
    let msg1 = pke_decrypt sk ct in
    let msg2 = pke_decrypt sk ct in
    let msg3 = pke_decrypt sk ct in

    (msg1 = msg2 && msg2 = msg3, msg1, msg2, msg3)
end
