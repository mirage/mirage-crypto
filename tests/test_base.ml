open OUnit2

open Mirage_crypto

open Test_common

(* Xor *)

let xor_cases =
  cases_of (f2_eq ~msg:"xor" Uncommon.Cs.xor) [
    "00 01 02 03 04 05 06 07 08 09 0a 0b 0c" ,
    "0c 0b 0a 09 08 07 06 05 04 03 02 01 00" ,
    "0c 0a 08 0a 0c 02 00 02 0c 0a 08 0a 0c" ;

    "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f" ,
    "0f 0e 0d 0c 0b 0a 09 08 07 06 05 04 03 02 01 00" ,
    "0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f" ;

    "00 01 02", "00", "00" ;

    "00", "00 01 02", "00" ;
  ]

(* B64 *)

let b64_enc_cases =
  cases_of (f1_eq ~msg:"b64" Base64.encode) [
    "66 6f 6f", "5a 6d 39 76" ;
    "66 6f 6f 6f", "5a 6d 39 76 62 77 3d 3d" ;
    "66 6f 6f 6f 6f", "5a 6d 39 76 62 32 38 3d" ;
    "66 6f 6f 6f 6f 6f", "5a 6d 39 76 62 32 39 76" ;
  ]

let b64_dec_cases =
  cases_of (f1_opt_eq ~msg:"b64" Base64.decode) [
    "00 5a 6d 39 76", None ;
    "5a 6d 39 76", Some "66 6f 6f" ;
    "5a 6d 39 76 76", None ;
    "5a 6d 39 76 76 76", None ;
    "5a 6d 39 76 76 76 76", None ;
    "5a 6d 39 76 00", None ;
    "5a 6d 39 76 62 77 3d 3d", Some "66 6f 6f 6f" ;
    "5a 6d 39 76 62 77 3d 3d 00", None ;
    "5a 6d 39 76 62 77 3d 3d 00 01", None ;
    "5a 6d 39 76 62 77 3d 3d 00 01 02", None ;
    "5a 6d 39 76 62 77 3d 3d 00 01 02 03", None ;
    "5a 6d 39 76 62 32 38 3d", Some "66 6f 6f 6f 6f" ;
    "5a 6d 39 76 62 32 39 76", Some "66 6f 6f 6f 6f 6f" ;
  ]

let suite = [
  "XOR" >::: [ "example" >::: xor_cases ];
  "B64" >::: [ "encoding" >::: b64_enc_cases ; "decoding" >::: b64_dec_cases ];
]
