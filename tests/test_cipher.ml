open OUnit2

open Mirage_crypto

open Test_common

(* NIST SP 800-38A test vectors for block cipher modes of operation *)

let nist_sp_800_38a = vx
  "6b c1 be e2 2e 40 9f 96 e9 3d 7e 11 73 93 17 2a
   ae 2d 8a 57 1e 03 ac 9c 9e b7 6f ac 45 af 8e 51
   30 c8 1c 46 a3 5c e4 11 e5 fb c1 19 1a 0a 52 ef
   f6 9f 24 45 df 4f 9b 17 ad 2b 41 7b e6 6c 37 10"

let aes_ecb_cases =
  let open Cipher_block in

  let case ~key ~out = (AES.ECB.of_secret (vx key), vx out)

  and check (key, out) _ =
    let enc = AES.ECB.encrypt ~key nist_sp_800_38a in
    let dec = AES.ECB.decrypt ~key enc in
    assert_oct_equal ~msg:"ciphertext" out enc ;
    assert_oct_equal ~msg:"plaintext" nist_sp_800_38a dec in

  cases_of check [
    case ~key: "2b 7e 15 16 28 ae d2 a6 ab f7 15 88 09 cf 4f 3c"
         ~out: "3a d7 7b b4 0d 7a 36 60 a8 9e ca f3 24 66 ef 97
                f5 d3 d5 85 03 b9 69 9d e7 85 89 5a 96 fd ba af
                43 b1 cd 7f 59 8e ce 23 88 1b 00 e3 ed 03 06 88
                7b 0c 78 5e 27 e8 ad 3f 82 23 20 71 04 72 5d d4"

  ; case ~key: "8e 73 b0 f7 da 0e 64 52 c8 10 f3 2b 80 90 79 e5
                62 f8 ea d2 52 2c 6b 7b"
         ~out: "bd 33 4f 1d 6e 45 f2 5f f7 12 a2 14 57 1f a5 cc
                97 41 04 84 6d 0a d3 ad 77 34 ec b3 ec ee 4e ef
                ef 7a fd 22 70 e2 e6 0a dc e0 ba 2f ac e6 44 4e
                9a 4b 41 ba 73 8d 6c 72 fb 16 69 16 03 c1 8e 0e"

  ; case ~key: "60 3d eb 10 15 ca 71 be 2b 73 ae f0 85 7d 77 81
                1f 35 2c 07 3b 61 08 d7 2d 98 10 a3 09 14 df f4"
         ~out: "f3 ee d1 bd b5 d2 a0 3c 06 4b 5a 7e 3d b1 81 f8
                59 1c cb 10 d4 10 ed 26 dc 5b a7 4a 31 36 28 70
                b6 ed 21 b9 9c a6 f4 f9 f1 53 e7 b1 be af ed 1d
                23 30 4b 7a 39 f9 f3 ff 06 7d 8d 8f 9e 24 ec c7"
  ]

let aes_cbc_cases =
  let open Cipher_block in

  let case ~key ~iv ~out = (AES.CBC.of_secret (vx key), vx iv, vx out)

  and check (key, iv, out) _ =
    let enc = AES.CBC.encrypt ~key ~iv nist_sp_800_38a in
    let dec = AES.CBC.decrypt ~key ~iv enc in
    assert_oct_equal ~msg:"ciphertext" out enc ;
    assert_oct_equal ~msg:"plaintext" nist_sp_800_38a dec in

  cases_of check [
    case ~key: "2b 7e 15 16 28 ae d2 a6 ab f7 15 88 09 cf 4f 3c"
         ~iv:  "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f"
         ~out: "76 49 ab ac 81 19 b2 46 ce e9 8e 9b 12 e9 19 7d
                50 86 cb 9b 50 72 19 ee 95 db 11 3a 91 76 78 b2
                73 be d6 b8 e3 c1 74 3b 71 16 e6 9e 22 22 95 16
                3f f1 ca a1 68 1f ac 09 12 0e ca 30 75 86 e1 a7"

  ; case ~key: "8e 73 b0 f7 da 0e 64 52 c8 10 f3 2b 80 90 79 e5
                62 f8 ea d2 52 2c 6b 7b"
         ~iv:  "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f"
         ~out: "4f 02 1d b2 43 bc 63 3d 71 78 18 3a 9f a0 71 e8
                b4 d9 ad a9 ad 7d ed f4 e5 e7 38 76 3f 69 14 5a
                57 1b 24 20 12 fb 7a e0 7f a9 ba ac 3d f1 02 e0
                08 b0 e2 79 88 59 88 81 d9 20 a9 e6 4f 56 15 cd"

  ; case ~key: "60 3d eb 10 15 ca 71 be 2b 73 ae f0 85 7d 77 81
                1f 35 2c 07 3b 61 08 d7 2d 98 10 a3 09 14 df f4"
         ~iv:  "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f"
         ~out: "f5 8c 4c 04 d6 e5 f1 ba 77 9e ab fb 5f 7b fb d6
                9c fc 4e 96 7e db 80 8d 67 9f 77 7b c6 70 2c 7d
                39 f2 33 69 a9 d9 ba cf a5 30 e2 63 04 23 14 61
                b2 eb 05 e2 c3 9b e9 fc da 6c 19 07 8c 6a 9d 1b"
  ]

let aes_ctr_cases =
  let case ~key ~ctr ~out ~ctr1 = test_case @@ fun _ ->
    let open Cipher_block.AES.CTR in
    let key  = vx key |> of_secret
    and ctr  = vx ctr |> ctr_of_octets
    and ctr1 = vx ctr1 |> ctr_of_octets
    and out  = vx out in
    let enc = encrypt ~key ~ctr nist_sp_800_38a in
    let dec = decrypt ~key ~ctr enc in
    assert_oct_equal ~msg:"cipher" out enc;
    assert_oct_equal ~msg:"plain" nist_sp_800_38a dec;
    let blocks = String.length nist_sp_800_38a / block_size in
    assert_equal ~msg:"counters" ctr1 (add_ctr ctr (Int64.of_int blocks))
  in
  [ case ~key:  "2b7e1516 28aed2a6 abf71588 09cf4f3c"
         ~ctr:  "f0f1f2f3 f4f5f6f7 f8f9fafb fcfdfeff"
         ~out:  "874d6191 b620e326 1bef6864 990db6ce
                 9806f66b 7970fdff 8617187b b9fffdff
                 5ae4df3e dbd5d35e 5b4f0902 0db03eab
                 1e031dda 2fbe03d1 792170a0 f3009cee"
         ~ctr1: "f0f1f2f3 f4f5f6f7 f8f9fafb fcfdff03"

  ; case ~key:  "8e73b0f7 da0e6452 c810f32b 809079e5
                 62f8ead2 522c6b7b"
         ~ctr:  "f0f1f2f3 f4f5f6f7 f8f9fafb fcfdfeff"
         ~out:  "1abc9324 17521ca2 4f2b0459 fe7e6e0b
                 090339ec 0aa6faef d5ccc2c6 f4ce8e94
                 1e36b26b d1ebc670 d1bd1d66 5620abf7
                 4f78a7f6 d2980958 5a97daec 58c6b050"
         ~ctr1: "f0f1f2f3 f4f5f6f7 f8f9fafb fcfdff03"

  ; case ~key:  "603deb10 15ca71be 2b73aef0 857d7781
                 1f352c07 3b6108d7 2d9810a3 0914dff4"
         ~ctr:  "f0f1f2f3 f4f5f6f7 f8f9fafb fcfdfeff"
         ~out:  "601ec313 775789a5 b7a7f504 bbf3d228
                 f443e3ca 4d62b59a ca84e990 cacaf5c5
                 2b0930da a23de94c e87017ba 2d84988d
                 dfc9c58d b67aada6 13c2dd08 457941a6"
         ~ctr1: "f0f1f2f3 f4f5f6f7 f8f9fafb fcfdff03"

  ; case ~key:  "00010203 04050607 08090a0b 0c0d0e0f" (* ctr rollover *)
         ~ctr:  "00000000 00000000 ffffffff fffffffe"
         ~out:  "5d0a5645 378f579a 988ff186 d42eaa2f
                 978a655d 145bfe34 21656c8f 01101a43
                 23d0862c 47f7e3bf 95586ba4 2ab4cb31
                 790b0d01 93c0d022 3469534e 537ce82d"
         ~ctr1: "00000000 00000001 00000000 00000002"
  ]

(* aes gcm *)

let gcm_cases =
  let open Cipher_block in

  let case ~key ~p ~a ~nonce ~c ~t =
    (AES.GCM.of_secret (vx key), vx p, vx a, vx nonce, vx c, vx t) in

  let check (key, p, adata, nonce, c, t) _ =
    let cipher = AES.GCM.authenticate_encrypt ~key ~nonce ~adata p in
    let pdata =
      match AES.GCM.authenticate_decrypt ~key ~nonce ~adata cipher with
      | None -> assert_failure "GCM decryption broken"
      | Some data -> data
    in
    assert_oct_equal ~msg:"ciphertext" (c ^ t) cipher ;
    assert_oct_equal ~msg:"decrypted plaintext" p pdata
  in

  cases_of check [

    case ~key: "00000000000000000000000000000000"
         ~p:   ""
         ~a:   ""
         ~nonce: "000000000000000000000000"
         ~c:   ""
         ~t:   "58e2fccefa7e3061367f1d57a4e7455a" ;
    case ~key: "00000000000000000000000000000000"
         ~p:   "00000000000000000000000000000000"
         ~a:   ""
         ~nonce: "000000000000000000000000"
         ~c:   "0388dace60b6a392f328c2b971b2fe78"
         ~t:   "ab6e47d42cec13bdf53a67b21257bddf" ;
    case ~key: "feffe9928665731c6d6a8f9467308308"
         ~p:   "d9313225f88406e5a55909c5aff5269a
                86a7a9531534f7da2e4c303d8a318a72
                1c3c0c95956809532fcf0e2449a6b525
                b16aedf5aa0de657ba637b391aafd255"
         ~a:   ""
         ~nonce: "cafebabefacedbaddecaf888"
         ~c:   "42831ec2217774244b7221b784d0d49c
                e3aa212f2c02a4e035c17e2329aca12e
                21d514b25466931c7d8f6a5aac84aa05
                1ba30b396a0aac973d58e091473f5985"
         ~t:   "4d5c2af327cd64a62cf35abd2ba6fab4" ;
    case ~key: "feffe9928665731c6d6a8f9467308308"
         ~p:   "d9313225f88406e5a55909c5aff5269a
                86a7a9531534f7da2e4c303d8a318a72
                1c3c0c95956809532fcf0e2449a6b525
                b16aedf5aa0de657ba637b39"
         ~a:   "feedfacedeadbeeffeedfacedeadbeef
                abaddad2"
         ~nonce: "cafebabefacedbaddecaf888"
         ~c:   "42831ec2217774244b7221b784d0d49c
                e3aa212f2c02a4e035c17e2329aca12e
                21d514b25466931c7d8f6a5aac84aa05
                1ba30b396a0aac973d58e091"
         ~t:   "5bc94fbc3221a5db94fae95ae7121a47" ;
    case ~key: "feffe9928665731c6d6a8f9467308308"
         ~p:   "d9313225f88406e5a55909c5aff5269a
                86a7a9531534f7da2e4c303d8a318a72
                1c3c0c95956809532fcf0e2449a6b525
                b16aedf5aa0de657ba637b39"
         ~a:   "feedfacedeadbeeffeedfacedeadbeef
                abaddad2"
         ~nonce: "cafebabefacedbad"
         ~c:   "61353b4c2806934a777ff51fa22a4755
                699b2a714fcdc6f83766e5f97b6c7423
                73806900e49f24b22b097544d4896b42
                4989b5e1ebac0f07c23f4598"
         ~t:   "3612d2e79e3b0785561be14aaca2fccb" ;
    case ~key: "feffe9928665731c6d6a8f9467308308"
         ~p:   "d9313225f88406e5a55909c5aff5269a
                86a7a9531534f7da2e4c303d8a318a72
                1c3c0c95956809532fcf0e2449a6b525
                b16aedf5aa0de657ba637b39"
         ~a:   "feedfacedeadbeeffeedfacedeadbeef
                abaddad2"
         ~nonce: "9313225df88406e555909c5aff5269aa
                6a7a9538534f7da1e4c303d2a318a728
                c3c0c95156809539fcf0e2429a6b5254
                16aedbf5a0de6a57a637b39b"
         ~c:   "8ce24998625615b603a033aca13fb894
                be9112a5c3a211a8ba262a3cca7e2ca7
                01e4a9a4fba43c90ccdcb281d48c7c6f
                d62875d2aca417034c34aee5"
         ~t:   "619cc5aefffe0bfa462af43c1699d050" ;
    case ~key: "feffe9928665731c6d6a8f9467308308
                feffe9928665731c"
         ~p:   "d9313225f88406e5a55909c5aff5269a
                86a7a9531534f7da2e4c303d8a318a72
                1c3c0c95956809532fcf0e2449a6b525
                b16aedf5aa0de657ba637b39"
         ~a:   "feedfacedeadbeeffeedfacedeadbeef
                abaddad2"
         ~nonce: "cafebabefacedbaddecaf888"
         ~c:   "3980ca0b3c00e841eb06fac4872a2757
                859e1ceaa6efd984628593b40ca1e19c
                7d773d00c144c525ac619d18c84a3f47
                18e2448b2fe324d9ccda2710"
         ~t:   "2519498e80f1478f37ba55bd6d27618c" ;
    case ~key: "feffe9928665731c6d6a8f9467308308
                feffe9928665731c6d6a8f9467308308"
         ~p:   "d9313225f88406e5a55909c5aff5269a
                86a7a9531534f7da2e4c303d8a318a72
                1c3c0c95956809532fcf0e2449a6b525
                b16aedf5aa0de657ba637b39"
         ~a:   "feedfacedeadbeeffeedfacedeadbeef
                abaddad2"
         ~nonce: "9313225df88406e555909c5aff5269aa
                6a7a9538534f7da1e4c303d2a318a728
                c3c0c95156809539fcf0e2429a6b5254
                16aedbf5a0de6a57a637b39b"
         ~c:   "5a8def2f0c9e53f1f75d7853659e2a20
                eeb2b22aafde6419a058ab4f6f746bf4
                0fc0c3b780f244452da3ebf1c5d82cde
                a2418997200ef82e44ae7e3f"
         ~t:   "a44a8266ee1c8eb0c8b5d4cf5ae9f19a";
    case ~key: "00000000000000000000000000000000"  (* large GHASH batch *)
         ~p:   ""
         ~a:   "f0f0f0f0f0f0f0f00f0f0f0f0f0f0f0f
                e0e0e0e0e0e0e0e00e0e0e0e0e0e0e0e
                d0d0d0d0d0d0d0d00d0d0d0d0d0d0d0d
                c0c0c0c0c0c0c0c00c0c0c0c0c0c0c0c
                b0b0b0b0b0b0b0b00b0b0b0b0b0b0b0b
                a0a0a0a0a0a0a0a00a0a0a0a0a0a0a0a
                90909090909090900909090909090909
                80808080808080800808080808080808
                70707070707070700707070707070707
                60606060606060600606060606060606
                50505050505050500505050505050505
                40404040404040400404040404040404
                30303030303030300303030303030303
                20202020202020200202020202020202
                10101010101010100101010101010101
                00000000000000000000000000000000
                ff"
         ~nonce: "000000000000000000000000"
         ~c:   ""
         ~t:   "9bfdb8fdac1be65739780c41703c0fb6";
    case ~key: "00000000000000000000000000000002"  (* ctr rollover *)
         ~nonce: "3222415d"
         ~p:   "deadbeefdeadbeefdeadbeefdeadbeef
                deadbeefdeadbeefdeadbeefdeadbeef
                deadbeef"
         ~a:   ""
         ~c:   "42627ce3de61b5c105c7f01629c031c1
                b890bb273b6b6bc26b56c801f87fa95c
                a8b37503"
         ~t:   "3631cbe44782713b93b1c7d93c3c8638"
]


(*
(* from SP800-38C_updated-July20_2007.pdf appendix C *)
let ccm_cases =
  let open Cipher_block.AES.CCM in
  let case ~key ~p ~a ~nonce ~c ~maclen =
    (of_secret ~maclen (vx key), vx p, vx a, vx nonce, vx c) in

  let check (key, p, adata, nonce, c) _ =
    let cip = authenticate_encrypt ~key ~nonce ~adata p in
    assert_oct_equal ~msg:"encrypt" c cip ;
    match authenticate_decrypt ~key ~nonce ~adata c with
      | Some x -> assert_oct_equal ~msg:"decrypt" p x
      | None -> assert_failure "CCM decryption broken"
  in

  cases_of check [

    case ~key:    "404142434445464748494a4b4c4d4e4f"
         ~p:      "20212223"
         ~a:      "0001020304050607"
         ~nonce:  "10111213141516"
         ~c:      "7162015b4dac255d"
         ~maclen: 4
    ;
    case ~key:    "40414243 44454647 48494a4b 4c4d4e4f"
         ~p:      "20212223 24252627 28292a2b 2c2d2e2f"
         ~a:      "00010203 04050607 08090a0b 0c0d0e0f"
         ~nonce:  "10111213 14151617"
         ~c:      "d2a1f0e0 51ea5f62 081a7792 073d593d 1fc64fbf accd"
         ~maclen: 6
    ;
    case ~key:    "404142434445464748494a4b4c4d4e4f"
         ~p:      "202122232425262728292a2b2c2d2e2f3031323334353637"
         ~a:      "000102030405060708090a0b0c0d0e0f10111213"
         ~nonce:  "101112131415161718191a1b"
         ~c:      "e3b201a9f5b71a7a9b1ceaeccd97e70b6176aad9a4428aa5484392fbc1b09951"
         ~maclen: 8
  ]
*)

let ccm_regressions =
  let open Cipher_block.AES.CCM16 in
  let no_vs_empty_ad _ =
    (* as reported in https://github.com/mirleft/ocaml-nocrypto/issues/166 *)
    (* see RFC 3610 Section 2.1, AD of length 0 should be same as no AD *)
    let key = of_secret (vx "000102030405060708090a0b0c0d0e0f")
    and nonce = vx "0001020304050607"
    and plaintext = "hello"
    in
    assert_oct_equal ~msg:"CCM no vs empty ad"
      (authenticate_encrypt ~key ~nonce plaintext)
      (authenticate_encrypt ~adata:"" ~key ~nonce plaintext)
  and short_nonce_enc _ =
    (* as reported in https://github.com/mirleft/ocaml-nocrypto/issues/167 *)
    (* valid nonce sizes for CCM are 7..13 (L can be 2..8, nonce is 15 - L)*)
    (* see RFC3610 Section 2.1 *)
    let key = of_secret (vx "000102030405060708090a0b0c0d0e0f")
    and nonce = ""
    and plaintext = "hello"
    in
    assert_raises ~msg:"CCM with short nonce raises"
      (Invalid_argument "Mirage_crypto: CCM: nonce length not between 7 and 13: 0")
      (fun () -> authenticate_encrypt ~key ~nonce plaintext)
  and short_nonce_enc2 _ =
    let key = of_secret (vx "000102030405060708090a0b0c0d0e0f")
    and nonce = vx "00"
    and plaintext = "hello"
    in
    assert_raises ~msg:"CCM with short nonce raises"
      (Invalid_argument "Mirage_crypto: CCM: nonce length not between 7 and 13: 1")
      (fun () -> authenticate_encrypt ~key ~nonce plaintext)
  and short_nonce_enc3 _ =
    let key = of_secret (vx "000102030405060708090a0b0c0d0e0f")
    and nonce = vx "000102030405"
    and plaintext = "hello"
    in
    assert_raises ~msg:"CCM with short nonce raises"
      (Invalid_argument "Mirage_crypto: CCM: nonce length not between 7 and 13: 6")
      (fun () -> authenticate_encrypt ~key ~nonce plaintext)
  and long_nonce_enc _ =
    let key = of_secret (vx "000102030405060708090a0b0c0d0e0f")
    and nonce = vx "000102030405060708090a0b0c0d"
    and plaintext = "hello"
    in
    assert_raises ~msg:"CCM with short nonce raises"
      (Invalid_argument "Mirage_crypto: CCM: nonce length not between 7 and 13: 14")
      (fun () -> authenticate_encrypt ~key ~nonce plaintext)
  and enc_dec_empty_message _ =
    (* as reported in https://github.com/mirleft/ocaml-nocrypto/issues/168 *)
    let key = of_secret (vx "000102030405060708090a0b0c0d0e0f")
    and nonce = vx "0001020304050607"
    and adata = "hello"
    and p = ""
    in
    let cipher = authenticate_encrypt ~adata ~key ~nonce p in
    match authenticate_decrypt ~key ~nonce ~adata cipher with
    | Some x -> assert_oct_equal ~msg:"CCM decrypt of empty message" p x
    | None -> assert_failure "decryption broken"
  and long_adata _ =
    let key = of_secret (vx "000102030405060708090a0b0c0d0e0f")
    and nonce = vx "0001020304050607"
    and plaintext = "hello"
    (* [adata] is greater than [1 lsl 16 - 1 lsl 8] *)
    and adata = String.make 65280 '\x00'
    and expected = vx "6592169e946f98973bc06d080f7c9dbb493a536f8a"
    in
    let cipher = authenticate_encrypt ~adata ~key ~nonce plaintext in
    assert_oct_equal ~msg:"CCM encrypt of >=65280 adata" expected cipher
  in
  [
    test_case no_vs_empty_ad ;
    test_case short_nonce_enc ;
    test_case short_nonce_enc2 ;
    test_case short_nonce_enc3 ;
    test_case long_nonce_enc ;
    test_case enc_dec_empty_message ;
    test_case long_adata ;
  ]

let gcm_regressions =
  let open Cipher_block.AES.GCM in
  let msg = vx "000102030405060708090a0b0c0d0e0f" in
  let key = of_secret msg
  and nonce = ""
  in
  let nonce_zero_length_enc _ =
    (* reported in https://github.com/mirleft/ocaml-nocrypto/issues/169 *)
    assert_raises ~msg:"GCM with nonce of length 0"
      (Invalid_argument "Mirage_crypto: GCM: invalid nonce of length 0")
      (fun () -> authenticate_encrypt ~key ~nonce msg)
  and nonce_zero_length_dec _ =
    assert_raises ~msg:"GCM with nonce of 0"
      (Invalid_argument "Mirage_crypto: GCM: invalid nonce of length 0")
      (fun () -> authenticate_decrypt ~key ~nonce msg)
  in
  [
    test_case nonce_zero_length_enc ;
    test_case nonce_zero_length_dec ;
  ]


let chacha20_cases =
  let case msg ?ctr ~key ~nonce ?(input = String.make 128 '\000') output =
    let key = Chacha20.of_secret (vx key)
    and nonce = vx nonce
    and output = vx output
    in
    assert_oct_equal ~msg (Chacha20.crypt ~key ~nonce ?ctr input) output
  in
  let rfc8439_input = "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it." in
  let rfc8439_test_2_4_2 _ =
    let key = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
    and nonce = "000000000000004a00000000"
    and output =
{|6e 2e 35 9a 25 68 f9 80 41 ba 07 28 dd 0d 69 81
  e9 7e 7a ec 1d 43 60 c2 0a 27 af cc fd 9f ae 0b
  f9 1b 65 c5 52 47 33 ab 8f 59 3d ab cd 62 b3 57
  16 39 d6 24 e6 51 52 ab 8f 53 0c 35 9f 08 61 d8
  07 ca 0d bf 50 0d 6a 61 56 a3 8e 08 8a 22 b6 5e
  52 bc 51 4d 16 cc f8 06 81 8c e9 1a b7 79 37 36
  5a f9 0b bf 74 a3 5b e6 b4 0b 8e ed f2 78 5e 42
  87 4d|}
    in
    case "Chacha20 RFC 8439 2.4.2" ~ctr:1L ~key ~nonce ~input:rfc8439_input output
  and rfc8439_test_2_8_2 _ =
    let key = Chacha20.of_secret (vx "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f")
    and adata = vx "50515253c0c1c2c3c4c5c6c7"
    and nonce = vx "0700000040 41424344454647"
    and output = vx {|
  d3 1a 8d 34 64 8e 60 db 7b 86 af bc 53 ef 7e c2
  a4 ad ed 51 29 6e 08 fe a9 e2 b5 a7 36 ee 62 d6
  3d be a4 5e 8c a9 67 12 82 fa fb 69 da 92 72 8b
  1a 71 de 0a 9e 06 0b 29 05 d6 a5 b6 7e cd 3b 36
  92 dd bd 7f 2d 77 8b 8c 98 03 ae e3 28 09 1b 58
  fa b3 24 e4 fa d6 75 94 55 85 80 8b 48 31 d7 bc
  3f f4 de f0 8e 4b 7a 9d e5 76 d2 65 86 ce c6 4b
  61 16
  1a e1 0b 59 4f 09 e2 6a 7e 90 2e cb d0 60 06 91|}
    in
    assert_oct_equal ~msg:"Chacha20/Poly1305 RFC 8439 2.8.2 encrypt"
      (Chacha20.authenticate_encrypt ~key ~nonce ~adata rfc8439_input)
      output;
    assert_oct_equal ~msg:"Chacha20/Poly1305 RFC 8439 2.8.2 decrypt"
      (match Chacha20.authenticate_decrypt ~key ~nonce ~adata output with
       | Some cs -> cs | None -> assert_failure "Chacha20/poly1305 decryption broken")
      rfc8439_input;
  in
  (* from https://tools.ietf.org/html/draft-strombergson-chacha-test-vectors-01 *)
  let case ~key ~nonce ~output0 ~output1 _ =
    case "chacha20 crypt" ~key ~nonce (output0 ^ output1)
  in
  List.map test_case
    [
      rfc8439_test_2_4_2 ;

      rfc8439_test_2_8_2 ;

      case
        ~key:(String.make 64 '0')
        ~nonce:(String.make 16 '0')
        ~output0:("76b8e0ada0f13d90405d6ae55386bd28" ^
                  "bdd219b8a08ded1aa836efcc8b770dc7" ^
                  "da41597c5157488d7724e03fb8d84a37" ^
                  "6a43b8f41518a11cc387b669b2ee6586")
        ~output1:("9f07e7be5551387a98ba977c732d080d" ^
                  "cb0f29a048e3656912c6533e32ee7aed" ^
                  "29b721769ce64e43d57133b074d839d5" ^
                  "31ed1f28510afb45ace10a1f4b794d6f") ;

      case
        ~key:("01" ^ String.make 62 '0')
        ~nonce:(String.make 16 '0')
        ~output0:("c5d30a7ce1ec119378c84f487d775a85" ^
                  "42f13ece238a9455e8229e888de85bbd" ^
                  "29eb63d0a17a5b999b52da22be4023eb" ^
                  "07620a54f6fa6ad8737b71eb0464dac0")
        ~output1:("10f656e6d1fd55053e50c4875c9930a3" ^
                  "3f6d0263bd14dfd6ab8c70521c19338b" ^
                  "2308b95cf8d0bb7d202d2102780ea352" ^
                  "8f1cb48560f76b20f382b942500fceac") ;

      case
        ~key:(String.make 64 '0')
        ~nonce:("01" ^ String.make 14 '0')
        ~output0:("ef3fdfd6c61578fbf5cf35bd3dd33b80" ^
                  "09631634d21e42ac33960bd138e50d32" ^
                  "111e4caf237ee53ca8ad6426194a8854" ^
                  "5ddc497a0b466e7d6bbdb0041b2f586b")
        ~output1:("5305e5e44aff19b235936144675efbe4" ^
                  "409eb7e8e5f1430f5f5836aeb49bb532" ^
                  "8b017c4b9dc11f8a03863fa803dc71d5" ^
                  "726b2b6b31aa32708afe5af1d6b69058") ;

      case
        ~key:(String.make 64 'f')
        ~nonce:(String.make 16 'f')
        ~output0:("d9bf3f6bce6ed0b54254557767fb5744" ^
                  "3dd4778911b606055c39cc25e674b836" ^
                  "3feabc57fde54f790c52c8ae43240b79" ^
                  "d49042b777bfd6cb80e931270b7f50eb")
        ~output1:("5bac2acd86a836c5dc98c116c1217ec3" ^
                  "1d3a63a9451319f097f3b4d6dab07787" ^
                  "19477d24d24b403a12241d7cca064f79" ^
                  "0f1d51ccaff6b1667d4bbca1958c4306") ;

      case
        ~key:(String.make 64 '5')
        ~nonce:(String.make 16 '5')
        ~output0:("bea9411aa453c5434a5ae8c92862f564" ^
                  "396855a9ea6e22d6d3b50ae1b3663311" ^
                  "a4a3606c671d605ce16c3aece8e61ea1" ^
                  "45c59775017bee2fa6f88afc758069f7")
        ~output1:("e0b8f676e644216f4d2a3422d7fa36c6" ^
                  "c4931aca950e9da42788e6d0b6d1cd83" ^
                  "8ef652e97b145b14871eae6c6804c700" ^
                  "4db5ac2fce4c68c726d004b10fcaba86") ;

      case
        ~key:(String.make 64 'a')
        ~nonce:(String.make 16 'a')
        ~output0:("9aa2a9f656efde5aa7591c5fed4b35ae" ^
                  "a2895dec7cb4543b9e9f21f5e7bcbcf3" ^
                  "c43c748a970888f8248393a09d43e0b7" ^
                  "e164bc4d0b0fb240a2d72115c4808906")
        ~output1:("72184489440545d021d97ef6b693dfe5" ^
                  "b2c132d47e6f041c9063651f96b623e6" ^
                  "2a11999a23b6f7c461b2153026ad5e86" ^
                  "6a2e597ed07b8401dec63a0934c6b2a9") ;

      case
        ~key:"00112233445566778899aabbccddeeffffeeddccbbaa99887766554433221100"
        ~nonce:"0f1e2d3c4b5a6978"
        ~output0:("9fadf409c00811d00431d67efbd88fba" ^
                  "59218d5d6708b1d685863fabbb0e961e" ^
                  "ea480fd6fb532bfd494b215101505742" ^
                  "3ab60a63fe4f55f7a212e2167ccab931")
        ~output1:("fbfd29cf7bc1d279eddf25dd316bb884" ^
                  "3d6edee0bd1ef121d12fa17cbc2c574c" ^
                  "ccab5e275167b08bd686f8a09df87ec3" ^
                  "ffb35361b94ebfa13fec0e4889d18da5") ;

      case
        ~key:"c46ec1b18ce8a878725a37e780dfb7351f68ed2e194c79fbc6aebee1a667975d"
        ~nonce:"1ada31d5cf688221"
        ~output0:("f63a89b75c2271f9368816542ba52f06" ^
                  "ed49241792302b00b5e8f80ae9a473af" ^
                  "c25b218f519af0fdd406362e8d69de7f" ^
                  "54c604a6e00f353f110f771bdca8ab92")
        ~output1:("e5fbc34e60a1d9a9db17345b0a402736" ^
                  "853bf910b060bdf1f897b6290f01d138" ^
                  "ae2c4c90225ba9ea14d518f55929dea0" ^
                  "98ca7a6ccfe61227053c84e49a4a3332") ;

      case
        ~key:(String.make 32 '0')
        ~nonce:(String.make 16 '0')
        ~output0:("89670952608364fd00b2f90936f031c8" ^
                  "e756e15dba04b8493d00429259b20f46" ^
                  "cc04f111246b6c2ce066be3bfb32d9aa" ^
                  "0fddfbc12123d4b9e44f34dca05a103f")
        ~output1:("6cd135c2878c832b5896b134f6142a9d" ^
                  "4d8d0d8f1026d20a0a81512cbce6e975" ^
                  "8a7143d021978022a384141a80cea306" ^
                  "2f41f67a752e66ad3411984c787e30ad") ;

      case
        ~key:("01" ^ String.make 30 '0')
        ~nonce:(String.make 16 '0')
        ~output0:("ae56060d04f5b597897ff2af1388dbce" ^
                  "ff5a2a4920335dc17a3cb1b1b10fbe70" ^
                  "ece8f4864d8c7cdf0076453a8291c7db" ^
                  "eb3aa9c9d10e8ca36be4449376ed7c42")
        ~output1:("fc3d471c34a36fbbf616bc0a0e7c5230" ^
                  "30d944f43ec3e78dd6a12466547cb4f7" ^
                  "b3cebd0a5005e762e562d1375b7ac445" ^
                  "93a991b85d1a60fba2035dfaa2a642d5") ;

      case
        ~key:(String.make 32 '0')
        ~nonce:("01" ^ String.make 14 '0')
        ~output0:("1663879eb3f2c9949e2388caa343d361" ^
                  "bb132771245ae6d027ca9cb010dc1fa7" ^
                  "178dc41f8278bc1f64b3f12769a24097" ^
                  "f40d63a86366bdb36ac08abe60c07fe8")
        ~output1:("b057375c89144408cc744624f69f7f4c" ^
                  "cbd93366c92fc4dfcada65f1b959d8c6" ^
                  "4dfc50de711fb46416c2553cc60f21bb" ^
                  "fd006491cb17888b4fb3521c4fdd8745") ;

      case
        ~key:(String.make 32 'f')
        ~nonce:(String.make 16 'f')
        ~output0:("992947c3966126a0e660a3e95db048de" ^
                  "091fb9e0185b1e41e41015bb7ee50150" ^
                  "399e4760b262f9d53f26d8dd19e56f5c" ^
                  "506ae0c3619fa67fb0c408106d0203ee")
        ~output1:("40ea3cfa61fa32a2fda8d1238a2135d9" ^
                  "d4178775240f99007064a6a7f0c731b6" ^
                  "7c227c52ef796b6bed9f9059ba0614bc" ^
                  "f6dd6e38917f3b150e576375be50ed67") ;

      case
        ~key:(String.make 32 '5')
        ~nonce:(String.make 16 '5')
        ~output0:("357d7d94f966778f5815a2051dcb0413" ^
                  "3b26b0ead9f57dd09927837bc3067e4b" ^
                  "6bf299ad81f7f50c8da83c7810bfc17b" ^
                  "b6f4813ab6c326957045fd3fd5e19915")
        ~output1:("ec744a6b9bf8cbdcb36d8b6a5499c68a" ^
                  "08ef7be6cc1e93f2f5bcd2cad4e47c18" ^
                  "a3e5d94b5666382c6d130d822dd56aac" ^
                  "b0f8195278e7b292495f09868ddf12cc") ;

      case
        ~key:(String.make 32 'a')
        ~nonce:(String.make 16 'a')
        ~output0:("fc79acbd58526103862776aab20f3b7d" ^
                  "8d3149b2fab65766299316b6e5b16684" ^
                  "de5de548c1b7d083efd9e3052319e0c6" ^
                  "254141da04a6586df800f64d46b01c87")
        ~output1:("1f05bc67e07628ebe6f6865a2177e0b6" ^
                  "6a558aa7cc1e8ff1a98d27f7071f8335" ^
                  "efce4537bb0ef7b573b32f32765f2900" ^
                  "7da53bba62e7a44d006f41eb28fe15d6") ;

      case
        ~key:"00112233445566778899aabbccddeeff"
        ~nonce:"0f1e2d3c4b5a6978"
        ~output0:("d1abf630467eb4f67f1cfb47cd626aae" ^
                  "8afedbbe4ff8fc5fe9cfae307e74ed45" ^
                  "1f1404425ad2b54569d5f18148939971" ^
                  "abb8fafc88ce4ac7fe1c3d1f7a1eb7ca")
        ~output1:("e76ca87b61a9713541497760dd9ae059" ^
                  "350cad0dcedfaa80a883119a1a6f987f" ^
                  "d1ce91fd8ee0828034b411200a9745a2" ^
                  "85554475d12afc04887fef3516d12a2c") ;

      case
        ~key:"c46ec1b18ce8a878725a37e780dfb735"
        ~nonce:"1ada31d5cf688221"
        ~output0:("826abdd84460e2e9349f0ef4af5b179b" ^
                  "426e4b2d109a9c5bb44000ae51bea90a" ^
                  "496beeef62a76850ff3f0402c4ddc99f" ^
                  "6db07f151c1c0dfac2e56565d6289625")
        ~output1:("5b23132e7b469c7bfb88fa95d44ca5ae" ^
                  "3e45e848a4108e98bad7a9eb15512784" ^
                  "a6a9e6e591dce674120acaf9040ff50f" ^
                  "f3ac30ccfb5e14204f5e4268b90a8804")
    ]

let poly1305_rfc8439_2_5_2 _ =
  let key = vx "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"
  and data = "Cryptographic Forum Research Group"
  and output = vx "a8061dc1305136c6c22b8baf0c0127a9"
  in
  assert_oct_equal ~msg:"poly 1305 RFC8439 Section 2.5.2"
    (Poly1305.mac ~key data) output

let empty_cases _ =
  let open Cipher_block in
  let plain = ""
  and cipher = ""
  in
  (* 3DES ECB CBC CTR *)
  Array.iter (fun key_size ->
      let key = DES.ECB.of_secret (String.make key_size '\x00') in
      assert_oct_equal ~msg:"DES ECB encrypt" cipher (DES.ECB.encrypt ~key plain) ;
      assert_oct_equal ~msg:"DES ECB decrypt" plain (DES.ECB.decrypt ~key cipher))
    DES.ECB.key_sizes ;
  Array.iter (fun key_size ->
      let key = DES.CBC.of_secret (String.make key_size '\x00')
      and iv = String.make DES.CBC.block_size '\x00'
      in
      assert_oct_equal ~msg:"DES CBC encrypt" cipher (DES.CBC.encrypt ~key ~iv plain) ;
      assert_oct_equal ~msg:"DES CBC decrypt" plain (DES.CBC.decrypt ~key ~iv cipher))
    DES.CBC.key_sizes ;
  Array.iter (fun key_size ->
      let key = DES.CTR.of_secret (String.make key_size '\x00')
      and ctr = DES.CTR.ctr_of_octets (String.make DES.CTR.block_size '\x00')
      in
      assert_oct_equal ~msg:"DES CTR encrypt" cipher (DES.CTR.encrypt ~key ~ctr plain) ;
      assert_oct_equal ~msg:"DES CTR decrypt" plain (DES.CTR.decrypt ~key ~ctr cipher))
    DES.CTR.key_sizes ;

  (* AES ECB CBC CTR GCM CCM16 *)
  Array.iter (fun key_size ->
      let key = AES.ECB.of_secret (String.make key_size '\x00') in
      assert_oct_equal ~msg:"AES ECB encrypt" cipher (AES.ECB.encrypt ~key plain) ;
      assert_oct_equal ~msg:"AES ECB decrypt" plain (AES.ECB.decrypt ~key cipher))
    AES.ECB.key_sizes ;
  Array.iter (fun key_size ->
      let key = AES.CBC.of_secret (String.make key_size '\x00')
      and iv = String.make AES.CBC.block_size '\x00'
      in
      assert_oct_equal ~msg:"AES CBC encrypt" cipher (AES.CBC.encrypt ~key ~iv plain) ;
      assert_oct_equal ~msg:"AES CBC decrypt" plain (AES.CBC.decrypt ~key ~iv cipher))
    AES.CBC.key_sizes ;
  Array.iter (fun key_size ->
      let key = AES.CTR.of_secret (String.make key_size '\x00')
      and ctr = AES.CTR.ctr_of_octets (String.make AES.CTR.block_size '\x00')
      in
      assert_oct_equal ~msg:"AES CTR encrypt" cipher (AES.CTR.encrypt ~key ~ctr plain) ;
      assert_oct_equal ~msg:"AES CTR decrypt" plain (AES.CTR.decrypt ~key ~ctr cipher))
    AES.CTR.key_sizes ;
  Array.iter (fun key_size ->
      let key = AES.CCM16.of_secret (String.make key_size '\x00') in
      let test_one nonce =
        let c, tag = AES.CCM16.authenticate_encrypt_tag ~key ~nonce plain in
        assert_oct_equal ~msg:"AES CCM16 encrypt" cipher c ;
        match AES.CCM16.authenticate_decrypt_tag ~key ~nonce ~tag cipher with
        | None -> assert false
        | Some p -> assert_oct_equal ~msg:"AES CCM16 decrypt" plain p
      in
      test_one (String.make 7 '\x00');
      test_one (String.make 8 '\x00');
      test_one (String.make 13 '\x00'))
    AES.CCM16.key_sizes ;

  (* ChaCha20 *)
  Array.iter (fun key_size ->
      let key = Chacha20.of_secret (String.make key_size '\x00') in
      let test_one nonce =
        let c, tag = Chacha20.authenticate_encrypt_tag ~key ~nonce plain in
        assert_oct_equal ~msg:"Chacha20 encrypt" cipher c ;
        match Chacha20.authenticate_decrypt_tag ~key ~nonce ~tag cipher with
        | None -> assert false
        | Some p -> assert_oct_equal ~msg:"Chacha20 decrypt" plain p
      in
      test_one (String.make 8 '\x00');
      if key_size = 32 then
        test_one (String.make 12 '\x00'))
    [| 16 ; 32 |] ;

  (* ARC4 *)
  let key = Cipher_stream.ARC4.of_secret (String.make 16 '\x00') in
  assert_oct_equal ~msg:"ARC4 encrypt" cipher (Cipher_stream.ARC4.(encrypt ~key plain).message) ;
  assert_oct_equal ~msg:"ARC4 decrypt" plain (Cipher_stream.ARC4.(decrypt ~key cipher).message)

let suite = [
  "AES-ECB" >::: [ "SP 300-38A" >::: aes_ecb_cases ] ;
  "AES-CBC" >::: [ "SP 300-38A" >::: aes_cbc_cases ] ;
  "AES-CTR" >::: [ "SP 300-38A" >::: aes_ctr_cases; ] ;
  "AES-GCM" >::: gcm_cases ;
  (* "AES-CCM" >::: ccm_cases ; *)
  "AES-CCM-REGRESSION" >::: ccm_regressions ;
  "AES-GCM-REGRESSION" >::: gcm_regressions ;
  "Chacha20" >::: chacha20_cases ;
  "poly1305" >:: poly1305_rfc8439_2_5_2 ;
  "empty" >:: empty_cases ;
]
