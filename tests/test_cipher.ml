open OUnit2

open Mirage_crypto

open Test_common

let des_ecb_cases =
  let case ~data ~key ~out = vx data, DES.ECB.of_secret (vx key), vx out

  and check (data, key, out) _ =
    let enc = DES.ECB.encrypt ~key data in
    let dec = DES.ECB.decrypt ~key enc in
    assert_oct_equal ~msg:"ciphertext" out enc ;
    assert_oct_equal ~msg:"plaintext" data dec in

  cases_of check [
    case
      ~data:"3f87 9123 0058 8d88  e784 d52a 5d0f 2038
             f523 6889 bbce ce1f  a7bf 7aa8 6fcc 8245
             0576 2144 8f11 94d7  07bc 1bba 9b92 5e45
             3190 c42b 758f 3d91  f68e ebbb ce62 b8e7"
      ~key: "3f47 f79c c120 7188  4700 217e fd88 bbe4 6f51 27fb 7340 81e5"
      ~out: "b43b 3ae3 d765 b299  06ea 7c35 ceeb 9e52
             946c 06e7 0d50 193e  5a22 1ff0 afe9 abe0
             3b82 ce7d c42a 465d  0f19 45f0 5382 7006
             b4cd 21f0 5b0f 6843  de2a 67b6 9fb4 6a8f"
]

let des_cbc_cases =
  let case ~data ~key ~iv ~out = vx data, DES.CBC.of_secret (vx key), vx iv, vx out

  and check (data, key, iv, out) _ =
    let enc = DES.CBC.encrypt ~key ~iv data in
    let dec = DES.CBC.decrypt ~key ~iv enc in
    assert_oct_equal ~msg:"ciphertext" out enc ;
    assert_oct_equal ~msg:"plaintext" data dec in

  cases_of check [
    case
      ~data:
"8f8c 1e0a c8fb 1614  3cec ed1c 28ac fd6f
ae6d 3686 5365 511d  6707 68d9 7928 0479
cacd 6808 1540 d5fc  2971 2a8a c2b1 17c2
f0e6 a329 e190 44ff  54e7 5eec 8296 6a58"
      ~iv:"b219 ef93 4c37 aadf"
      ~key:"7ecd 2240 a2ac a10a  e713 f467 7ea5 d327  e04c cfe0 5cb4 bb09"
~out:
"3110 3904 faa1 4ef4  e404 d3d0 f2ee ae58
5fe9 e6b7 9552 b4f4  3608 03ca 395a f6e9
2330 69d6 2c6f a52a  d083 faab 3306 b794
89f6 6671 e3dd 3368  0b13 f8d9 7136 9674"
  ]

let des_ctr_cases =
  let case ~data ~key ~ctr ~out = test_case @@ fun _ ->
    let open DES.CTR in
    let key  = vx key |> of_secret
    and ctr  = vx ctr |> ctr_of_octets
    and out  = vx out
    and data = vx data in
    let enc = encrypt ~key ~ctr data in
    let dec = decrypt ~key ~ctr enc in
    assert_oct_equal ~msg:"cipher" out enc;
    assert_oct_equal ~msg:"plain" data dec
  in
  [ case
      ~data:
"e9ee ce61 7b75 4c70  79f3 3e5b 036a 7d5b
4bee f693 0eb3 fa50  9fe3 61d8 713a a487
a692 21b0 8627 5e6f  d021 4030 7c58 507a
5fea ca64 d17d a493  7337 8c17 ae05 f3c4
c6dc 15cc 49c4 3ab0  dab3 9c9b e964 a3c8
5865 7bb8 6e4d 8507  3866 b805 02c2 4970
dbbd 3554 20b1 76b2  ee6c 98b3 f7ce 9035
1e5f 880e"
~key:"76b9 d4ff d52f 5024  6d24 a3e1 4ebd e605  b82c d81f 0c07 2da1"
~ctr:"6318 a132 cafd aac0"
~out:
"b8d8 aeec d583 009c  f042 ec4d 7ddf c5e5
386f 89e6 d975 02bc  7583 e113 4899 dabc
bd93 871b 774b e5ce  4e12 6778 f208 0c53
52cb a3ac 7567 cdb9  ae81 fc46 25d4 7f9d
6f3f fbec 4512 8845  3739 1014 2b39 d293
845a 8505 91a6 f644  5168 bf00 ca4d 4603
6e5f 418f c43f fabd  272e 1009 c69b 2a6b
7d2c edb2"

  ]


(* NIST SP 800-38A test vectors for block cipher modes of operation *)

let nist_sp_800_38a = vx
  "6b c1 be e2 2e 40 9f 96 e9 3d 7e 11 73 93 17 2a
   ae 2d 8a 57 1e 03 ac 9c 9e b7 6f ac 45 af 8e 51
   30 c8 1c 46 a3 5c e4 11 e5 fb c1 19 1a 0a 52 ef
   f6 9f 24 45 df 4f 9b 17 ad 2b 41 7b e6 6c 37 10"

let aes_ecb_cases =
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
    let open AES.CTR in
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
  let open AES.CCM16 in
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
  let regr_tls =
    let key = of_secret (vx "063a 96fd 15f9 82d5  5aad 5bf9 d098 7546") in
    (* discovered while moving ocaml-tls to string *)
    let nonce = vx "81cd 4758 1880 9de0  c655 7c31"
    and adata = vx "1703 0300 17"
    and data = vx "0800 0002 0000 16"
    and expected = vx "94ca 065a c948 c5d6  92fd 5fab c850 0611  a07c 4f6e 0710 90"
    in
    let a _ =
      let cipher = authenticate_encrypt ~adata ~key ~nonce data in
      assert_oct_equal ~msg:"TLS regression 0" expected cipher
    and b _ =
      match authenticate_decrypt ~key ~nonce ~adata expected with
      | None -> assert_failure "TLS regression 0, decrypt broken"
      | Some x -> assert_oct_equal ~msg:"TLS regression 0 decrypt" x data
    in
    let nonce = vx "81cd 4758 1880 9de0  c655 7c30"
    and adata = vx "1703 0302 85"
    and data = vx {|
0b00 0270 0000 026c  0002 6730 8202 6330
8201 cc02 0900 cb6c  4e84 4b58 a1d4 300d
|}
    and expected = vx {|
1e59 904e e6d5 c2ac  e538 78d7 e24f 6e46
6169 f8e2 d3dd 8b5d  788c 78ff ea9f e1d0
9885 ac1a c6d9 fb88  b66a 3a11 5ba5 6e7c
        |}
    in
    let c _ =
      let cipher = authenticate_encrypt ~adata ~key ~nonce data in
      assert_oct_equal ~msg:"TLS regression 1" expected cipher
    and d _ =
      match authenticate_decrypt ~key ~nonce ~adata expected with
      | None -> assert_failure "TLS regression 1, decrypt broken"
      | Some x -> assert_oct_equal ~msg:"TLS regression 1 decrypt" x data
    in
    let data = vx {|
0b00 0270 0000 026c  0002 6730 8202 6330
8201 cc02 0900 cb6c  4e84 4b58 a1d4 300d
8201 cc02 0900
|}
    and expected = vx {|
1e59 904e e6d5 c2ac  e538 78d7 e24f 6e46
6169 f8e2 d3dd 8b5d  788c 78ff ea9f e1d0
7c8d 9993 6bfd cf76  9799 473b 58f4 ed69
d7a4 df7a 2d6b
      |}
    in
    let e _ =
      let cipher = authenticate_encrypt ~adata ~key ~nonce data in
      assert_oct_equal ~msg:"TLS regression 2" expected cipher
    and f _ =
      match authenticate_decrypt ~key ~nonce ~adata expected with
      | None -> assert_failure "TLS regression 2, decrypt broken"
      | Some x -> assert_oct_equal ~msg:"TLS regression 2 decrypt" x data
    in
    let data = vx {|
0b00 0270 0000 026c  0002 6730 8202 6330
8201 cc02 0900 cb6c  4e84 4b58 a1d4 300d
0609 2a86 4886 f70d  0101 0505 0030 7631
0b30 0906 0355 0406  1302 4155 3113 3011
0603 5504 080c 0a53  6f6d 652d 5374 6174
6531 2130 1f06 0355  040a 0c18 496e 7465
726e 6574 2057 6964  6769 7473 2050 7479
204c 7464 3115 3013  0603 5504 030c 0c59
4f55 5220 4e41 4d45  2121 2131 1830 1606
092a 8648 86f7 0d01  0901 1609 6d65 4062
6172 2e64 6530 1e17  0d31 3430 3231 3732
3230 3834 355a 170d  3135 3032 3137 3232
3038 3435 5a30 7631  0b30 0906 0355 0406
1302 4155 3113 3011  0603 5504 080c 0a53
6f6d 652d 5374 6174  6531 2130 1f06 0355
040a 0c18 496e 7465  726e 6574 2057 6964
6769 7473 2050 7479  204c 7464 3115 3013
0603 5504 030c 0c59  4f55 5220 4e41 4d45
2121 2131 1830 1606  092a 8648 86f7 0d01
0901 1609 6d65 4062  6172 2e64 6530 819f
300d 0609 2a86 4886  f70d 0101 0105 0003
818d 0030 8189 0281  8100 b640 48de e6bc
2194 3da2 ab5e b6f8  d837 007f 417c 0fe3
3492 c3aa 2f55 3e4d  5e31 4346 89c2 6f2b
e68e 00d2 88b0 e3ab  f6fe 1188 45d9 4989
8512 f192 cbe4 9fd5  b083 1f01 cb2d 274d
b3a6 38f5 befb 3ce8  1ab6 b559 3934 4404
4fed d6ca 154f 76bf  bd52 5608 bb55 0a39
bbd2 ed12 e6d7 1f9f  84ba 21aa 5e21 8015
0267 1aab 049a f864  0da1 0203 0100 0130
0d06 092a 8648 86f7  0d01 0105 0500 0381
8100 8a38 669a 4896  9dc9 4729 6d44 2d7f
0320 82d2 db21 e537  4cdd 6ef6 e7cc 1da0
fde5 11ed 3c52 52f0  a673 dc68 9fdc 5fca
cc1b 85df e22b 7bef  2adb 56b5 3732 e981
1063 794d 6e23 9f8f  a267 215b a7a4 d3dc
e505 e799 ec5c 38cd  1c16 ee75 e0d5 a46b
8f4c 8e82 6505 6153  9a84 305d f19a 5a24
1be5 55f8 7083 4e09  4d41 cf9f 74b3 342e
8345 0000 16
        |}
    and expected = vx {|
1e59 904e e6d5 c2ac  e538 78d7 e24f 6e46
6169 f8e2 d3dd 8b5d  788c 78ff ea9f e1d0
f885 7f17 2a7b f163  d29e 0a8e 8636 418f
a9da 651b f2ba 36aa  a1a4 14d0 6a9a f991
0836 eb93 80b9 bbe2  1f20 98d9 be0b c16f
d58c c98d 4082 dadd  f575 57a4 43f7 af31
c1b7 1eeb 2590 a887  e31c 590a 7e56 798c
69aa 4576 fde6 63d2  1b62 d00d 98f6 4015
dae7 8454 b96a f7f9  774f f539 24bf efe6
4629 ee35 4c81 32d4  43df ffa9 17a2 6306
fd07 f9ab b462 2bcd  bb0a 3750 af1a 3525
66ad 6c67 b647 2ca7  d6b5 b13e ea34 d90d
5731 a599 e608 d037  bc77 40aa b305 84ad
8d78 43fc 7f55 70a2  fbbb 1b30 a14a 2f5f
b3c3 2584 1f9e 7f3f  3dfa 19e2 9539 a1be
ead8 e051 d847 915b  ed23 87ab 7082 7df4
71a0 e0a6 46db a780  1e7b fb98 dac4 0af1
c3eb 42d4 3a6c 3c71  f55a b377 e4de ff20
14d7 b47c 8743 f291  56f3 6d8c 45d1 7cb3
0321 e2cf 8ffd babf  a129 ea0d cc1b 7a0d
b1ec 448d 0e3b 4386  9cc2 2b5a 5569 2930
ea33 080e 9168 3696  b224 6238 34fc 3e25
7895 6af3 cd60 f3c8  6643 3d6f 5736 4e78
6aca 8b2d 1575 2d34  4533 79bd e27e 9c46
f9f4 be4a 2fe3 f377  3acf 7b6e e4f0 3eb0
ec85 95a6 ed04 2316  fe4e 2a54 25aa c40a
c464 4128 0e35 1003  9f5d abfa e8e9 dc73
f709 f29b f930 0bdc  d941 981b c5b3 8295
97a5 c7e9 481d ce99  c6b6 5dfb 672d 3fdb
38bb a6be d7f8 9863  345d c3a8 77f3 6b77
f309 5c3b b9df fa40  8d42 ff79 6724 23da
8f24 c9b0 e02d 4794  581f e185 32e6 94bb
5b6a 6d5c 3b80 4c83  a0d8 0b42 d575 4fc3
4353 a78d fdb5 003c  4f0b 437d 75fb 5886
a76a 35f5 892d a10b  ce33 3ce6 ffd9 f09c
7264 5b09 c50a 7013  344c 11a1 ab92 5728
43e1 bc8c 8c1b 3fad  4a02 25a9 cb96 5fd2
1962 4b0c b46b 9f8f  1225 b18c 2572 6297
c890 238f 22d6 2bb0  7678 568a 3c9b 75e5
b8fc 10f3 13c7 aa16  8165 a29c 67f1 46f4
6e44 8e84 f5
|}
    in
    let g _ =
      let cipher = authenticate_encrypt ~adata ~key ~nonce data in
      assert_oct_equal ~msg:"TLS regression 3" expected cipher
    and h _ =
      match authenticate_decrypt ~key ~nonce ~adata expected with
      | None -> assert_failure "TLS regression 3, decrypt broken"
      | Some x -> assert_oct_equal ~msg:"TLS regression 3 decrypt" x data
    in
    [ a ; b ; c ; d ; e ; f ; g ; h ]
  in
  [
    test_case no_vs_empty_ad ;
    test_case short_nonce_enc ;
    test_case short_nonce_enc2 ;
    test_case short_nonce_enc3 ;
    test_case long_nonce_enc ;
    test_case enc_dec_empty_message ;
    test_case long_adata ;
  ] @ List.map test_case regr_tls

let gcm_regressions =
  let open AES.GCM in
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
  let key = ARC4.of_secret (String.make 16 '\x00') in
  assert_oct_equal ~msg:"ARC4 encrypt" cipher (ARC4.(encrypt ~key plain).message) ;
  assert_oct_equal ~msg:"ARC4 decrypt" plain (ARC4.(decrypt ~key cipher).message)

let suite = [
  "3DES-ECB" >::: des_ecb_cases ;
  "3DES-CBC" >::: des_cbc_cases ;
  "3DES-CTR" >::: des_ctr_cases ;
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
