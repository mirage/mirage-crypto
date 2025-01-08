open Mirage_crypto

module Time = struct

  let time ~n f a =
    let t1 = Sys.time () in
    for _ = 1 to n do ignore (f a) done ;
    let t2 = Sys.time () in
    (t2 -. t1)

  let warmup () =
    let x = ref 0 in
    let rec go start =
      if Sys.time () -. start < 1. then begin
        for i = 0 to 10000 do x := !x + i done ;
        go start
      end in
    go (Sys.time ())

end

let burn_period = 2.0

let sizes = [16; 64; 256; 1024; 8192]
(* let sizes = [16] *)

let burn f n =
  let buf = Mirage_crypto_rng.generate n in
  let (t1, i1) =
    let rec loop it =
      let t = Time.time ~n:it f buf in
      if t > 0.2 then (t, it) else loop (it * 10) in
    loop 10 in
  let iters = int_of_float (float i1 *. burn_period /. t1) in
  let time  = Time.time ~n:iters f buf in
  (iters, time, float (n * iters) /. time)

let mb = 1024. *. 1024.

let throughput title f =
  Printf.printf "\n* [%s]\n%!" title ;
  sizes |> List.iter @@ fun size ->
    Gc.full_major () ;
    let (iters, time, bw) = burn f size in
    Printf.printf "    % 5d:  %04f MB/s  (%d iters in %.03f s)\n%!"
      size (bw /. mb) iters time

let throughput_into ?(add = 0) title f =
  Printf.printf "\n* [%s]\n%!" title ;
  sizes |> List.iter @@ fun size ->
    Gc.full_major () ;
    let dst = Bytes.create (size + add) in
    let (iters, time, bw) = burn (f dst) size in
    Printf.printf "    % 5d:  %04f MB/s  (%d iters in %.03f s)\n%!"
      size (bw /. mb) iters time

let count_period = 10.

let count f n =
  ignore (f n);
  let i1 = 5 in
  let t1 = Time.time ~n:i1 f n in
  let iters = int_of_float (float i1 *. count_period /. t1) in
  let time  = Time.time ~n:iters f n in
  (iters, time)

let count title f to_str args =
  Printf.printf "\n* [%s]\n%!" title ;
  args |> List.iter @@ fun arg ->
  Gc.full_major () ;
  let iters, time = count f arg in
  Printf.printf "    %s:  %.03f ops per second (%d iters in %.03f)\n%!"
    (to_str arg) (float iters /. time) iters time

let msg_str = String.make 100 '\xAA'

let msg_str_32 = String.sub msg_str 0 32
let msg_str_48 = String.sub msg_str 0 48
let msg_str_65 = String.sub msg_str 0 65

module PSS = Mirage_crypto_pk.Rsa.PSS(Digestif.SHA256)

let rsa_1024 =
  let p = Z.of_string "10798561676627454710140432432014696449593673631094049392368450463276546091610832740190717321579865870896133380991892468262437092547408603618427685009427773"
  and q = Z.of_string "10400664760062606994038747844895625872750212488858602663971334563613232045185857177383833781411830934303117994128623611996670112272953487791473086751129863"
  and e = Z.of_string "65537"
  in
  match Mirage_crypto_pk.Rsa.priv_of_primes ~e ~p ~q with Ok r -> r | _ -> assert false

let enc_1024 = Mirage_crypto_pk.Rsa.(encrypt ~key:(pub_of_priv rsa_1024) msg_str)

let pkcs1_sig_1024 () =
  Mirage_crypto_pk.Rsa.PKCS1.sign ~hash:`SHA256 ~key:rsa_1024 (`Message msg_str)

let pkcs1_enc_1024 () =
  Mirage_crypto_pk.Rsa.(PKCS1.encrypt ~key:(pub_of_priv rsa_1024) msg_str)

let pss_sig_1024 () = PSS.sign ~key:rsa_1024 (`Message msg_str)

let rsa_2048 =
  let p = Z.of_string "146881832325800831419400417618624202055588545997890787121932184528831630537012732415698782899346395306540669232648045731896347007978622067056705527305566180903122107927148832001099595387953189273726394573803912262323600581299712943797238366745329534148223987933536186022708693674753193534229263584177098260169"
  and q = Z.of_string "146461957885839900502732892013745315236120923895767594427579857452138451155393985820080680192640369593315439290134409437965406213465262989382655388410330601076036910359057156988645246773259111682038096388585157599977808854893528900530706460128823381760931962351810679571404043148961709991714582814015259432029"
  and e = Z.of_string "65537"
  in
  match Mirage_crypto_pk.Rsa.priv_of_primes ~e ~p ~q with Ok r -> r | _ -> assert false

let enc_2048 = Mirage_crypto_pk.Rsa.(encrypt ~key:(pub_of_priv rsa_2048) msg_str)

let pkcs1_sig_2048 () =
  Mirage_crypto_pk.Rsa.PKCS1.sign ~hash:`SHA256 ~key:rsa_2048 (`Message msg_str)

let pkcs1_enc_2048 () =
  Mirage_crypto_pk.Rsa.(PKCS1.encrypt ~key:(pub_of_priv rsa_2048) msg_str)

let pss_sig_2048 () = PSS.sign ~key:rsa_2048 (`Message msg_str)

let rsa_4096 =
  let p = Z.of_string "30773596934476715066776070065844902670036493980016387964275170019397018472432997910667589359581914549510631424565206701540136804180560112829236103459317928059975099687383138310206374921731816027058152009810073337617754052401932141110921176212810704858018214605862299356217860547747262170495777126218319842708093667844701139914958775637423731967187071886349669479192453619522943080948061657926138418380417577129184420732857906610804965319661598089231703183044642635889126023201809407430354992888247464125783088294095728916671050049684448794153783082653555256735912037270303014887722063417225893745458164718800442738569"
  and q = Z.of_string "25905916162566396401205858643227945415345838368190515936191926936462899261314859092468495558719305083654714669527919862817831941531613073577910643681172802147392797914485263753968375221243705167910636382434485717071007054833155618588980128488599406663210791261135710551020276087851551652652612955553056226986980360600996201307035494196112173475664509878923172924086102889718336621725968169373963280305056864698229857913526607314633711315503607289470716733189135747457446349029256257187264165837635026903463931381823712962360342258652047970731347111703873301687821992467888598546386551115261010493902143134851640738743"
  and e = Z.of_string "65537"
  in
  match Mirage_crypto_pk.Rsa.priv_of_primes ~e ~p ~q with Ok r -> r | _ -> assert false

let enc_4096 = Mirage_crypto_pk.Rsa.(encrypt ~key:(pub_of_priv rsa_4096) msg_str)

let pkcs1_sig_4096 () =
  Mirage_crypto_pk.Rsa.PKCS1.sign ~hash:`SHA256 ~key:rsa_4096 (`Message msg_str)

let pkcs1_enc_4096 () =
  Mirage_crypto_pk.Rsa.(PKCS1.encrypt ~key:(pub_of_priv rsa_4096) msg_str)

let pss_sig_4096 () = PSS.sign ~key:rsa_4096 (`Message msg_str)

let dsa_1024 =
  let p = Z.of_string "115320471016337933377056549329182706825658339080795846324118938187917903660539570102468495091957028599543345588517799627361082806070282899880721557018345825086927289316756283826093243695405203187016738458545513419551779925532261196890562077023934735570005318513791942265699098088390517334916527653326493928799"
  and q = Z.of_string "823267969559752761552104454322087597915195665001"
  and gg = Z.of_string "107937769619514611906619060647411205822947624664377868769814121409943849987480570028955037310439082345400300825969182219850876363034452830224901430080806055218560008392720388910894912162956691999057850754409178667408425482805161601110189024138168750376391340697351250728689008407981372513900837280131855895453"
  and x = Z.of_string "33269272469299670210735451373406214067383586377"
  and y = Z.of_string "43917635707590891841908570055721669604556135044554274942460553515946670787931699807386932177837523342760860376770220825997328312057886461226985675983491441562087492365801663397409369541614646669226917344513472367438132106373179011858311945451923744651780314133078253880297369792145807736223662521868826642853"
  in
  match Mirage_crypto_pk.Dsa.priv ~fips:true ~p ~q ~gg ~x ~y () with Ok p -> p | _ -> assert false

let dsa_sig_1024 () = Mirage_crypto_pk.Dsa.sign ~key:dsa_1024 msg_str

let dsa_2048 =
  let p = Z.of_string "27787495469795504213817302334103600594688179071059183073859876165757248559489321478170600304273914000462158587756787453177210321379060448141559798652196363556897576291878245650614903612762833777567911000834171168229784178643222849655095281437320492725855855778320111645629834980350492228611813830302209080760811887894272862901026864911346096471199762409562102789142939773632891860019140618313962854554152891445175391927591825205548689170996430765723064763763481336517107917261869303217480777161449935319930795628114622197586510378927239068257979584784079128534248603619156372913573809491691986354447396965646770535701"
  and q = Z.of_string "69694877308167037149745913456421442195328554169759046914164177549875778020469"
  and gg = Z.of_string "16749627588066214399529603991445197534749244283120164288067836662918885787186948571007751498897778360267876697044209030527058098259975959998878027668545429739993477399366554325353523024222400972678469229055566504457717513550553993631550406867067991877458269091177591866978827953084168571646688881904998325355571633065354963580984543158204292013550690051754036914978697535194466008409541982818422484127204033337933785318568157008881227465007133605041651516579370726984057624602011504585990465767875831799514375203088558577008613314886723862237337922917306616319550969129882699756566436846243489458865623046875627558535"
  and x = Z.of_string "52860861934156228997899838985740859941028688506510055821449681896336427977580"
  and y = Z.of_string "16697822339875604612001674517725789858937385326266245308648897963274039128000291572870559603618903431422492771498840266582664620626826186158140214481920146230768719356298465470274085841064126012204317638870338958089054809559680905413861272320951150917299685355736888023985398767858005440235842845908452598291689850063919053521400020402671375982259094019780813889586357332424647365679629398571364161673401249910198031061651183589601097975488702613291524957230624044246866866974886847601585171352897595544769649242723658395277456627024303041369757410970512818793143517329469213213147704583953410882515213978051041273924"
  in
  match Mirage_crypto_pk.Dsa.priv ~fips:true ~p ~q ~gg ~x ~y () with Ok p -> p | _ -> assert false

let dsa_sig_2048 () = Mirage_crypto_pk.Dsa.sign ~key:dsa_2048 msg_str

let dsa_3072 =
  let p = Z.of_string "4944862491052787177238323499959371418651354629231656321315236369672827559263545931134286049323485061071828187289578269594065783019111035804017538871324004047710342711620233110167493989997579634523303899794913823240058891327833786211541568251787338957336540247816021098378292806006955851897646808403078979142749428669072523191276645021175423303816467433407072660616741824124536840773744646488191896772232795413707995397140064396495425700133866462410490239713815308709711960470201906326732033816522202617817869465691798938486540955726912350768931476362143768721380759395525951947017232778140349423557015356082357043807910825817719748257213281893007933859227824276579765323175836008193865064772817200047353825332039369252224256435661514851653526942065285711420907389170574343434449883875510985495078384130667046036846831401643151166834922210257258578675547742596423035828159461629721005113634334227074529533688136165903014911127"
  and q = Z.of_string "72036757532428134359049138716615314032674441223147930984416116642785279309001"
  and gg = Z.of_string "988301665281495772390013694627509692333967846948672137624515090935924385717634154201978961497509784579332702743535206413508559565302483922898459454403718843912379531742192312937734625047119678718271184170003455506604118936761508195594240052138536667234693864514877750501896049675764191029147963148241546820518065141123555298022010467792468407477159110344370654433269478015817957411602389410658876373667769353995724289566719120654426746425129842353040271693696527020500630012804936844492302532860691617810440827122662134370347136275931360845416833023047973072799739252681873195380321841873819721774703093238289342578739869306714624065593724407718101053836638039267362740083113357679437895609399028133545708736803196232072972950098992845234240283344492163375862712470338417546036591824286944195749933069780384676421299008472374982388004050973085425949637720603596481254386896408204626665775305048865550117840561595366712598318"
  and x = Z.of_string "57079433780483458942713357293831115449694380253611914431677328021806898761674"
  and y = Z.of_string "2872172233173100601346399502391482510148300641600207189246652621232656647081281141886455256814187251102030580186748870087277263359506423691023579912476503628426574690699263443704236625550832436226673947182294798279064154375422081421444400893924415563728656687773617245084962617886701420982288621397340217078717788290878177343138178149109067141997920049624616209300715994802074480057682676445987819355778630849636079364539062466859717465892222793824712941403252645431023939220711618233660062829520555618350855085688062241702521140104357981881833598269736147371800258828202964600523335598361833482597511071900975835195171061421945579132277176597724513608746614358311884564512362186106480065540201711903122228315570389409204443391052987813355120223856839756971514334338065850104261467285471383663470187296456006848469765768400814476981451863643010333596484259098557995874127140419649424228480234701334240448168704764430867915060"
  in
  match Mirage_crypto_pk.Dsa.priv ~fips:true ~p ~q ~gg ~x ~y () with Ok p -> p | _ -> assert false

let dsa_sig_3072 () = Mirage_crypto_pk.Dsa.sign ~key:dsa_3072 msg_str

let dh_groups =
  ["oakley5 (1536)",Mirage_crypto_pk.Dh.Group.oakley_5;
   "oakley14 (2048)",Mirage_crypto_pk.Dh.Group.oakley_14;
   "ffdhe2048",Mirage_crypto_pk.Dh.Group.ffdhe2048;
   "ffdhe3072",Mirage_crypto_pk.Dh.Group.ffdhe3072;
   "ffdhe4096",Mirage_crypto_pk.Dh.Group.ffdhe4096;
   "ffdhe6144",Mirage_crypto_pk.Dh.Group.ffdhe6144]

let dh_secrets =
  List.map2 (fun (n, group) s ->
      (n, group), Mirage_crypto_pk.Dh.key_of_secret group ~s)
    dh_groups
    (List.map (fun s -> Z.of_string s |> Mirage_crypto_pk.Z_extra.to_octets_be)
       [
         "31271182055444024732867835946284871743952969208281694762833912267184" ;
         "27594341083884344999714422172371027333192426063917478556668524561591" ;
         "49745209598738800585479479877345156590922715411063492309021724116430" ;
         "54263413298355776701974737228250185414758929445654879795198916482466337662578919821" ;
         "38599161911587526396222063388324161227700603198435442693976375015855884010520671067171149524070089" ;
         "60057457975706301816395663645420233759377744187465730049174048360108513636349450241008234412972340882517684187851" ;
       ])

let ecdsa_p256 =
  Result.get_ok
    (Mirage_crypto_ec.P256.Dsa.priv_of_octets
       "\x08\x9f\x4f\xfc\xcc\xf9\xba\x13\xfe\xdd\x09\x42\xef\x08\xcf\x2d\x90\x9f\x32\xe2\x93\x4a\xb5\xc9\x3b\x6c\x99\xbe\x5a\x9f\xf5\x27")

let ecdsa_p256_sig () = Mirage_crypto_ec.P256.Dsa.sign ~key:ecdsa_p256 msg_str_32

let ecdsa_p384 =
  Result.get_ok
    (Mirage_crypto_ec.P384.Dsa.priv_of_octets
       "\xf5\xc0\xc9\xfb\x95\x17\x86\x41\xaf\x76\xf3\x83\x1f\x41\xe2\xd3\x7c\xfa\xaf\xff\xc7\xe6\x01\x72\xcf\xb0\x89\xfe\x60\x4b\x56\xa6\x1c\x7c\x31\xa6\x90\x4b\x3b\x5d\x08\x20\x7a\x4b\x81\xe2\x5e\xa5")

let ecdsa_p384_sig () = Mirage_crypto_ec.P384.Dsa.sign ~key:ecdsa_p384 msg_str_48

let ecdsa_p521 =
  Result.get_ok
    (Mirage_crypto_ec.P521.Dsa.priv_of_octets
       "\x00\xb1\x8f\x60\xc0\x35\x2a\xd8\xe3\xef\x98\x2f\x1d\xdf\xcf\x6e\xec\x7f\xa6\xca\xf0\xe6\xf3\x68\x35\x4a\x8b\x02\xb2\xd8\xac\x1e\x05\x9e\x30\x98\x91\xe2\xbf\xa8\x57\x91\xa5\xe7\x1b\x40\xbd\xec\xbf\x90\x2b\xf2\x43\xdc\x3b\x00\x80\x49\x5c\xf4\xd9\x1c\x78\x72\x8b\xd5")

let ecdsa_p521_sig () = Mirage_crypto_ec.P521.Dsa.sign ~key:ecdsa_p521 msg_str_65

let ed25519 =
  Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets
                   "\x3e\x0a\xb6\x82\x17\x12\x75\xc5\x69\xfc\xe9\xca\x8b\xcc\xd2\xd2\x77\x14\x54\xa2\x30\x0c\x35\x29\xf7\xa4\xd8\x0b\x84\x38\x83\xbc")

let ed25519_sig () = Mirage_crypto_ec.Ed25519.sign ~key:ed25519 msg_str

let ecdsas = [
  ("P256", `P256 (ecdsa_p256, ecdsa_p256_sig ()));
  ("P384", `P384 (ecdsa_p384, ecdsa_p384_sig ()));
  ("P521", `P521 (ecdsa_p521, ecdsa_p521_sig ()));
  ("Ed25519", `Ed25519 (ed25519, ed25519_sig ()));
]

let ecdh_shares =
  [
    ("P256", `P256 (Mirage_crypto_ec.P256.Dh.secret_of_octets "\x47\x0d\x57\x70\x6c\x77\x06\xb6\x8a\x3f\x42\x3a\xea\xf4\xff\x7f\xdd\x02\x49\x4a\x10\xd3\xe3\x81\xc3\xc1\x1f\x72\x76\x80\x2c\xdc" |> Result.get_ok |> fst,
                    "\x04\x11\xb3\xfc\x82\x72\x1c\x26\x9a\x19\x90\x9a\x3b\x2f\xc2\x6d\x98\x95\x82\x6d\x0c\xfc\xbc\x1f\x76\x26\xe4\x88\xf0\x1f\x4c\xa6\xb5\xc5\xed\x76\xad\xee\x7a\xf8\x1b\xb2\x0b\x17\xcf\x23\x1c\xbf\x0c\x67\xdb\x02\x95\xd6\x8d\x1d\x92\xc2\xd2\xa5\xa8\x06\x38\xd7\x8d"));
    ("P384", `P384 (Mirage_crypto_ec.P384.Dh.secret_of_octets "\xee\x55\xe2\x9b\x61\x75\x2d\x5a\x3e\x52\x56\x56\xdb\x8b\xd8\xfe\x6f\x94\xfa\xb8\xaa\xcc\x9e\x92\xac\xff\x4c\x48\x12\xbf\x7a\x61\x87\xab\xa4\x6c\xc6\x0a\xb8\xf0\x8e\xfc\xf2\xd5\x74\x58\x4b\x74" |> Result.get_ok |> fst,
                    "\x04\x04\x89\xcf\x24\xbc\x80\xbf\x89\xfd\xfe\x9c\x05\xec\xc3\x9f\x69\x16\xad\x45\x09\xd9\x39\x85\x97\x95\x0d\x3d\x24\xe8\x28\xf6\xbf\x56\xba\x4a\xd6\xd2\x1e\xd7\x86\x3b\xed\x68\xe4\x13\x36\x4b\xd4\xc7\xb1\xe9\x04\x7d\x36\x12\x4c\x69\x53\xbe\x7c\x61\x20\x9c\xb3\xfc\x56\x45\x2f\x73\x05\x29\x37\x83\xc7\xc0\xed\x92\x9d\x6c\x98\xc7\xbc\x97\xf6\x0a\x72\xed\x22\x69\xa8\xeb\x19\xbb\x7e\xe1\x31"));
    ("P521", `P521 (Mirage_crypto_ec.P521.Dh.secret_of_octets "\x00\xaa\x47\x0b\xa1\xcc\x84\x3b\xa3\x14\x82\x1e\x72\xde\x4c\xd2\x99\xae\xc1\xf2\x6e\x9d\x64\xa0\xd8\x7d\xb1\x8a\x3d\xa9\xf6\x5c\x45\xec\xfc\xc5\x61\x7f\xf0\xd7\x3b\x2e\x0e\x1c\xdf\xf8\x04\x8e\x01\xbe\x5e\x20\x14\x94\x12\xe7\xdb\xfa\xb7\xfe\xae\x24\x9b\x1b\xfa\x4d" |> Result.get_ok |> fst,
                    "\x04\x00\x1d\x16\x29\xee\xb1\xc4\x25\xf9\x04\xd7\x55\x33\x00\x79\xd1\x3c\x77\xda\x92\x1e\x01\xcf\x50\xd7\x17\xe0\xd6\x85\x0a\x81\xa3\x90\x2b\xb9\x2a\x03\xfa\xea\xcb\xd6\x28\x9c\x15\x90\x68\x5a\x60\x44\xb5\xe9\x4d\xcf\xc4\x1d\xeb\x6a\x88\xdb\x62\xa8\x91\xb0\xb8\x93\xbb\x00\xe4\x2a\x66\xb2\xf0\x13\xbd\xd0\xd2\x7d\x8e\x07\xcb\x35\xfc\x3e\x2c\x2b\x22\xf9\x3e\xcf\xd5\xea\xb7\x88\x61\x97\xca\x07\x3c\x2c\x5e\x68\x31\xd6\x5e\x2d\x0b\x8a\xa4\x08\x43\x8e\x49\x54\x2f\x05\xf4\x1c\x57\x6d\xf7\x0e\x3c\xaf\x5b\xb8\x22\x7d\x48\x30\x94\xae\x58"));
    ("X25519", `X25519 (Mirage_crypto_ec.X25519.secret_of_octets "\x4c\x6d\xb7\xcf\x93\x5b\xcf\x84\x02\x61\x78\xd4\x0c\x95\x6a\xf0\x9d\x8e\x36\x32\x03\x49\x0d\x2c\x41\x62\x5a\xcb\x68\xb9\x31\xa4" |> Result.get_ok |> fst,
                        "\xca\x19\x19\x3c\xf5\xc0\xb3\x8c\x61\xaa\x01\xc1\x72\xb2\xe9\x3d\x16\xf7\x50\xd0\x84\x62\x77\xad\x32\x2d\xe5\xe4\xfb\x33\x24\x29"));
  ]

let bm name f = (name, fun () -> f name)

let benchmarks = [

  bm "rsa-generate" (fun name ->
      count name (fun bits -> Mirage_crypto_pk.Rsa.generate ~bits ())
        string_of_int [1024;2048;4096]) ;

  bm "rsa-encrypt" (fun name ->
      count name (fun key -> Mirage_crypto_pk.Rsa.(encrypt ~key:(pub_of_priv key) msg_str))
        (fun k -> string_of_int (Mirage_crypto_pk.Rsa.priv_bits k))
        [rsa_1024;rsa_2048;rsa_4096]) ;

  bm "rsa-decrypt" (fun name ->
      count name (fun (key, msg) -> Mirage_crypto_pk.Rsa.(decrypt ~key msg))
        (fun (k, _) -> string_of_int (Mirage_crypto_pk.Rsa.priv_bits k))
        [rsa_1024,enc_1024 ; rsa_2048,enc_2048 ; rsa_4096,enc_4096]) ;

  bm "rsa-pkcs1-encrypt" (fun name ->
      count name (fun key -> Mirage_crypto_pk.Rsa.(PKCS1.encrypt ~key:(pub_of_priv key) msg_str))
        (fun k -> string_of_int (Mirage_crypto_pk.Rsa.priv_bits k))
        [rsa_1024;rsa_2048;rsa_4096]) ;

  bm "rsa-pkcs1-decrypt" (fun name ->
      count name (fun (key, msg) -> Mirage_crypto_pk.Rsa.(PKCS1.decrypt ~key msg))
        (fun (k, _) -> string_of_int (Mirage_crypto_pk.Rsa.priv_bits k))
        [rsa_1024,pkcs1_enc_1024 () ; rsa_2048,pkcs1_enc_2048 () ; rsa_4096,pkcs1_enc_4096 ()]) ;

  bm "rsa-pkcs1-sign" (fun name ->
      count name (fun key -> Mirage_crypto_pk.Rsa.PKCS1.sign ~hash:`SHA256 ~key (`Message msg_str))
        (fun k -> string_of_int (Mirage_crypto_pk.Rsa.priv_bits k))
        [rsa_1024;rsa_2048;rsa_4096]) ;

  bm "rsa-pkcs1-verify" (fun name ->
      count name (fun (key, signature) ->
          Mirage_crypto_pk.Rsa.(PKCS1.verify ~hashp:(fun _ -> true) ~key:(pub_of_priv key) ~signature (`Message msg_str)))
        (fun (k, _) -> string_of_int (Mirage_crypto_pk.Rsa.priv_bits k))
        [rsa_1024,pkcs1_sig_1024 () ; rsa_2048,pkcs1_sig_2048 () ; rsa_4096,pkcs1_sig_4096 ()]) ;

  bm "rsa-pss-sign" (fun name ->
      count name (fun key -> PSS.sign ~key (`Message msg_str))
        (fun k -> string_of_int (Mirage_crypto_pk.Rsa.priv_bits k))
        [rsa_1024;rsa_2048;rsa_4096]) ;

  bm "rsa-pss-verify" (fun name ->
      count name (fun (key, signature) ->
          PSS.verify ~key:(Mirage_crypto_pk.Rsa.pub_of_priv key) ~signature (`Message msg_str))
        (fun (k, _) -> string_of_int (Mirage_crypto_pk.Rsa.priv_bits k))
        [rsa_1024,pss_sig_1024 () ; rsa_2048,pss_sig_2048 () ; rsa_4096,pss_sig_4096 ()]) ;

  bm "dsa-generate" (fun name ->
      count name (fun ks -> Mirage_crypto_pk.Dsa.generate ks)
        (function `Fips1024 -> "1024" | `Fips2048 -> "2048" | `Fips3072 -> "3072" | `Exactly (l, _) -> string_of_int l)
        [`Fips1024;`Fips2048;`Fips3072]);

  bm "dsa-sign" (fun name ->
      count name (fun key -> Mirage_crypto_pk.Dsa.sign ~key msg_str)
        (fun k -> string_of_int (Z.numbits k.p))
        [dsa_1024;dsa_2048;dsa_3072]);

  bm "dsa-verify" (fun name ->
      count name (fun (key, signature) ->
          Mirage_crypto_pk.Dsa.(verify ~key:(pub_of_priv key) signature msg_str))
        (fun (k, _) -> string_of_int (Z.numbits k.p))
        [dsa_1024,dsa_sig_1024 () ; dsa_2048,dsa_sig_2048 () ; dsa_3072,dsa_sig_3072 ()]);

  bm "ecdsa-generate" (fun name ->
      let open Mirage_crypto_ec in
      count name
        (fun (_, x) -> match x with
           | `P256 _ -> P256.Dsa.generate () |> ignore
           | `P384 _ -> P384.Dsa.generate () |> ignore
           | `P521 _ -> P521.Dsa.generate () |> ignore
           | `Ed25519 _ -> Ed25519.generate () |> ignore
        )
        fst ecdsas);

  bm "ecdsa-sign" (fun name ->
      let open Mirage_crypto_ec in
      count name (fun (_, x) -> match x with
          | `P256 (key, _) -> P256.Dsa.sign ~key msg_str_32
          | `P384 (key, _) -> P384.Dsa.sign ~key msg_str_48
          | `P521 (key, _) -> P521.Dsa.sign ~key msg_str_65
          | `Ed25519 (key, _) -> Ed25519.sign ~key msg_str, ""
        )
        fst ecdsas);

  bm "ecdsa-verify" (fun name ->
      let open Mirage_crypto_ec in
      count name (fun (_, x) -> match x with
          | `P256 (key, signature) -> P256.Dsa.(verify ~key:(pub_of_priv key) signature msg_str_32)
          | `P384 (key, signature) -> P384.Dsa.(verify ~key:(pub_of_priv key) signature msg_str_48)
          | `P521 (key, signature) -> P521.Dsa.(verify ~key:(pub_of_priv key) signature msg_str_65)
          | `Ed25519 (key, signature) -> Ed25519.(verify ~key:(pub_of_priv key) signature ~msg:msg_str)
        ) fst ecdsas);

  bm "dh-secret" (fun name ->
      count name (fun (_, group) -> Mirage_crypto_pk.Dh.gen_key group)
        fst dh_groups);

  bm "dh-share" (fun name ->
      count name (fun (_, (sec, share)) ->
          Mirage_crypto_pk.Dh.shared sec share)
        (fun ((g, _), _) -> g) dh_secrets);

  bm "ecdh-secret" (fun name ->
      let open Mirage_crypto_ec in
      count name (fun (_, x) -> match x with
          | `P256 _ -> P256.Dh.gen_key () |> ignore
          | `P384 _ -> P384.Dh.gen_key () |> ignore
          | `P521 _ -> P521.Dh.gen_key () |> ignore
          | `X25519 _ -> X25519.gen_key () |> ignore)
        fst ecdh_shares);

  bm "ecdh-share" (fun name ->
      let open Mirage_crypto_ec in
      count name (fun (_, x) -> match x with
          | `P256 (sec, share) -> P256.Dh.key_exchange sec share |> Result.get_ok |> ignore
          | `P384 (sec, share) -> P384.Dh.key_exchange sec share |> Result.get_ok |> ignore
          | `P521 (sec, share) -> P521.Dh.key_exchange sec share |> Result.get_ok |> ignore
          | `X25519 (sec, share) -> X25519.key_exchange sec share  |> Result.get_ok |> ignore)
        fst ecdh_shares);

  bm "chacha20-poly1305" (fun name ->
      let key = Chacha20.of_secret (Mirage_crypto_rng.generate 32)
      and nonce = Mirage_crypto_rng.generate 8 in
      throughput_into ~add:Chacha20.tag_size name
        (fun dst cs -> Chacha20.authenticate_encrypt_into ~key ~nonce cs ~src_off:0 dst ~dst_off:0 ~tag_off:(String.length cs) (String.length cs))) ;

  bm "chacha20-poly1305-unsafe" (fun name ->
      let key = Chacha20.of_secret (Mirage_crypto_rng.generate 32)
      and nonce = Mirage_crypto_rng.generate 8 in
      throughput_into ~add:Chacha20.tag_size name
        (fun dst cs -> Chacha20.unsafe_authenticate_encrypt_into ~key ~nonce cs ~src_off:0 dst ~dst_off:0 ~tag_off:(String.length cs) (String.length cs))) ;

  bm "aes-128-ecb" (fun name ->
    let key = AES.ECB.of_secret (Mirage_crypto_rng.generate 16) in
    throughput_into name
      (fun dst cs -> AES.ECB.encrypt_into ~key cs ~src_off:0 dst ~dst_off:0 (String.length cs))) ;

  bm "aes-192-ecb" (fun name ->
    let key = AES.ECB.of_secret (Mirage_crypto_rng.generate 24) in
    throughput_into name (fun dst cs -> AES.ECB.encrypt_into ~key cs ~src_off:0 dst ~dst_off:0 (String.length cs))) ;

  bm "aes-192-ecb-unsafe" (fun name ->
    let key = AES.ECB.of_secret (Mirage_crypto_rng.generate 24) in
    throughput_into name (fun dst cs -> AES.ECB.unsafe_encrypt_into ~key cs ~src_off:0 dst ~dst_off:0 (String.length cs))) ;

  bm "aes-256-ecb" (fun name ->
    let key = AES.ECB.of_secret (Mirage_crypto_rng.generate 32) in
    throughput_into name (fun dst cs -> AES.ECB.encrypt_into ~key cs ~src_off:0 dst ~dst_off:0 (String.length cs))) ;

  bm "aes-256-ecb-unsafe" (fun name ->
    let key = AES.ECB.of_secret (Mirage_crypto_rng.generate 32) in
    throughput_into name (fun dst cs -> AES.ECB.unsafe_encrypt_into ~key cs ~src_off:0 dst ~dst_off:0 (String.length cs))) ;

  bm "aes-128-ecb-unsafe" (fun name ->
    let key = AES.ECB.of_secret (Mirage_crypto_rng.generate 16) in
    throughput_into name
      (fun dst cs -> AES.ECB.unsafe_encrypt_into ~key cs ~src_off:0 dst ~dst_off:0 (String.length cs))) ;

  bm "aes-128-cbc-e" (fun name ->
    let key = AES.CBC.of_secret (Mirage_crypto_rng.generate 16)
    and iv  = Mirage_crypto_rng.generate 16 in
    throughput_into name
      (fun dst cs -> AES.CBC.encrypt_into ~key ~iv cs ~src_off:0 dst ~dst_off:0 (String.length cs))) ;

  bm "aes-128-cbc-e-unsafe" (fun name ->
    let key = AES.CBC.of_secret (Mirage_crypto_rng.generate 16)
    and iv  = Mirage_crypto_rng.generate 16 in
    throughput_into name
      (fun dst cs -> AES.CBC.unsafe_encrypt_into ~key ~iv cs ~src_off:0 dst ~dst_off:0 (String.length cs))) ;

  bm "aes-128-cbc-e-unsafe-inplace" (fun name ->
    let key = AES.CBC.of_secret (Mirage_crypto_rng.generate 16)
    and iv  = Mirage_crypto_rng.generate 16 in
    throughput name
      (fun cs ->
         let b = Bytes.unsafe_of_string cs in
         AES.CBC.unsafe_encrypt_into_inplace ~key ~iv b ~dst_off:0 (String.length cs))) ;

  bm "aes-128-cbc-d" (fun name ->
    let key = AES.CBC.of_secret (Mirage_crypto_rng.generate 16)
    and iv  = Mirage_crypto_rng.generate 16 in
    throughput_into name
      (fun dst cs -> AES.CBC.decrypt_into ~key ~iv cs ~src_off:0 dst ~dst_off:0 (String.length cs))) ;

  bm "aes-128-cbc-d-unsafe" (fun name ->
    let key = AES.CBC.of_secret (Mirage_crypto_rng.generate 16)
    and iv  = Mirage_crypto_rng.generate 16 in
    throughput_into name
      (fun dst cs -> AES.CBC.unsafe_decrypt_into ~key ~iv cs ~src_off:0 dst ~dst_off:0 (String.length cs))) ;

  bm "aes-128-ctr" (fun name ->
    let key = Mirage_crypto_rng.generate 16 |> AES.CTR.of_secret
    and ctr = Mirage_crypto_rng.generate 16 |> AES.CTR.ctr_of_octets in
    throughput_into name (fun dst cs -> AES.CTR.encrypt_into ~key ~ctr cs ~src_off:0 dst ~dst_off:0 (String.length cs))) ;

  bm "aes-128-ctr-unsafe" (fun name ->
    let key = Mirage_crypto_rng.generate 16 |> AES.CTR.of_secret
    and ctr = Mirage_crypto_rng.generate 16 |> AES.CTR.ctr_of_octets in
    throughput_into name (fun dst cs -> AES.CTR.unsafe_encrypt_into ~key ~ctr cs ~src_off:0 dst ~dst_off:0 (String.length cs))) ;

  bm "aes-128-gcm" (fun name ->
    let key = AES.GCM.of_secret (Mirage_crypto_rng.generate 16)
    and nonce = Mirage_crypto_rng.generate 12 in
    throughput_into ~add:AES.GCM.tag_size name
      (fun dst cs -> AES.GCM.authenticate_encrypt_into ~key ~nonce cs ~src_off:0 dst ~dst_off:0 ~tag_off:(String.length cs) (String.length cs)));

  bm "aes-128-gcm-unsafe" (fun name ->
    let key = AES.GCM.of_secret (Mirage_crypto_rng.generate 16)
    and nonce = Mirage_crypto_rng.generate 12 in
    throughput_into ~add:AES.GCM.tag_size name
      (fun dst cs -> AES.GCM.unsafe_authenticate_encrypt_into ~key ~nonce cs ~src_off:0 dst ~dst_off:0 ~tag_off:(String.length cs) (String.length cs)));

  bm "aes-128-ghash" (fun name ->
    let key = AES.GCM.of_secret (Mirage_crypto_rng.generate 16)
    and nonce = Mirage_crypto_rng.generate 12 in
    throughput_into ~add:AES.GCM.tag_size name
      (fun dst cs -> AES.GCM.authenticate_encrypt_into ~key ~nonce ~adata:cs "" ~src_off:0 dst ~dst_off:0 ~tag_off:0 0));

  bm "aes-128-ghash-unsafe" (fun name ->
    let key = AES.GCM.of_secret (Mirage_crypto_rng.generate 16)
    and nonce = Mirage_crypto_rng.generate 12 in
    throughput_into ~add:AES.GCM.tag_size name
      (fun dst cs -> AES.GCM.unsafe_authenticate_encrypt_into ~key ~nonce ~adata:cs "" ~src_off:0 dst ~dst_off:0 ~tag_off:0 0));

  bm "aes-128-ccm" (fun name ->
    let key   = AES.CCM16.of_secret (Mirage_crypto_rng.generate 16)
    and nonce = Mirage_crypto_rng.generate 10 in
    throughput_into ~add:AES.CCM16.tag_size name
      (fun dst cs -> AES.CCM16.authenticate_encrypt_into ~key ~nonce cs ~src_off:0 dst ~dst_off:0 ~tag_off:(String.length cs) (String.length cs)));

  bm "aes-128-ccm-unsafe" (fun name ->
    let key   = AES.CCM16.of_secret (Mirage_crypto_rng.generate 16)
    and nonce = Mirage_crypto_rng.generate 10 in
    throughput_into ~add:AES.CCM16.tag_size name
      (fun dst cs -> AES.CCM16.unsafe_authenticate_encrypt_into ~key ~nonce cs ~src_off:0 dst ~dst_off:0 ~tag_off:(String.length cs) (String.length cs)));

  bm "d3des-ecb" (fun name ->
    let key = DES.ECB.of_secret (Mirage_crypto_rng.generate 24) in
    throughput_into name (fun dst cs -> DES.ECB.encrypt_into ~key cs ~src_off:0 dst ~dst_off:0 (String.length cs))) ;

  bm "d3des-ecb-unsafe" (fun name ->
    let key = DES.ECB.of_secret (Mirage_crypto_rng.generate 24) in
    throughput_into name (fun dst cs -> DES.ECB.unsafe_encrypt_into ~key cs ~src_off:0 dst ~dst_off:0 (String.length cs))) ;

  bm "fortuna" (fun name ->
    begin[@alert "-deprecated"]
      Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
      throughput name (fun buf ->
          let buf = Bytes.unsafe_of_string buf in
          Mirage_crypto_rng.generate_into buf ~off:0 (Bytes.length buf))
    end);

  bm "getentropy" (fun name ->
    Mirage_crypto_rng_unix.use_getentropy ();
    throughput name (fun buf ->
        let buf = Bytes.unsafe_of_string buf in
        Mirage_crypto_rng.generate_into buf ~off:0 (Bytes.length buf))) ;

  bm "urandom" (fun name ->
    Mirage_crypto_rng_unix.use_dev_urandom ();
    throughput name (fun buf ->
        let buf = Bytes.unsafe_of_string buf in
        Mirage_crypto_rng.generate_into buf ~off:0 (Bytes.length buf))) ;
]

let help () =
  Printf.printf "available benchmarks:\n  ";
  List.iter (fun (n, _) -> Printf.printf "%s  " n) benchmarks ;
  Printf.printf "\n%!"

let runv fs =
  Format.printf "accel: %a\n%!"
    (fun ppf -> List.iter @@ fun x ->
      Format.fprintf ppf "%s " @@
        match x with `XOR -> "XOR" | `AES -> "AES" | `GHASH -> "GHASH")
    accelerated;
  Time.warmup () ;
  List.iter (fun f -> f ()) fs


let () =
  let seed = "abcd" in
  let g = Mirage_crypto_rng.(create ~seed (module Fortuna)) in
  Mirage_crypto_rng.set_default_generator g;
  match Array.to_list Sys.argv with
  | _::(_::_ as args) -> begin
      try
        let fs =
          args |> List.map @@ fun n ->
            snd (benchmarks |> List.find @@ fun (n1, _) -> n = n1) in
        runv fs
      with Not_found -> help ()
    end
  | _ -> help ()
