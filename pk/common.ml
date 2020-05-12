let rec until p f = let r = f () in if p r then r else until p f

let guard p err = if p then Ok () else Error err

let src = Logs.Src.create "mirage-crypto-pk" ~doc:"Mirage crypto public key"
module Log = (val Logs.src_log src : Logs.LOG)

(* The Sexplib hack... *)
module Z_sexp = struct
  type t = Z.t

  open Sexplib.Conv
  let sexp_of_t z = sexp_of_string (Z.to_string z)
  let t_of_sexp s = Z.of_string (string_of_sexp s)
end
