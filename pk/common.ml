let rec until p f = let r = f () in if p r then r else until p f

(* The Sexplib hack... *)
module Z = struct
  include Z

  let two   = ~$2
  let three = ~$3

  let pp = pp_print

  open Sexplib.Conv
  let sexp_of_t z = sexp_of_string (Z.to_string z)
  let t_of_sexp s = Z.of_string (string_of_sexp s)
end
