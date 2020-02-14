module Fc = struct
  open Mirage_crypto_pk

  module Rng = struct
    type 'a t = (module Rng.N with type t = 'a)
    let int   : int   t = (module Rng.Int)
    let int32 : int32 t = (module Rng.Int32)
    let int64 : int64 t = (module Rng.Int64)
    let z     : Z.t   t = (module Rng.Z)
  end

  module Numeric = struct
    type 'a t = (module Numeric.S with type t = 'a)
    let int   : int   t = (module Numeric.Int)
    let int32 : int32 t = (module Numeric.Int32)
    let int64 : int64 t = (module Numeric.Int64)
    let z     : Z.t   t = (module Numeric.Z)
  end
end

let sample arr =
  let ix = Mirage_crypto_pk.Rng.Int.gen Array.(length arr) in arr.(ix)

let random_is seed = Mirage_crypto_rng.create ~seed (module Mirage_crypto_rng.Null)

let bits64 x =
  Bytes.init 64 @@ fun i ->
    let o = 63 - i in
    if Mirage_crypto_pk.Numeric.Int64.((x lsr o) land 1L = 1L) then '1' else '0'

let vz = Z.of_string_base 16
