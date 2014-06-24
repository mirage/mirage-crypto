open Lwt

type t = { id : string }
type id = string
type 'a io = 'a Lwt.t
type buffer = Cstruct.t
type error = [ `Invalid_entropy of string ]

let connect id = return (`Ok { id } )
let disconnect _ = return ()
let id { id } = id

let entropy t len =
  let r = Cstruct.create len in
  return (`Ok r)
