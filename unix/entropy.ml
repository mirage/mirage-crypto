open Lwt
open Lwt_unix

type t = { id : string ; fd : file_descr }
type id = string
type 'a io = 'a Lwt.t
type buffer = Cstruct.t
type error = [ `Invalid_entropy of string ]

let connect id =
  lwt fd = openfile "/dev/random" [ Unix.O_RDONLY ] 0 in
  return (`Ok { id ; fd } )

let disconnect { fd = fd } = close fd

let id { id } = id

let entropy { fd = fd } len =
  let r = Cstruct.create len in
  lwt res = Lwt_cstruct.read fd r in
  return (`Ok r)
