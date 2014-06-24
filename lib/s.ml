
module type ENTROPY = V1_LWT.ENTROPY
  with type 'a io = 'a Lwt.t
  and type id = string
  and type buffer = Cstruct.t
