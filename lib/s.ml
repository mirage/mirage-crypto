
module type ENTROPY = sig
   include V1_LWT.ENTROPY
     with type 'a io = 'a Lwt.t
     with type id = string
     with type buffer = Cstruct.t
end
