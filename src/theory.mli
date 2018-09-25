module type S =
sig
  val meta : Dkmeta.cfg
end

val from_file : Signature.t list -> bool -> string -> (module S)
