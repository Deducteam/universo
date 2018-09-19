module type S =
sig
  val rules : Rule.untyped_rule list
end

module type In = S

module type Th = S

module type Out = S

val from_file : string -> (module S)
