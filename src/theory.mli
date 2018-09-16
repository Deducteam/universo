module type In =
sig
  val constructors : Rule.untyped_rule list

  val sorts : Rule.untyped_rule list
end

module type Out =
sig
  val out : Rule.untyped_rule list
end
