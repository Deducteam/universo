module type Compat =
sig
  val constructors : Rule.untyped_rule list

  val sorts : Rule.untyped_rule list
end
