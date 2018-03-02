open Basic


type constraints =
  | Univ of ident * int
  | Eq of ident * ident
  | Max of ident * ident * ident
  | Succ of ident * ident
  | Rule of ident * ident * ident


module type Generation =
sig

  type t

  val generate : Basic.ident list -> Parser.entry -> t

end

module CS : Set.S with type elt = constraints

module Naive : Generation with type t = CS.t

module Test  : Generation with type t = unit
