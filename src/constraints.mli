open Basic

module type Generation =
sig

  type constraints =
    | Univ of ident * int
    | Eq of ident * ident
    | Max of ident * ident * ident
    | Succ of ident * ident
    | Rule of ident * ident * ident

  module CS : Set.S with type elt = constraints

  val generate : Basic.ident list -> Parser.entry -> CS.t

end

module Basic : Generation
