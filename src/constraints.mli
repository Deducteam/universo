module type Generation =
sig

  type constraints

  module CS : Set.S with type elt = constraints

  val generate : Parser.entry -> CS.t

end

module Basic : Generation
