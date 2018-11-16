module U = Checking.Universes

type model = Basic.name -> U.univ

module type SOLVER =
sig

  val parse   : Basic.mident -> Basic.mident -> string -> string -> unit

  val solve   : Dkmeta.cfg -> int * model

  val reset   : unit -> unit
end


module Z3Syn : SOLVER  = Z3solver.Syn
