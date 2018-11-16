module U = Checking.Universes

type model = Basic.name -> U.univ

module type SOLVER =
sig

  type t =
    {
      model : Dkmeta.cfg
    }

  val parse   : Basic.mident -> Basic.mident -> string -> string -> unit

  val solve   : t -> int * model

  val reset   : unit -> unit
end


module Z3Syn : SOLVER  = Z3solver.Syn
