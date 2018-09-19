val init : Signature.t -> Basic.mident -> unit

module type S =
  sig
    include Typing.Typer

    val mk_entry : Configuration.t -> Entry.entry -> unit
  end

module Make(TH:Theory.Th) : S
