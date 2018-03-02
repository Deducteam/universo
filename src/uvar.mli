type uvar

exception NotUvar

val is_uvar : Term.term -> bool

val ident_of_uvar : Term.term -> Basic.ident
(** may raise NotUvar exception *)

val fresh_uvar : unit -> Term.term
(** Add a fresh definable variable to the current environment and returns the fresh variable as a term.*)

module Elaboration :
sig
  val prop_elaboration  : bool ref
  val elaboration_entry : Entry.entry -> Basic.ident list * Entry.entry
end
