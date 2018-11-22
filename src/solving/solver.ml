module F = Common.Files
module O = Common.Oracle
module U = Common.Universes

(** model is a function that associate to each fresh universe a concrete universe *)
type model = Basic.name -> U.univ

(** Signature for an abstract solver *)
module type SOLVER =
sig

  (** [add pred] add the predicate [cstr] to the solver  *)
  val add   : U.cstr -> unit

  (** [solve mk_theory] call the solver and returns the mimimum number of universes needed to solve the constraints as long as the model. The theory used by solver depends on the number of universes needed. Hence one needs to provide a function [mk_theory] that builds a theory when at most [i] are used.*)
  val solve   : O.theory_maker -> int * model

end

(** A concrete implementation of a solver using Z3 with non interpreted symbol functions for universes *)
module Z3Syn : Z3cfg.ALGEBRAIC = Z3syn

module Z3Arith : Z3cfg.ALGEBRAIC = Z3arith

module type S =
sig
  val parse : Dkmeta.cfg -> F.file -> unit

  val print_model : Dkmeta.cfg -> model -> F.file -> unit

  val solve : O.theory_maker -> int * model
end


module Make(S:SOLVER) : S =
struct
  (** [from_rule pat rhs] add the assertion [pat = rhs] to Z3. *)
  let from_rule : Rule.pattern -> Term.term -> U.cstr = fun pat right ->
    let left   = Rule.pattern_to_term pat in
    try (* the constraint is a predicate *)
      U.Pred (U.extract_pred left)
    with U.Not_pred ->
      (* the constraint is en equality between variables *)
      let left' = Elaboration.Var.name_of_uvar left in
      let right' = Elaboration.Var.name_of_uvar right in
      U.EqVar(left',right')

  (** [parse meta s] parses a constraint file. *)
  let parse : Dkmeta.cfg -> string -> unit =
    fun meta file ->
      let md_elab = F.md_of_file (F.from_string file `Elaboration) in
      let md_check = F.md_of_file (F.from_string file `Checking) in
      (* meta transform the constraints to universos constraints *)
      let mk_entry = function
        | Entry.Rules(_,rs) ->
          List.map (fun (r:Rule.untyped_rule) -> from_rule r.pat r.rhs) rs
        | _  -> assert false
      in
      let mk_entry e = mk_entry (Dkmeta.mk_entry meta md_elab e) in
      let entries = Parser.Parse_channel.parse md_check (open_in file) in
      let entries' = List.flatten (List.map mk_entry entries) in
      List.iter S.add entries'

  (** [print_model meta model f] print the model associated to the universes elaborated in file [f]. Each universe are elaborated to the original universe theory thanks to the dkmeta [meta] configuration. *)
  let print_model meta model in_file =
    let elab_file = F.from_string in_file `Elaboration in
    let md = F.md_of_file elab_file in
    (* extract declarations from [file_univ] *)
    let mk_entry = function
      | Entry.Decl(_,id,_,_) -> Basic.mk_name md id
      | _ -> assert false
    in
    let ic = open_in elab_file in
    let entries = Parser.Parse_channel.parse md ic in
    let oc = open_out (F.from_string in_file `Solution) in
    let fmt = Format.formatter_of_out_channel oc in
    let print_rule e =
      let name = mk_entry e in
      let sol = model name in
      let rhs = U.term_of_univ sol in
      (* Solution is translated back to the original theory *)
      let rhs' = Dkmeta.mk_term meta rhs in
      (* Solution is encoded as rewrite rules to make the files type check. *)
      Format.fprintf fmt "[] %a --> %a.@." Pp.print_name name Pp.print_term rhs'
    in
    List.iter print_rule entries

  let solve = S.solve
end
