module F = Common.Files
module L = Common.Log
module O = Common.Oracle
module U = Common.Universes

type env =
  {
    mk_theory: O.theory_maker;
    (** construct a list of axioms,rules and cumulativity with there truth value for n universes. *)
    min: int;
    (** minimum number of universes to check *)
    max: int;
    (** maximum number of universes to check *)
    smt2_file: bool;
    (** print an smt2 file *)
  }


(** [model] is a function that associate to each fresh universe a concrete universe. *)
type model = Basic.name -> U.univ

(** Signature for an abstract solver *)
module type SOLVER =
sig
  (** [add pred] add the predicate [cstr] to the solver  *)
  val add   : U.cstr -> unit

  (** [solve mk_theory] call the solver and returns the mimimum number of universes needed to solve the constraints as long as the model. The theory used by solver depends on the number of universes needed. Hence one needs to provide a function [mk_theory] that builds a theory when at most [i] are used.*)
  val solve : O.theory_maker -> int * model
end

(** A concrete implementation of a solver using Z3 with non interpreted symbol functions for universes *)

module ZSyn = Z3cfg.Make(Z3syn)
module ZArith = Z3cfg.Make(Z3arith)

module type S =
sig
  val parse : Dkmeta.cfg -> F.path -> unit

  val print_model : Dkmeta.cfg -> model -> F.path -> unit

  val solve : O.theory_maker -> int * model
end


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

module Make(S:SOLVER) : S =
struct

  (** [parse meta s] parses a constraint file. *)
  let parse : Dkmeta.cfg -> string -> unit =
    fun meta in_path ->
      let md_elab = F.md_of in_path `Elaboration in
      let md_check = F.md_of in_path `Checking in
      (* meta transform the constraints to universos constraints *)
      let mk_entry = function
        | Entry.Rules(_,rs) ->
          List.map (fun (r:Rule.untyped_rule) -> from_rule r.pat r.rhs) rs
        | Entry.Require _ -> []
        | _ -> assert false
      in
      let mk_entry e = mk_entry (Dkmeta.mk_entry meta md_elab e) in
      let cstr_file = F.in_from_string in_path `Checking in
      let entries = Parser.Parse_channel.parse md_check (F.in_channel_of_file cstr_file) in
      let entries' = List.flatten (List.map mk_entry entries) in
      List.iter S.add entries'

  (** [print_model meta model f] print the model associated to the universes elaborated in file [f]. Each universe are elaborated to the original universe theory thanks to the dkmeta [meta] configuration. *)
  let print_model meta model in_path =
    let elab_file = F.in_from_string in_path `Elaboration in
    (* extract declarations from [file_univ] *)
    let mk_entry = function
      | Entry.Decl(_,id,_,_) -> Some(Basic.mk_name elab_file.md id)
      | Entry.Require _ -> None
      | _ -> assert false
    in
    let entries = Parser.Parse_channel.parse elab_file.md (F.in_channel_of_file elab_file) in
    let sol_file = F.out_from_string in_path `Solution in
    let fmt = F.fmt_of_file sol_file in
    F.add_requires fmt [F.md_of in_path `Elaboration; F.md_of_path !F.theory];
    let print_rule e =
      match mk_entry e with
      | None -> ()
      | Some name ->
        let sol = model name in
        let rhs = U.term_of_univ sol in
        (* Solution is translated back to the original theory *)
        let rhs' = Dkmeta.mk_term meta rhs in
        (* Solution is encoded as rewrite rules to make the files type check. *)
        Format.fprintf fmt "[] %a --> %a.@." Pp.print_name name Pp.print_term rhs'
    in
    List.iter print_rule entries;
    F.close_in elab_file;
    F.close_out sol_file

  let solve = S.solve
end

(** Performance are bad with LRA *)
module MakeUF(S:SOLVER) : S =
struct

  module SP = Set.Make(struct type t = U.pred let compare = compare end)

  let sg = Signature.make "solver"

  let sp = ref SP.empty

  let mk_rule : Rule.untyped_rule -> unit = fun r ->
    match from_rule r.pat r.rhs with
    | U.EqVar _ -> Signature.add_rules sg [(Rule.to_rule_infos r)]
    | U.Pred p -> sp := SP.add p !sp

  (** [parse meta s] parses a constraint file. *)
  let parse : Dkmeta.cfg -> string -> unit =
    fun meta in_path ->
      let md_elab = F.md_of in_path `Elaboration in
      let md_check = F.md_of in_path `Checking in
      (* meta transform the constraints to universos constraints *)
      let mk_entry = function
        | Entry.Rules(_,rs) -> List.iter mk_rule rs
        | Entry.Require _ -> ()
        | _ -> assert false
      in
      let mk_entry e = mk_entry (Dkmeta.mk_entry meta md_elab e) in
      let cstr_file = F.in_from_string in_path `Checking in
      let entries = Parser.Parse_channel.parse md_check (F.in_channel_of_file cstr_file) in
      List.iter mk_entry entries

  (* List.iter S.add entries' *)

  (** [print_model meta model f] print the model associated to the universes elaborated in file [f]. Each universe are elaborated to the original universe theory thanks to the dkmeta [meta] configuration. *)
  let print_model meta model in_path  =
    let elab_file = F.in_from_string in_path `Elaboration in
    (* extract declarations from [file_univ] *)
    let mk_entry = function
      | Entry.Decl(_,id,_,_) -> Some(Basic.mk_name elab_file.md id)
      | Entry.Require _ -> None
      | _ -> assert false
    in
    let find =
      let meta = {Dkmeta.default_config with sg = sg} in
      fun name ->
        match Dkmeta.mk_term meta (Term.mk_Const Basic.dloc name) with
        | Term.Const(_,name) -> name
        | _ -> assert false
    in
    let entries = Parser.Parse_channel.parse elab_file.md (F.in_channel_of_file elab_file) in
    let sol_file = F.out_from_string in_path `Solution in
    let fmt = F.fmt_of_file sol_file in
    F.add_requires fmt [F.md_of in_path `Elaboration; F.md_of_path !F.theory];
    let print_rule e =
      match mk_entry e with
      | None -> ()
      | Some name ->
        let sol = model (find name) in
        let rhs = U.term_of_univ sol in
        (* Solution is translated back to the original theory *)
        let rhs' = Dkmeta.mk_term meta rhs in
        (* Solution is encoded as rewrite rules to make the files type check. *)
        Format.fprintf fmt "[] %a --> %a.@." Pp.print_name name Pp.print_term rhs'
    in
    List.iter print_rule entries;
    F.close_in elab_file;
    F.close_out sol_file

  let solve mk_theory =
    let meta = {Dkmeta.default_config with sg = sg} in
    let normalize : U.pred -> U.pred = fun p ->
      U.extract_pred (Dkmeta.mk_term meta (U.term_of_pred p))
    in
    L.log_solver "[NORMALIZE CONSTRAINTS...]";
    let sp' = SP.map normalize !sp in
    L.log_solver "[NORMALIZE DONE]";
    SP.iter (fun p -> S.add (Pred p)) sp';
    S.solve mk_theory

end
