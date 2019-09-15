include    Common.Import
module F = Common.Files
module L = Common.Log
module O = Common.Oracle
module U = Common.Universes

open Utils

(** [from_rule pat rhs] add the assertion [pat = rhs]. *)
let from_rule : Rule.pattern -> Term.term -> U.cstr = fun pat right ->
  let left   = Rule.pattern_to_term pat in
  try (* the constraint is a predicate *)
    U.Pred (U.extract_pred left)
  with U.Not_pred ->
    (* the constraint is en equality between variables *)
    let left' = Elaboration.Var.name_of_uvar left in
    let right' = Elaboration.Var.name_of_uvar right in
    U.EqVar(left',right')

module Make(S:SMTSOLVER) : SOLVER =
struct

  (** [parse meta s] parses a constraint file. *)
  let parse : string -> unit =
    fun in_path ->
      let md_check = F.md_of in_path `Checking in
      let mk_entry = function
        | Entry.Rules(_,rs) ->
          List.map (fun (r:Rule.untyped_rule) -> from_rule r.pat r.rhs) rs
        | Entry.Require _ -> []
        | _ -> assert false
      in
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
    let theory = F.get_theory () in
    F.add_requires fmt [F.md_of in_path `Elaboration; theory.F.md];
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
    F.close elab_file;
    F.close sol_file

  let solve = S.solve
end

(** Performance are bad with LRA *)
module MakeUF(S:SMTSOLVER) : SOLVER =
struct

  module SP = Set.Make(struct type t = U.pred let compare = compare end)

  let sg = Signature.make "solver"

  let sp = ref SP.empty

  let mk_rule : Rule.untyped_rule -> unit = fun r ->
    match from_rule r.pat r.rhs with
    | U.EqVar _ -> Signature.add_rules sg [(Rule.to_rule_infos r)]
    | U.Pred p -> sp := SP.add p !sp

  (** [parse meta s] parses a constraint file. *)
  let parse : string -> unit =
    fun in_path ->
      let md_check = F.md_of in_path `Checking in
      let mk_entry = function
        | Entry.Rules(_,rs) -> List.iter mk_rule rs
        | Entry.Require _ -> ()
        | _ -> assert false
      in
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
    let theory = F.get_theory () in
    F.add_requires fmt [F.md_of in_path `Elaboration; theory.F.md];
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
    F.close elab_file;
    F.close sol_file

  let solve env =
    let meta = {Dkmeta.default_config with sg = sg} in
    let normalize : U.pred -> U.pred = fun p ->
      U.extract_pred (Dkmeta.mk_term meta (U.term_of_pred p))
    in
    L.log_solver "[NORMALIZE CONSTRAINTS...]";
    let sp' = SP.map normalize !sp in
    L.log_solver "[NORMALIZE DONE]";
    SP.iter (fun p -> S.add (Pred p)) sp';
    S.solve env

end
