module B = Basic
module F = Common.Files
module U = Common.Universes

(** The path that contains the configuration file *)
let config_path = ref ""

let config = Hashtbl.create 11

type cmd_error =
  | NoTargetSpecification
  | WrongConfiguration of Entry.entry
  | NoOutputSection
  | NoElaborationSection

exception Cmd_error of cmd_error

let parse_config : unit -> unit = fun () ->
  let ic = open_in !config_path in
  let md = Basic.mk_mident !config_path in
  let section = ref "" in
  let parameters = ref [] in
  let mk_entry e =
    let open Entry in
    let sections = ["elaboration";"target";"output";"constraints"; "solver"; "arith"; "end"] in
    let check_section s = List.mem s sections in
    match e with
    | Decl(_,id,_,_) when check_section (Basic.string_of_ident id) ->
      if !section <> "" then
        begin
          Hashtbl.add config !section (List.rev !parameters);
          parameters := []
        end;
      section := Basic.string_of_ident id
    | Rules(_,rs) ->
      parameters := rs @ !parameters
    | _ -> failwith (Format.asprintf "Configuration file (entry not recognized): %a" Pp.print_entry e)
  in
  Parser.Parse_channel.handle md mk_entry ic

let elaboration_meta_cfg : unit -> Dkmeta.cfg = fun () ->
 let rules = try Hashtbl.find config "elaboration" with _ -> raise @@ Cmd_error NoElaborationSection in
 Dkmeta.meta_of_rules rules Dkmeta.default_config

let output_meta_cfg : unit -> Dkmeta.cfg = fun () ->
  let rules = try Hashtbl.find config "output" with _ -> raise @@ Cmd_error NoOutputSection in
  Dkmeta.meta_of_rules rules Dkmeta.default_config


let mk_constraints : unit -> (B.name, U.pred) Hashtbl.t = fun () ->
  let table = Hashtbl.create 11 in
  let mk_rule : Rule.untyped_rule -> unit = fun r ->
    let open Rule in
    let name = match r.pat with
      | Rule.Pattern(_,name,[]) -> name
      | _ -> failwith "Constraints are not in correct format"
    in
    (* let pred = try U.extract_pred (Dkmeta.mk_term meta r.rhs) *)
    let pred = try U.extract_pred r.rhs
      with U.Not_pred -> failwith "Constraints are not in correct format"
    in
    Hashtbl.add table name pred
  in
  (try List.iter mk_rule (Hashtbl.find config "constraints") with _ -> ());
  table

(** [add_rules sg rs] add the rewrite rules [rs] to the signature [sg] *)
let add_rules sg rs =
  (* Several rules might be bound to different constant *)
  let add_rule sg r =
    Signature.add_rules sg [(Rule.to_rule_infos r)]
  in
  List.iter (add_rule sg) rs

(** [to_elaboration_env f] generates a fresh environement to elaborate file [f]. *)
let to_elaboration_env : F.path -> Elaboration.Elaborate.t = fun in_path ->
  let file = F.out_from_string in_path `Elaboration in
  {file; meta=elaboration_meta_cfg ()}

(** [mk_theory ()] returns the theory used by universo. *)
let mk_theory : unit -> Signature.t = fun () ->
  let ic = open_in !F.theory in
  let md = F.md_of_path !F.theory in
  let entries = Parser.Parse_channel.parse md ic in
  Entry.to_signature !F.theory entries

(** [elab_signature f] returns the signature containing all the universes declaration associated to
    file [f] *)
let elab_signature : string -> Signature.t = fun in_path ->
  F.signature_of_file (F.get_out_path in_path `Elaboration)

(** [to_checking_env f] returns the type checking environement for the file [f] *)
let to_checking_env : string -> Checking.Checker.t = fun in_path ->
  let theory_signature = mk_theory () in
  let sg = Signature.make (Filename.basename in_path) in
  Signature.import_signature sg theory_signature;
  Signature.import_signature sg (elab_signature in_path);
  let constraints = mk_constraints () in
  let out_file = F.out_from_string in_path `Checking in
  { sg; in_path; meta_out=output_meta_cfg (); constraints; out_file}

(** [theory_meta f] returns the meta configuration that allows to elaborate a theory for the SMT solver *)
let theory_meta : unit -> Dkmeta.cfg = fun () ->
  try
    let rules = Hashtbl.find config "target" in
    Dkmeta.meta_of_rules rules (output_meta_cfg ())
  with Not_found -> raise @@ Cmd_error NoTargetSpecification

let mk_solver : unit -> (module Solving.Solver.S) = fun () ->
  let open Solving in
  let _ : (module Solver.SOLVER) -> (module Solver.S) =
    if true then
      (fun (module S) -> (module Solver.MakeUF(S)))
    else
      (fun (module S) -> (module Solver.Make(S)))
  in (*
  let solver : (module Solver.S) =
    if true then
      (module ZSyn)
    else
      (module ZArith)
  in *)
  failwith "todo"
