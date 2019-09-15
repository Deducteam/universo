include    Common.Import
module B = Basic
module F = Common.Files
module L = Common.Logic
module O = Common.Oracle
module U = Common.Universes

(** The path that contains the configuration file *)
let config_path = ref ""

let config = Hashtbl.create 11

type cmd_error =
  | NoTargetSpecification
  | WrongConfiguration of Entry.entry
  | NoOutputSection
  | NoElaborationSection
  | Misc of string

exception Cmd_error of cmd_error

let sections =
  ["elaboration"
  ;"output"
  ;"constraints"
  ; "solver"
  ; "lra_specification"
  ; "qfuf_specification"
  ; "end"]

let parse_config : unit -> unit = fun () ->
  let ic = open_in !config_path in
  let md = Basic.mk_mident !config_path in
  let section = ref "" in
  let parameters = ref [] in
  let mk_entry e =
    let open Entry in
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
    | _ -> raise @@
      Cmd_error( Misc(Format.asprintf
                        "Configuration file (entry not recognized): %a" Pp.print_entry e))
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
  let theory = F.get_theory () in
  let entries = Parser.Parse_channel.parse theory.md (F.in_channel_of_file theory) in
  let _ = EE.init theory.path in
  List.iter SB.handle_entry entries;
  SB.get_data ()

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
let mk_theory : unit -> int -> O.theory = fun () ->
  try
    let rules = Hashtbl.find config "qfuf_specification" in
    let meta = Dkmeta.meta_of_rules rules (output_meta_cfg ()) in
    O.mk_theory meta
  with Not_found -> raise @@ Cmd_error NoTargetSpecification


let find_predicate s r =
  let open Rule in
  match r.pat with
  | Pattern(_,n',_) -> Basic.string_of_ident (Basic.id n') = s
  | _ -> false

let get_lra_specification_config : string -> string list * Term.term = fun s ->
  try
    let rs = Hashtbl.find config "lra_specification" in
    let r = List.find (find_predicate s) rs in
    let to_string = function
      | Rule.Var(_,id,_,_) -> Basic.string_of_ident id
      | _ -> assert false
    in
    match r.pat with
    | Rule.Pattern(_,_,l) -> List.map to_string l,r.rhs
    | _ -> assert false
  with _ -> raise @@ Cmd_error (Misc ("Wrong solver specification"))

let mk_lra_reification : unit -> (module L.LRA_REIFICATION) = fun () ->
  (module (struct
            let axiom_specification = get_lra_specification_config "axiom"
            let rule_specification = get_lra_specification_config   "rule"
            let cumul_specification = get_lra_specification_config "cumul"
          end))

let mk_solver : unit -> (module Solving.Utils.SOLVER) * Solving.Utils.env = fun () ->
  let open Solving in
  let get_rhs (r : Rule.untyped_rule) =
    let open Rule in
    match r.rhs with
    | Term.Const(_,n) -> Basic.string_of_ident (Basic.id n)
    | _ -> raise @@ Cmd_error (WrongConfiguration(Entry.Rules(Basic.dloc,[r])))
  in
  let find_lhs opt r =
    let open Rule in
    match r.pat with
    | Pattern(_,n,_) -> Basic.string_of_ident (Basic.id n) = opt
    | _ -> false
  in
  let options = try Hashtbl.find config "solver" with _ -> [] in
  let find key default = try get_rhs @@ List.find (find_lhs key)   options with _ -> default  in
  let smt     = find "smt" "z3" in
  let logic   = find "logic" "qfuf" in
  let opt     = find "opt" "uf" in
  let (module SS) : (module Utils.SMTSOLVER) =
    if smt = "z3" then
      begin
        let open Z3cfg in
        if logic = "lra" then
          let (module R:L.LRA_REIFICATION) = mk_lra_reification () in
          (module Make(Arith(L.MakeLraSpecif(R))))
        else if logic = "qfuf" then
          (module Make(Syn))
        else
         raise @@ Cmd_error (Misc ("Wrong solver specification: logic"))
      end
    else
      raise @@ Cmd_error (Misc ("Wrong solver specification: smt"))
  in
  let (module S : Utils.SOLVER) =
    if opt = "uf" then
      (module Solver.MakeUF(SS))
    else if opt = "normal" then
      (module Solver.Make(SS))
    else
      raise @@ Cmd_error (Misc ("Wrong solver specification: opt"))
  in
  let open Utils in
  let min         = int_of_string (find "minimum" "1") in
  let max         = int_of_string (find "maximum" "6") in
  let print       = find "print" "false" = "true" in
  let mk_theory   = mk_theory () in
  let env = {min;max;print;mk_theory} in
  (module S:Utils.SOLVER), env
