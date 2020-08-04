module B = Kernel.Basic
module E = Parsers.Entry
module F = Common.Files
module L = Common.Logic
module M = Dkmeta
module O = Common.Oracle
module P = Parsers.Parser
module R = Kernel.Rule
module S = Kernel.Signature
module T = Kernel.Term
module U = Common.Universes

(** The path that contains the configuration file *)
let config_path = ref "universo_cfg.dk"

let config = Hashtbl.create 11

type cmd_error =
  | ConfigurationFileNotFound of string
  | NoTargetSpecification
  | WrongConfiguration of E.entry
  | NoOutputSection
  | NoElaborationSection
  | Misc of string

exception Cmd_error of cmd_error

let sections =
  [
    "elaboration";
    "output";
    "constraints";
    "solver";
    "lra_specification";
    "qfuf_specification";
    "end";
  ]

let parse_config : unit -> unit =
 fun () ->
  let module P = struct
    type t = unit

    let section = ref ""

    let parameters = ref []

    let handle_entry env =
      let (module Printer) = Api.Env.get_printer env in
      let check_section s = List.mem s sections in
      function
      | E.Decl (_, id, _, _, _) when check_section (B.string_of_ident id) ->
          if !section <> "" then (
            Hashtbl.add config !section (List.rev !parameters);
            parameters := [] );
          section := B.string_of_ident id
      | E.Rules (_, rs) -> parameters := rs @ !parameters
      | _ as e ->
          raise
          @@ Cmd_error
               (Misc
                  (Format.asprintf
                     "Configuration file (entry not recognized): %a"
                     Printer.print_entry e))

    let get_data _ = ()
  end in
  Api.Processor.T.handle_files [ !config_path ] (module P)

let elaboration_meta_cfg : unit -> M.cfg =
 fun () ->
  let rules =
    try Hashtbl.find config "elaboration"
    with _ -> raise @@ Cmd_error NoElaborationSection
  in
  M.meta_of_rules rules M.default_config

let output_meta_cfg : unit -> M.cfg =
 fun () ->
  let rules =
    try Hashtbl.find config "output"
    with _ -> raise @@ Cmd_error NoOutputSection
  in
  M.meta_of_rules rules M.default_config

let mk_constraints : unit -> (B.name, U.pred) Hashtbl.t =
 fun () ->
  let table = Hashtbl.create 11 in
  let mk_rule : R.partially_typed_rule -> unit =
   fun r ->
    let name =
      match r.pat with
      | R.Pattern (_, name, []) -> name
      | _ -> failwith "Constraints are not in correct format"
    in
    (* let pred = try U.extract_pred (M.mk_term meta r.rhs) *)
    let pred =
      try U.extract_pred r.rhs
      with U.Not_pred -> failwith "Constraints are not in correct format"
    in
    Hashtbl.add table name pred
  in
  (try List.iter mk_rule (Hashtbl.find config "constraints") with _ -> ());
  table

(** [add_rules sg rs] add the rewrite rules [rs] to the signature [sg] *)
let add_rules sg rs =
  (* Several rules might be bound to different constant *)
  let add_rule sg r = S.add_rules sg [ R.to_rule_infos r ] in
  List.iter (add_rule sg) rs

(** [to_elaboration_env f] generates a fresh environement to elaborate file [f]. *)
let to_elaboration_env : F.path -> Elaboration.Elaborate.t =
 fun in_path ->
  let file = F.out_from_string in_path `Elaboration in
  { file; meta = elaboration_meta_cfg () }

(** [mk_theory ()] returns the theory used by universo. *)
let mk_theory : unit -> S.t =
 fun () ->
  Api.Processor.(
    handle_files [ F.get_theory () ] Api.Processor.SignatureBuilder)

(* (\** [elab_signature f] returns the signature containing all the universes declaration associated to *)
(*     file [f] *\) *)
(* let elab_signature : string -> S.t = fun in_path -> *)
(*   F.signature_of_file (F.get_out_path in_path `Elaboration) *)

(** [to_checking_env f] returns the type checking environement for the file [f] *)
let to_checking_env : string -> Checking.Checker.t =
 fun in_path ->
  (* FIXME: UGLY, rework to match the new API *)
  let out = F.get_out_path in_path `Output in
  let env = Api.Env.init (P.input_from_file out) in
  let constraints = mk_constraints () in
  let out_file = F.out_from_string in_path `Checking in
  { env; in_path; meta_out = output_meta_cfg (); constraints; out_file }

(** [theory_meta f] returns the meta configuration that allows to elaborate a theory for the SMT solver *)
let mk_smt_theory : unit -> int -> O.theory =
 fun () ->
  try
    let rules = Hashtbl.find config "qfuf_specification" in
    let meta = M.meta_of_rules rules (output_meta_cfg ()) in
    O.mk_theory meta
  with Not_found -> raise @@ Cmd_error NoTargetSpecification

let find_predicate s r =
  match r.R.pat with
  | Pattern (_, n', _) -> B.string_of_ident (B.id n') = s
  | _ -> false

let get_lra_specification_config : string -> string list * T.term =
 fun s ->
  try
    let rs = Hashtbl.find config "lra_specification" in
    let r = List.find (find_predicate s) rs in
    let to_string = function
      | R.Var (_, id, _, _) -> B.string_of_ident id
      | _ -> assert false
    in
    match r.pat with
    | R.Pattern (_, _, l) -> (List.map to_string l, r.rhs)
    | _ -> assert false
  with _ -> raise @@ Cmd_error (Misc "Wrong solver specification")

let mk_lra_reification : unit -> (module L.LRA_REIFICATION) =
 fun () ->
  ( module struct
    let axiom_specification = get_lra_specification_config "axiom"

    let rule_specification = get_lra_specification_config "rule"

    let cumul_specification = get_lra_specification_config "cumul"
  end )

let mk_solver : unit -> (module Solving.Utils.SOLVER) * Solving.Utils.env =
 fun () ->
  let open Solving in
  let get_rhs (r : R.partially_typed_rule) =
    match r.rhs with
    | T.Const (_, n) -> B.string_of_ident (B.id n)
    | _ -> raise @@ Cmd_error (WrongConfiguration (E.Rules (B.dloc, [ r ])))
  in
  let find_lhs opt r =
    match r.R.pat with
    | Pattern (_, n, _) -> B.string_of_ident (B.id n) = opt
    | _ -> false
  in
  let options = try Hashtbl.find config "solver" with _ -> [] in
  let find key default =
    try get_rhs @@ List.find (find_lhs key) options with _ -> default
  in
  let smt = find "smt" "z3" in
  let logic = find "logic" "qfuf" in
  let opt = find "opt" "uf" in
  let (module SS : Utils.SMTSOLVER) =
    if smt = "z3" then
      let open Z3cfg in
      if logic = "lra" then
        let (module R : L.LRA_REIFICATION) = mk_lra_reification () in
        (module Make (Arith (L.MakeLraSpecif (R))))
      else if logic = "qfuf" then (module Make (Syn))
      else raise @@ Cmd_error (Misc "Wrong solver specification: logic")
    else raise @@ Cmd_error (Misc "Wrong solver specification: smt")
  in
  let (module S : Utils.SOLVER) =
    if opt = "uf" then (module Solver.MakeUF (SS))
    else if opt = "normal" then (module Solver.Make (SS))
    else raise @@ Cmd_error (Misc "Wrong solver specification: opt")
  in
  let open Utils in
  let min = int_of_string (find "minimum" "1") in
  let max = int_of_string (find "maximum" "6") in
  let print = find "print" "false" = "true" in
  let mk_theory = mk_smt_theory () in
  let env = { min; max; print; mk_theory } in
  ((module S : Utils.SOLVER), env)
