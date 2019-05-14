(** Main file *)

module C = Common.Constraints
module E = Elaboration.Elaborate
module F = Common.Files
module L = Common.Log
module P = Parser.Parse_channel
module O = Common.Oracle
module U = Common.Universes

module Syn = Solving.Solver.Z3Syn
module Arith = Solving.Solver.Z3Arith
module ZSyn = Solving.Z3cfg.Make(Syn)
module ZArith = Solving.Z3cfg.Make(Arith)
module S = Solving.Solver.Make(ZSyn)

let _ =
  (* For debugging purposes, it is better to see error messages in SNF *)
  Errors.errors_in_snf := false;
  (* Dedukti option to avoid problems with signatures and rewriting on static symbols. *)
  Signature.unsafe := true

(** Direct the control flow of Universo. The control flow of Universo can be sum up in 4 steps:
    1) Elaborate the files to replace universes by variables
    2) Check the files to generate constraints
    3) Solve the constraints
    4) Reconstruct the files with the solution *)
type execution_mode =
  | Normal (** Go through the four steps *)
  | JustElaborate (** Do not generate constraints (only step 1). *)
  | JustCheck (** Only generate constraints (only step 2). ASSUME that elaboration has been done before. *)
  | JustSolve (** Only solve the constraints (only step 3). ASSUME that constraints has been generated before. *)

(** By default, Universo go through all the steps *)
let mode = Pervasives.ref Normal

(** [elaborate file] generates two new files [file'] and [file_univ].
    [file'] is the same as [file] except that all universes are replaced by fresh variables.
    [file_univ] contains the declaration of these variables. Everything is done modulo the logic. *)
let elaborate : string  -> unit = fun in_path ->
  let in_file = F.in_from_string in_path `Input in
  let env = Cmd.to_elaboration_env in_file.path in
  let entries = P.parse in_file.md (F.in_channel_of_file in_file) in
  (* This steps generates the fresh universe variables *)
  let entries' = List.map (E.mk_entry env) entries in
  (* Write the elaborated terms in the normal file (in the output directory) *)
  let out_file = F.out_from_string in_path `Output in
  let out_fmt = F.fmt_of_file out_file in
  (* The elaborated file depends on the out_sol_md file that contains solution. If the mode is JustElaborate, then this file is empty and import the declaration of the fresh universes *)
  F.add_requires out_fmt [F.md_of in_path `Elaboration; F.md_of in_path `Solution];
  List.iter (Pp.print_entry out_fmt) entries';
  F.close_in in_file;
  F.close_out out_file;
  F.close_out env.file;
  F.export in_path `Elaboration

(** [check file] type checks the file [file] and write the generated constraints in the file [file_cstr]. ASSUME that [file_univ] has been generated previously.
    ASSUME also that the dependencies have been type checked before. *)
let check : string -> unit = fun in_path ->
  let md = F.md_of_path in_path in
  let file = F.in_from_string in_path `Output in
  let entries = P.parse md (F.in_channel_of_file file) in
  let env = Cmd.to_checking_env in_path in
  let requires_mds =
    let deps = C.get_deps () in
    let elab_dep = F.md_of in_path `Elaboration in
    if List.mem elab_dep deps then deps else elab_dep::deps
  in
  F.add_requires (F.fmt_of_file env.out_file) requires_mds;
  Signature.unsafe := false; (* For this step, we want the real type checker *)
  List.iter (Checking.Checker.mk_entry env) entries;
  Signature.unsafe := true;
  Signature.export env.sg;
  C.flush ();
  F.close_in file;
  F.close_out env.out_file;
  F.export in_path `Checking;
  F.export in_path `Solution;
  F.export in_path `Output

(** [solve files] call a SMT solver on the constraints generated for all the files [files].
    ASSUME that [file_cstr] and [file_univ] have been generated for all [file] in [files]. *)
let solve : string list -> unit = fun in_paths ->
  let add_constraints in_path =
    S.parse (Cmd.elaboration_meta_cfg ()) in_path
  in
  List.iter add_constraints in_paths;
  L.log_univ "[SOLVING CONSTRAINTS...]";
  let mk_theory i = O.mk_theory (Cmd.theory_meta ()) i in
  let i,model = S.solve mk_theory !C.predicative in
  L.log_univ "[SOLVED] Solution found with %d universes." i;
  List.iter (S.print_model (Cmd.output_meta_cfg ()) model) in_paths

(** [run_on_file file] process steps 1 and 2 (depending the mode selected on [file] *)
let run_on_file file =
  L.log_univ "[FILE] %s" file;
  match !mode with
  | Normal ->
    elaborate file;
    check file
  | JustElaborate ->
    elaborate file
  | JustCheck ->
    check file
  | JustSolve -> ()

let cmd_options =
  [ ( "-d"
    , Arg.String L.enable_flag
    , " flags enables debugging for all given flags" )
  ; ( "--elab-only"
    , Arg.Unit (fun _ -> mode := JustElaborate)
    , " only elaborate files" )
  ; ( "--check-only"
    , Arg.Unit (fun _ -> mode := JustCheck)
    , " only generate constraints" )
  ; ( "--solve-only"
    , Arg.Unit (fun _ -> mode := JustSolve)
    , " only solves the constraints" )
  ; ( "-I"
    , Arg.String Basic.add_path
    , " DIR Add the directory DIR to the load path" )
  ; ( "-o"
    , Arg.String (fun s -> F.output_directory := Some s; Basic.add_path s)
    , " Set the output directory" )
  ; ( "--theory"
    , Arg.String (fun s -> F.theory := s; U.md_theory := F.md_of_path s)
    , " Theory file" )
  ; ( "--config"
    , Arg.String  (fun s -> Cmd.config_path := s)
    , " Configuration file")
  ]

(** [generate_empty_sol_file file] generates the file [file_sol] that requires the file [file_univ].
    This is necessary when universo is used with another mode than the Normal one (see elaboration). *)
let generate_empty_sol_file : string -> unit = fun in_path ->
  let sol_file = F.get_out_path in_path `Solution in
  let check_md = F.md_of_path (F.get_out_path in_path `Checking) in
  let oc = open_out sol_file in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "#REQUIRE %a.@.@." Pp.print_mident check_md;
  close_out oc

let _ =
  try
    let options = Arg.align cmd_options in
    let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
    let usage = usage ^ "Available options:" in
    let files =
      let files = ref [] in
      Arg.parse options (fun f -> files := f :: !files) usage;
      Cmd.parse_config ();
      List.rev !files
    in
    List.iter generate_empty_sol_file files;
    List.iter run_on_file files;
    if !mode = Normal || !mode = JustSolve then
      solve files;
  with
  | Env.EnvError(l,e) -> Errors.fail_env_error l e
  | Signature.SignatureError e ->
     Errors.fail_env_error Basic.dloc (Env.EnvErrorSignature e)
  | Typing.TypingError e ->
    Errors.fail_env_error Basic.dloc (Env.EnvErrorType e)
  | Sys_error err -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit          -> exit 3
