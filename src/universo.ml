module B = Kernel.Basic
(** Main file *)

module C = Common.Constraints
module E = Elaboration.Elaborate
module F = Common.Files
module L = Common.Log
module M = Meta.Dkmeta
module P = Parsers.Parser
module S = Kernel.Signature
module O = Common.Oracle
module U = Common.Universes

let _ =
  (* Dedukti option to avoid problems with signatures and rewriting on static symbols. *)
  (* TODO: This should be a matter of dkmeta not Universo *)
  S.fail_on_symbol_not_found := false

(** Direct the control flow of Universo. The control flow of Universo can be sum up in 4 steps:
    1) Elaborate the files to replace universes by variables
    2) Check the files to generate constraints
    3) Solve the constraints
    4) Reconstruct the files with the solution *)
type execution_mode =
  | Normal  (** Go through the four steps *)
  | JustElaborate  (** Do not generate constraints (only step 1). *)
  | JustCheck
      (** Only generate constraints (only step 2). ASSUME that elaboration has been done before. *)
  | JustSolve
      (** Only solve the constraints (only step 3). ASSUME that constraints has been generated before. *)
  | Simplify

(** By default, Universo go through all the steps *)
let mode = ref Normal

(** [elaborate file] generates two new files [file'] and [file_univ].
    [file'] is the same as [file] except that all universes are replaced by fresh variables.
    [file_univ] contains the declaration of these variables. Everything is done modulo the logic. *)
let elaborate : string -> unit =
 fun in_path ->
  L.log_univ "[ELAB] %s" (F.get_out_path in_path `Elaboration);
  let in_file = F.get_out_path in_path `Input in
  let env = Cmd.to_elaboration_env in_file in
  let entries = P.parse (P.input_from_file in_file) in
  (* This steps generates the fresh universe variables *)
  let entries' = List.map (E.mk_entry env) entries in
  (* Write the elaborated terms in the normal file (in the output directory) *)
  let out_file = F.out_from_string in_path `Output in
  let out_fmt = F.fmt_of_file out_file in
  (* The elaborated file depends on the out_sol_md file that contains solution. If the mode is JustElaborate, then this file is empty and import the declaration of the fresh universes *)
  F.add_requires out_fmt
    [ F.md_of in_path `Elaboration; F.md_of in_path `Solution ];
  List.iter (Api.Pp.Default.print_entry out_fmt) entries';
  F.close out_file;
  F.close env.file;
  F.export in_path `Elaboration

(** [check file] type checks the file [file] and write the generated constraints in the file [file_cstr]. ASSUME that [file_univ] has been generated previously.
    ASSUME also that the dependencies have been type checked before. *)
let check : string -> unit =
 fun in_path ->
  L.log_univ "[CHECKING] %s" (F.get_out_path in_path `Output);
  let file = F.get_out_path in_path `Output in
  let universo_env = Cmd.to_checking_env in_path in
  let requires_mds =
    let deps = C.get_deps () in
    let elab_dep = F.md_of in_path `Elaboration in
    if List.mem elab_dep deps then deps else elab_dep :: deps
  in
  F.add_requires (F.fmt_of_file universo_env.out_file) requires_mds;
  (* TODO: this should be internalize in dkmeta, if universo needs it there is an API problem with dkmeta *)
  (* For this step, we want the real type checker *)
  Kernel.Signature.fail_on_symbol_not_found := true;
  let module P = struct
    type t = unit

    let handle_entry env entry =
      Checking.Checker.mk_entry universo_env env entry

    let get_data _ = ()
  end in
  let hook =
    Api.Processor.
      {
        before =
          (fun env ->
            let sg = Api.Env.get_signature env in
            S.import_signature sg (Cmd.mk_theory ()));
        after =
          (fun _ -> function
            | None -> ()
            | Some (env, lc, exn) -> Api.Env.fail_env_error env lc exn);
      }
  in
  Api.Processor.T.handle_files ~hook [ file ] (module P);
  S.fail_on_symbol_not_found := false;
  Api.Env.export universo_env.env;
  C.flush ();
  F.close universo_env.out_file;
  F.export in_path `Checking;
  F.export in_path `Solution;
  F.export in_path `Output

(** [solve files] call a SMT solver on the constraints generated for all the files [files].
    ASSUME that [file_cstr] and [file_univ] have been generated for all [file] in [files]. *)
let solve : string list -> unit =
 fun in_paths ->
  let (module S : Solving.Utils.SOLVER), env = Cmd.mk_solver () in
  let add_constraints in_path =
    L.log_univ "[PARSE] %s" (F.get_out_path in_path `Checking);
    S.parse in_path
  in
  List.iter add_constraints in_paths;
  L.log_univ "[SOLVING CONSTRAINTS...]";
  let i, model = S.solve env in
  L.log_univ "[SOLVED] Solution found with %d universes." i;
  S.print_model (Cmd.output_meta_cfg ()) model in_paths

let simplify : string list -> unit =
 fun in_paths ->
  B.Debug.enable_flag M.debug_flag;
  let normalize_file out_cfg in_path =
    let meta =
      let path = F.get_out_path in_path `Solution in
      M.meta_of_files ~cfg:out_cfg [ path ]
    in
    let file = F.get_out_path in_path `Output in
    let input = P.input_from_file file in
    let env = Api.Env.init input in
    let output = F.out_from_string in_path `Simplify in
    let fmt = F.fmt_of_file output in
    let mk_entry e =
      match e with
      | Parsers.Entry.Require (_, _) -> ()
      | e ->
          Format.fprintf fmt "%a@." Api.Pp.Default.print_entry
            (M.mk_entry env meta e)
    in
    P.handle input mk_entry;
    P.close input;
    F.close output
  in
  let out_cfg = Cmd.output_meta_cfg () in
  List.iter (normalize_file out_cfg) in_paths

(** [run_on_file file] process steps 1 and 2 (depending the mode selected on [file] *)
let run_on_file file =
  match !mode with
  | Normal ->
      elaborate file;
      check file
  | JustElaborate -> elaborate file
  | JustCheck -> check file
  | JustSolve -> ()
  | Simplify -> ()

let cmd_options =
  [
    ( "-o",
      Arg.String
        (fun s ->
          F.mk_dir F.output_directory s;
          Api.Files.add_path s),
      " (MANDATORY) Set the output directory" );
    ( "--theory",
      Arg.String
        (fun s ->
          F.mk_theory s;
          U.md_theory := P.md_of_file s),
      " (MANDATORY) Theory file" );
    ( "--config",
      Arg.String (fun s -> Cmd.config_path := s),
      " (MANDATORY) Configuration file" );
    ( "-l",
      Arg.Unit (fun () -> L.enable_universo_flag ()),
      " Active the debug flag specific to universo" );
    ( "-d",
      Arg.String L.enable_flag,
      " flags enables debugging for all given flags" );
    ( "--elab-only",
      Arg.Unit (fun _ -> mode := JustElaborate),
      " only elaborate files" );
    ( "--check-only",
      Arg.Unit (fun _ -> mode := JustCheck),
      " only generate constraints" );
    ( "--solve-only",
      Arg.Unit (fun _ -> mode := JustSolve),
      " only solves the constraints" );
    ( "--simplify",
      Arg.String
        (fun s ->
          mode := Simplify;
          F.mk_dir F.simplify_directory s),
      " output is simplified so that only usual dk files remain" );
    ( "-I",
      Arg.String Api.Files.add_path,
      " DIR Add the directory DIR to the load path" );
  ]

(** [generate_empty_sol_file file] generates the file [file_sol] that requires the file [file_univ].
    This is necessary when universo is used with another mode than the Normal one (see elaboration). *)
let generate_empty_sol_file : string -> unit =
 fun in_path ->
  let sol_file = F.get_out_path in_path `Solution in
  let check_md = P.md_of_file (F.get_out_path in_path `Checking) in
  let oc = open_out sol_file in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "#REQUIRE %a.@.@." Api.Pp.Default.print_mident check_md;
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
    if !mode <> Simplify then List.iter generate_empty_sol_file files;
    List.iter run_on_file files;
    if !mode = Normal || !mode = JustSolve then solve files;
    if !mode = Simplify then simplify files
  with
  (* | Env.EnvError(md,l,e) -> Errors.fail_env_error(md,l,e)
   * | Signature.SignatureError e ->
   *    Errors.fail_env_error(None,Basic.dloc, Env.EnvErrorSignature e)
   * | Typing.TypingError e ->
   *   Errors.fail_env_error(None,Basic.dloc, Env.EnvErrorType e) *)
  | Cmd.Cmd_error (Misc s) ->
      Api.Errors.fail_exit ~code:"-1" ~file:"" None "%s@." s
  | Sys_error err ->
      Format.eprintf "ERROR %s.@." err;
      exit 1
  | Exit -> exit 3
  | Solving.Utils.NoSolution ->
      L.error "Universo found no solution";
      exit 2
