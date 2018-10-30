module P = Parser.Parse_channel

let _ = Errors.errors_in_snf := true

let export : string -> unit = fun file ->
  let ic = open_in file in
  let md = Files.md_of_file file in
  let entries = P.parse md ic in
  Signature.export (Dkmeta.to_signature md entries)

type execution_mode =
  | Normal
  | JustElaborate
  | JustCheck

type solution

let mode = Pervasives.ref Normal

let elaborate : string  -> unit = fun in_file ->
  let ic = open_in in_file in
  let md = Files.md_of_file in_file in
  let env = Cmd.to_elaboration_env in_file in
  let entries = P.parse md ic in
  let entries' = List.map (Elaboration.mk_entry env) entries in
  let out_file = open_out (Files.from_string in_file `Normal) in
  let out_fmt = Format.formatter_of_out_channel out_file in
  Format.fprintf out_fmt "#REQUIRE %a.@.@." Pp.print_mident env.out_md;
  List.iter (Pp.print_entry (Format.formatter_of_out_channel out_file)) entries'

let checking : string -> unit = fun in_file ->
  let md = Files.md_of_file in_file in
  let out_file = Files.from_string in_file `Normal in
  let ic = open_in out_file in
  let entries = P.parse md ic in
  let env = Cmd.to_checking_env in_file in
  let entries' = List.map (Dkmeta.mk_entry env.meta md) entries in
  List.iter (Checking.Checker.mk_entry env) entries'

let solve : string list -> unit = fun in_files ->
  let add_file in_file =
    let check_file = Files.from_string in_file `Checking in
    let md_elab  = Files.md_of_file (Files.from_string in_file `Elaboration) in
    let md_check = Files.md_of_file (Files.from_string in_file `Checking) in
    Solving.Solver.Z3Syn.parse md_elab md_check check_file
  in
  List.iter add_file in_files

let reconstruction : solution -> string -> Basic.mident = fun _ _ -> failwith "todo"

let compile : string list -> unit = fun _ -> failwith "todo"

let run_on_file file =
  match !mode with
  | Normal ->
    elaborate file;
    checking file
  | JustElaborate ->
    elaborate file
  | JustCheck ->
    checking file

(* FIXME: find better names for options *)
let cmd_options =
  [ ( "-d"
    , Arg.String Env.set_debug_mode
    , " flags enables debugging for all given flags" )
  ; ( "-I"
    , Arg.String Basic.add_path
    , " DIR Add the directory DIR to the load path" )
  ; ( "-o"
    , Arg.String (fun s -> Files.output_directory := Some s; Basic.add_path s)
    , " Set the output directory" )
  ; ( "-theory"
    , Arg.String (fun s -> Cmd.theory := s)
    , " Theory file" )
  ; ( "--theory"
    , Arg.String (fun s -> Cmd.compat_theory := s)
    , " Rewrite rules mapping theory's universes to Universo's universes" )
  ; ( "--in"
    , Arg.String (fun s -> Cmd.compat_input := s)
    , " Rewrite rules mapping theory's universes to Universo's universes (input)" )
  ; ( "--out"
    , Arg.String  (fun s -> Cmd.compat_output := s)
    , " Rewrite rules mapping Universo's universes to the theory's universes (output)" )]

module S = Solving.Solver

let _ =
  try
    let options = Arg.align cmd_options in
    let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
    let usage = usage ^ "Available options:" in
    let files =
      let files = ref [] in
      Arg.parse options (fun f -> files := f :: !files) usage;
      List.rev !files
    in
    List.iter run_on_file files
  with
  | Env.EnvError(l,e) -> Errors.fail_env_error l e
  | Signature.SignatureError e ->
     Errors.fail_env_error Basic.dloc (Env.EnvErrorSignature e)
  | Typing.TypingError e ->
    Errors.fail_env_error Basic.dloc (Env.EnvErrorType e)
  | Sys_error err -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit          -> exit 3
