module P = Parser.Parse_channel
(*
type t =
  {
    uvar           : (module Uvar.S);
    elaboration    : (module Elaboration.S);
    checker        : (module Uconv.S);
    sg_univ_theory : Signature.t;
    output_dir     : string
  }

let universo : Signature.t =
  let open Entry in
  let file = "encodings/universo.dk" in
  let ic = open_in file in
  let md = Env.init file in
  let entries = Parser.Parse_channel.parse md ic in
  let mk_entry = function
    | Decl(lc,id,st,ty) -> Env.declare lc id st ty
    | Def(lc,id,opaque,ty,te) -> Env.define lc id opaque te ty
    | _ -> assert false
  in
  List.iter mk_entry entries;
  Env.get_signature ()

let mk_elab_entry : t -> Configuration.t -> Entry.entry -> Entry.entry = fun env cfg e ->
  let open Configuration in
  let (module E:Elaboration.S) = (module (val env.elaboration)) in
  E.elab_entry cfg e

let mk_check_entry : t -> Configuration.t ->Entry.entry -> unit = fun env cfg e ->
  let (module C:Uconv.S) = (module (val env.checker)) in
  C.mk_entry cfg e

let run_on_file (env:t) file =
  let open Configuration in
  let ic = open_in file in
  let md_check = Env.init file in
  let uf = (env.output_dir ^ "/" ^ (string_of_mident md_check) ^ "_univ.dk") in
  let oc_univ  = open_out uf in
  let md_univ  = mk_mident (string_of_mident md_check ^"_univ") in
  let sg_univ  = Signature.make (string_of_mident md_univ) in
  let sg_check = Env.get_signature () in
  Signature.import_signature sg_check (Dkmeta.LF.signature);
  Signature.import_signature sg_check env.sg_univ_theory;
  let cfg = {
    md_univ;
    oc_univ;
    sg_univ;
    md_check;
    sg_check;
  }
  in
  let entries  = Parser.Parse_channel.parse md_check ic in
  let entries' = List.map (mk_elab_entry env cfg) entries in
  Signature.import_signature sg_check sg_univ;
  List.iter (mk_check_entry env cfg) entries';
  close_in ic

let _ =
  let compatibility_input_file = ref "" in
  let set_compatibility_input_file f =
    compatibility_input_file := f
  in
  let compatibility_theory_file = ref "" in
  let set_compatibility_theory_file f =
    compatibility_theory_file := f
  in
  let compatibility_output_file = ref "" in
  let set_compatibility_output_file f =
    compatibility_output_file := f
  in
  let output_dir = ref "" in
  let set_output_dir d =
    output_dir := d
  in
  let theory = "experiments/matita/theory/cic.dk" in
  let export = ref false in
  let options = Arg.align
      [ ( "-d"
        , Arg.String Env.set_debug_mode
        , " flags enables debugging for all given flags" )
      ; ( "-e"
      , Arg.Set export
        , " Generates an object file (\".dko\")" )
      ; ( "-errors-in-snf"
      , Arg.Set Errors.errors_in_snf
        , " Normalize the types in error messages" )
      ; ( "-I"
      , Arg.String Basic.add_path
      , " DIR Add the directory DIR to the load path" )
      ; ( "-o"
      , Arg.String set_output_dir
      , " Set the output directory" )
      ; ( "--theory"
        , Arg.String set_compatibility_theory_file
      , " Rewrite rules mapping theory's universes to Universo's universes" )
      ; ( "--in"
      , Arg.String set_compatibility_input_file
      , " Rewrite rules mapping theory's universes to Universo's universes (input)" )
      ; ( "--out"
      , Arg.String set_compatibility_output_file
      , " Rewrite rules mapping Universo's universes to the theory's universes (output)" )]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  let check _ _ _ = () in (* *)
  check !compatibility_input_file !compatibility_output_file !compatibility_theory_file;
  try
    let sg_of_theory theory =
      let ic = open_in theory in
      let md = Env.init theory in
      let entries = Parser.Parse_channel.parse md ic in
      let sg = Signature.make (string_of_mident md) in
      Dkmeta.to_signature md ~sg entries
    in
    let (module Ti : Theory.S) =
      Theory.from_file [sg_of_theory theory] true !compatibility_input_file in
    let (module To : Theory.S) =
      Theory.from_file [universo] false !compatibility_output_file in
    let (module Th : Theory.S) =
      Theory.from_file [sg_of_theory theory] true !compatibility_theory_file in
    let mk_theory sg theory =
      let ic = open_in theory in
      let md = Env.init theory in
      let entries  = Parser.Parse_channel.parse md ic in
      let entries' = List.map (Dkmeta.mk_entry Th.meta md) entries in
      let sg = Signature.make (string_of_mident md) in
      Signature.import_signature sg universo;
      Dkmeta.to_signature md ~sg entries'
    in
    let theory_universo = mk_theory (Signature.make (string_of_mident (Env.init theory))) theory in
    let (module U:Uvar.S) = (module Uvar.Make(To)) in
    let (module E:Elaboration.S) = (module Elaboration.Make(Ti)(U)) in
    let (module TYP:Uconv.S) = (module Uconv.Make(Th)) in
    let env =
      {
        uvar           = (module U);
        elaboration    = (module E);
        checker        = (module TYP);
        sg_univ_theory = theory_universo;
        output_dir     = !output_dir
      }
    in
    List.iter (run_on_file env) files;
  with
  | Env.EnvError(l,e) -> Errors.fail_env_error l e
  | Signature.SignatureError e ->
     Errors.fail_env_error Basic.dloc (Env.EnvErrorSignature e)
  | Typing.TypingError e ->
    Errors.fail_env_error Basic.dloc (Env.EnvErrorType e)
  | Sys_error err -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit          -> exit 3

*)
type execution_mode =
  | Normal
  | JustElaborate
  | JustCheck
  | JustSolve

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

let checking : string -> Basic.mident = fun _ -> failwith "todo"

let solve : string list -> solution = fun _ -> failwith "todo"

let reconstruction : solution -> string -> Basic.mident = fun _ _ -> failwith "todo"

let compile : string list -> unit = fun _ -> failwith "todo"

let run_on_file file =
  match !mode with
  | Normal ->
    elaborate file
  | _ -> failwith "todo modes"


let cmd_options =
  [ ( "-d"
    , Arg.String Env.set_debug_mode
    , " flags enables debugging for all given flags" )
  ; ( "-I"
    , Arg.String Basic.add_path
    , " DIR Add the directory DIR to the load path" )
  ; ( "-o"
    , Arg.String (fun s -> Files.output_directory := Some s)
    , " Set the output directory" )
  ; ( "--theory"
    , Arg.String (fun s -> Cmd.compat_theory := s)
    , " Rewrite rules mapping theory's universes to Universo's universes" )
  ; ( "--in"
    , Arg.String (fun s -> Cmd.compat_input := s)
    , " Rewrite rules mapping theory's universes to Universo's universes (input)" )
  ; ( "--out"
    , Arg.String  (fun s -> Cmd.compat_output := s)
    , " Rewrite rules mapping Universo's universes to the theory's universes (output)" )]

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
    List.iter run_on_file files;
  with
  | Env.EnvError(l,e) -> Errors.fail_env_error l e
  | Signature.SignatureError e ->
     Errors.fail_env_error Basic.dloc (Env.EnvErrorSignature e)
  | Typing.TypingError e ->
    Errors.fail_env_error Basic.dloc (Env.EnvErrorType e)
  | Sys_error err -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit          -> exit 3
