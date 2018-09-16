open Basic
open Meta
open Elaboration
open Configuration

let universo : Signature.t =
  let open Entry in
  let file = "encodings/universo.dk" in
  let ic = open_in file in
  let md = Env.init file in
  let entries = Parser.parse_channel md ic in
  let mk_entry = function
    | Decl(lc,id,st,ty) -> Env.declare lc id st ty
    | Def(lc,id,opaque,ty,te) -> Env.define lc id opaque te ty
    | _ -> assert false
  in
  List.iter mk_entry entries;
  Env.get_signature ()

let elab_entry (module Elab:Elaboration.S) sg e =
  let e =  Elab.elab_entry sg e in
  Format.printf "%a@." Pp.print_entry e

let run_on_file (module Elab:Elaboration.S) file =
  let ic = open_in file in
  let oc = open_out (Filename.chop_extension file ^ "_univ.dk") in
  let md = Env.init file in
  let md_univ = (string_of_mident md) ^ "_univ" in
  let sg = Signature.make  md_univ in
  let ecfg = {sg;oc=Some oc} in
  Signature.import_signature sg universo;
  let entries  = Parser.parse_channel md ic in
  let entries' = List.map (elab_entry (module Elab) ecfg) entries in
  ignore(entries');
  close_in ic;
  close_out oc

let _ =
  let compatibility_input_file = ref "" in
  let set_compatibility_input_file f =
    compatibility_input_file := f
  in
  let compatibility_output_file = ref "" in
  let set_compatibility_output_file f =
    compatibility_output_file := f
  in
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
      ; ( "--in"
      , Arg.String set_compatibility_input_file
      , " Rewrite rules mapping theory's universes to Universo's universes" )
      ; ( "--out"
      , Arg.String set_compatibility_output_file
      , " Rewrite rules mapping Universo's universes to the theory's universes" )]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  let cfg = Configuration.mk_configuration !compatibility_input_file
                                           !compatibility_output_file
  in
  try
    List.iter (run_on_file (module (val cfg.elaboration))) files;
  with
  | Env.EnvError(l,e) -> Errors.fail_env_error l e
  | Signature.SignatureError e ->
    Errors.fail_env_error Basic.dloc (Env.EnvErrorSignature e)
  | Sys_error err -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit          -> exit 3
