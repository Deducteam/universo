open Parser
open Basic
open Cic

let mk_entry md e =
  let e = Uvar.Elaboration.elaboration_entry e in
  ignore(Constraints.Basic.generate e);
  Indent.indent_entry e

let run_on_file export file =
  let input = open_in file in
  debug 1 "Processing file '%s'..." file;
  let md = mk_mident file in
  Env.init md;
  Parser.handle_channel md (mk_entry md) input;
  Errors.success "File '%s' was successfully checked." file;
  if export && not (Env.export ()) then
    Errors.fail dloc "Fail to export module '%a@." pp_mident (Env.get_name ());
  close_in input



let _ =
  let export = ref false in
  let options = Arg.align
      [ ( "-d"
        , Arg.Int Basic.set_debug_mode
        , "N sets the debuging level to N" )
      ; ( "-e"
      , Arg.Set export
        , " Generates an object file (\".dko\")" )
      ; ( "--prop"
      , Arg.Set Uvar.Elaboration.prop_elaboration
      , " Turn Prop universes to variables" ) ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  Rule.allow_non_linear := true;
  try
    List.iter (run_on_file !export) files;
  with
  | Sys_error err -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit          -> exit 3
