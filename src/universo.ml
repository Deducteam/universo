open Parser
open Basic
open Cic

module CS = Constraints.Basic.CS

let mk_entry md cs e =
  let cs' = Constraints.Basic.generate e in
  CS.union cs cs'

let run_on_file export file =
  let input = open_in file in
  debug 1 "Processing file '%s'..." file;
  let md = mk_mident file in
  Env.init md;
  let entries = Parser.parse_channel md input in
  let entries = List.map Uvar.Elaboration.elaboration_entry entries in
  (*  List.iter Indent.indent_entry entries; *)
  Errors.success "File '%s' was successfully parsed." file;
  let cs = List.fold_left (mk_entry md) CS.empty entries in
  Errors.success "File '%s' was successfully checked." file;
  let model = Export.solve cs in
  Errors.success "Constraints solved.";
  let entries' = List.map (Reconstruction.entry_reconstruction model) entries in
  (* List.iter Indent.indent_entry entries'; *)
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
