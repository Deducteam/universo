open Parser
open Basic
open Cic

module CS = Constraints.Basic.CS

let mk_entry md cs (uvars,e) =
  let cs' = Constraints.Basic.generate uvars e in
  CS.union cs cs'

let run_on_file output export file =
  let input = open_in file in
  debug 1 "Processing file '%s'..." file;
  let md = mk_mident file in
  let fmt =
    if output = "" then Format.std_formatter
    else
      let oc = open_out (Filename.concat output file) in
      Format.formatter_of_out_channel oc
  in
  Env.init md;
  let entries = Parser.parse_channel md input in
  let entries_vars = List.map Uvar.Elaboration.elaboration_entry entries in
  (*  List.iter Indent.indent_entry entries; *)
  Errors.success "File '%s' was successfully parsed." file;
  let cs = List.fold_left (mk_entry md) CS.empty entries_vars in
  Errors.success "File '%s' was successfully checked." file;
  let model = Export.solve cs in
  Errors.success "Constraints solved.";
  let entries' = List.map (Reconstruction.entry_reconstruction model) entries in
  List.iter (Indent.indent_entry fmt) entries';
  if export && not (Env.export ()) then
    Errors.fail dloc "Fail to export module '%a@." pp_mident (Env.get_name ());
  close_in input



let _ =
  let export = ref false in
  let output_dir = ref "" in
  let set_output_dir s = output_dir := s in
  let options = Arg.align
      [ ( "-d"
        , Arg.Int Basic.set_debug_mode
        , "N sets the debuging level to N" )
      ; ( "-e"
      , Arg.Set export
        , " Generates an object file (\".dko\")" )
      ; ( "--prop"
      , Arg.Set Uvar.Elaboration.prop_elaboration
      , " Turn Prop universes to variables" )
      ; ( "--output-dir"
      , Arg.String set_output_dir
      , " Directory to print the files" ) ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  Rule.allow_non_linear := true;
  try
    List.iter (run_on_file !output_dir !export) files;
  with
  | Sys_error err -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit          -> exit 3
