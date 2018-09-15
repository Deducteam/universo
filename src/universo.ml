open Basic
open Meta
open Elaboration

let elab_entry (module Elab:Elaboration.S) sg e =
  let e =  Elab.elab_entry sg e in
  Format.printf "%a@." Pp.print_entry e

let compatibility_file = ref ""
let set_compatibility_file f =
  compatibility_file := f

let run_on_file (module Elab:Elaboration.S)file =
  let ic = open_in file in
  let md = Env.init file in
  let md_univ = "univ" ^ (string_of_mident md) in
  let sg = Signature.make  md_univ in
  Signature.import sg Basic.dloc (mk_mident "universo");
  let entries  = Parser.parse_channel md ic in
  let entries' = List.map (elab_entry (module Elab) sg) entries in
  ignore(entries');
  close_in ic

let run_on_compatibility_file file : (module Theory.Compat) =
  let ic = open_in file in
  let md = Env.init file in
  let mk_entry = function
    | Entry.Rules(_,rs) -> rs
    | _ -> assert false
  in
  let entries = Parser.parse_channel md ic in
  let rules = List.fold_left (fun r e -> r@mk_entry e) [] entries in
  let (module T:Theory.Compat) =
  match rules with
  | [] -> assert false
  (* the first rule is assumed to be the one that rewrite the Sort type *)
  | r::rs ->

    (module struct
       let sorts = [r]
       let constructors = rs
     end)
  in
  (module T)

let _ =
  let export = ref false in
  let options = Arg.align
      [ ( "-d"
        , Arg.String Env.set_debug_mode
        , "N sets the debuging level to N" )
      ; ( "-e"
      , Arg.Set export
        , " Generates an object file (\".dko\")" )
      ; ( "-errors-in-snf"
      , Arg.Set Errors.errors_in_snf
        , " Normalize the types in error messages" )
      ; ( "-I"
      , Arg.String Basic.add_path
      , " DIR Add the directory DIR to the load path" )
      ; ( "--compat"
      , Arg.String set_compatibility_file
      , " Normalize the types in error messages" )]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  let (module Compat:Theory.Compat) =
    if !compatibility_file = "" then
      (
        Debug.debug Debug.D_warn "No compatibility file given.";
        (module struct let sorts = [] let constructors = [] end)
      )
    else
      run_on_compatibility_file !compatibility_file
  in
  let (module Elab:Elaboration.S) = (module Elaboration.Make(Compat)(Uvar.Uvar)) in
  try
    List.iter (run_on_file (module Elab)) files;
  with
  | Env.EnvError(l,e) -> Errors.fail_env_error l e
  | Signature.SignatureError e ->
    Errors.fail_env_error Basic.dloc (Env.EnvErrorSignature e)
  | Sys_error err -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit          -> exit 3
