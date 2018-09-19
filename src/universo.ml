open Basic
open Dkmeta
open Elaboration

let metaify rs = List.map Dkmeta.LF.encode_rule rs

let add_rule sg r =
  Signature.add_rules sg [(Rule.to_rule_infos r)]
(* Several rules might be bound to different constant *)
let add_rules sg rs = List.iter (add_rule sg) rs

type t =
  {
    uvar          : (module Uvar.S);
    theory_input  : (module Theory.In);
    theory        : (module Theory.Th);
    theory_output : (module Theory.Out);
    elaboration   : (module Elaboration.S);
    checker       : (module Uconv.S);
    sg            : Signature.t;
    sg_theory     : Signature.t;
    md_theory     : Basic.mident;
    output_dir    : string
  }

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

let mk_entry : t -> Configuration.t -> Entry.entry -> Entry.entry = fun env cfg e ->
  let open Configuration in
  let (module E:Elaboration.S) = (module (val env.elaboration)) in
  let (module C:Uconv.S) = (module (val env.checker)) in
  let e' =  E.elab_entry cfg e in
  Signature.import_signature cfg.sg cfg.sg_univ;
  let _ = C.mk_entry cfg e' in
  Format.printf "%a@." Pp.print_entry e';
  e'

let run_on_file (env:t) file =
  let open Configuration in
  let ic = open_in file in
  let md = Env.init file in
  let uf = (env.output_dir ^ "/" ^ (string_of_mident md) ^ "_univ.dk") in
  let oc_univ = open_out uf in
  let md_univ = mk_mident (string_of_mident md ^"_univ") in
  let sg_univ = Signature.make (string_of_mident md_univ) in
  let sg = Env.get_signature () in
  let (module TH:Theory.Th) = (module (val env.theory)) in
  Signature.import_signature sg (Dkmeta.LF.signature);
  Signature.import_signature sg env.sg_theory;
  add_rules sg (metaify TH.rules);
  let cfg = {
      md_theory = env.md_theory;
      sg_meta = env.sg;
      md = md;
      sg = sg;
      md_univ;
      sg_univ;
      oc_univ
    }
  in
  let entries  = Parser.parse_channel md ic in
  let entries' = List.map (mk_entry env cfg) entries in
  ignore(entries');
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
  let md_theory = ref (mk_mident "") in
  let set_md_theory s =
    md_theory := (mk_mident s)
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
      ; ( "--md-theory"
      , Arg.String set_md_theory
      , " Set the module of the theory" )
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
  try
    let (module TI:Theory.In) = Theory.from_file !compatibility_input_file in
    let (module TH:Theory.Th) = Theory.from_file !compatibility_theory_file in
    let (module TO:Theory.Out) = Theory.from_file !compatibility_output_file in
    let (module U:Uvar.S) = (module Uvar.Make(TO)) in
    let (module E:Elaboration.S) = (module Elaboration.Make(TI)(U)) in
    let (module TYP:Uconv.S) = (module Uconv.Make(TH)) in
    let mk_theory sg to_universo theory =
      let ic = open_in theory in
      let md = Env.init theory in
      let entries  = Parser.parse_channel md ic in
      let meta_cfg = Dkmeta.({default_config with encoding = Some (module LF); sg=sg}) in
      meta_cfg.meta_rules <- Some (List.map (fun (r:Rule.untyped_rule) -> r.Rule.name) to_universo);
      let entries' = List.map (Dkmeta.mk_entry meta_cfg md) entries in
      let sg = Signature.make (string_of_mident md) in
      Signature.import_signature sg universo;
      Dkmeta.to_signature md ~sg entries'
    in
    let sg = Signature.make "meta" in (* The name is not important here *)
    Signature.import_signature sg universo;
    Signature.import_signature sg (Dkmeta.LF.signature);
    add_rules sg (metaify TI.rules);
    add_rules sg (metaify TH.rules);
    add_rules sg TO.rules;
    let cfg =
      {uvar          = (module U);
       theory_input  = (module TI);
       theory        = (module TH);
       theory_output = (module TO);
       elaboration   = (module E);
       checker       = (module TYP);
       sg            = sg;
       sg_theory     = mk_theory sg (metaify TH.rules) theory;
       md_theory     = !md_theory;
       output_dir    = !output_dir}
    in
    Uconv.init sg !md_theory;
    List.iter (run_on_file cfg) files;
  with
  | Env.EnvError(l,e) -> Errors.fail_env_error l e
  | Signature.SignatureError e ->
     Errors.fail_env_error Basic.dloc (Env.EnvErrorSignature e)
  | Typing.TypingError e ->
    Errors.fail_env_error Basic.dloc (Env.EnvErrorType e)
  | Sys_error err -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit          -> exit 3
