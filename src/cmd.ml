module F = Common.Files

let theory        = ref ""
let compat_theory = ref ""
let compat_input  = ref ""
let compat_output = ref ""

let add_rules sg rs =
  (* Several rules might be bound to different constant *)
  let add_rule sg r =
    Signature.add_rules sg [(Rule.to_rule_infos r)]
  in
  List.iter (add_rule sg) rs

let dummy_name = ""

(* Contains theory for universes *)
let universo () =
  F.signature_of_file "encodings/universo.dk"

let theory_sort : unit -> Term.term = fun () ->
  let sort = Basic.mk_name (Basic.mk_mident "universo") (Basic.mk_ident "Sort") in
  let meta = Dkmeta.meta_of_file false !compat_output in
  let sort = Term.mk_Const Basic.dloc sort in
  (* compat_output (universo.sort) --> <theory>.sort *)
  Dkmeta.mk_term meta sort

let to_elaboration_env : string -> Elaboration.Elaborate.t = fun in_file ->
  let out_md = F.md_of_file (F.from_string in_file `Elaboration) in
  let out_file = F.from_string in_file `Elaboration in
  let out_fmt = Format.formatter_of_out_channel (open_out out_file) in
  let meta = Dkmeta.meta_of_file false !compat_input in
  let theory_sort = theory_sort () in
  {out_fmt; out_md; theory_sort; meta}

let mk_theory : Dkmeta.cfg -> Signature.t = fun meta ->
  let ic = open_in !theory in
  let md = F.md_of_file !theory in
  let entries = Parser.Parse_channel.parse md ic in
  let sg = universo () in
  let entries' = List.map (Dkmeta.mk_entry ~all_def:true meta md) entries in
  let sg = Dkmeta.to_signature md ~sg entries' in
  F.signature_of_file ~sg !compat_theory

let elab_signature : string -> Signature.t = fun in_file ->
  let file = F.from_string in_file `Elaboration in
  F.signature_of_file file

let to_checking_env : string -> Checking.Checker.t = fun in_file ->
  let meta = Dkmeta.meta_of_file false !compat_theory in
  let theory_signature = mk_theory meta in
  let sg = Signature.make (Filename.basename in_file) in
  Signature.import_signature sg theory_signature;
  Signature.import_signature sg (elab_signature in_file);
  let meta_out = Dkmeta.meta_of_file false !compat_output in
  let check_fmt = Format.formatter_of_out_channel (open_out (F.from_string in_file `Checking)) in
  { sg;
    md=F.md_of_file (F.from_string in_file `Normal);
    md_check=F.md_of_file (F.from_string in_file `Checking);
    md_elab=F.md_of_file (F.from_string in_file `Elaboration);
    meta;
    meta_out;
    check_fmt
  }

let to_solver_env : unit -> Dkmeta.cfg = fun () ->
  let open Dkmeta in
  let meta = Dkmeta.meta_of_file false !compat_theory in
  {sg=mk_theory meta; beta=true;encoding=None;meta_rules=None}
