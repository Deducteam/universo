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
let universo =
  let ic = open_in "encodings/universo.dk" in
  let md = Basic.mk_mident "universo" in
  let entries = Parser.Parse_channel.parse md ic in
  Dkmeta.to_signature md entries (* this should not be in dkmeta *)

let theory_sort : unit -> Term.term = fun () ->
  let sort = Basic.mk_name (Basic.mk_mident "universo") (Basic.mk_ident "Sort") in
  let meta = Dkmeta.meta_of_file ~sg:universo false !compat_output in
  let sort = Term.mk_Const Basic.dloc sort in
  Dkmeta.mk_term meta sort

let to_elaboration_env : string -> Elaboration.t = fun in_file ->
  let out_md = Files.md_of_file (Files.from_string in_file `Elaboration) in
  let out_file = Files.from_string in_file `Elaboration in
  let out_fmt = Format.formatter_of_out_channel (open_out out_file) in
  let meta = Dkmeta.meta_of_file true !compat_input in
  let theory_sort = theory_sort () in
  {out_fmt; out_md; theory_sort; meta}
