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

(* Probably should be in dkmeta *)
let meta_of_file : string -> Dkmeta.cfg = fun file ->
  let ic = open_in file in
  let mk_entry = function
    | Entry.Rules(_,rs) -> rs
    | _ -> assert false
  in
  let md = (Files.md_of_file file) in
  let entries = Parser.Parse_channel.parse md ic in
  let rules = List.fold_left (fun r e -> r@mk_entry e) [] entries in
  let rule_names = List.map (fun (r:Rule.untyped_rule) -> r.Rule.name) rules in
  let sg = Signature.make dummy_name in
  {
    beta = true;
    encoding = None;
    sg ;
    meta_rules = Some rule_names
  }

let theory_sort : unit -> Term.term = fun () ->
  let sort = Basic.mk_name (Basic.mk_mident "universo") (Basic.mk_ident "Sort") in
  let meta = meta_of_file !compat_output in
  let sort = Term.mk_Const Basic.dloc sort in
  Dkmeta.mk_term meta sort

let to_elaboration_env : string -> Elaboration.t = fun in_file ->
  let out_md = Files.md_of_file in_file in
  let out_file = Files.from_string in_file `Elaboration in
  let out_channel = open_out out_file in
  let meta = meta_of_file !compat_input in
  let theory_sort = theory_sort () in
  {out_channel; out_md; theory_sort; meta}
