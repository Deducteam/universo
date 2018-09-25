module type S =
sig
  val meta : Dkmeta.cfg
end

let sg md = Signature.make md

let metaify rs = List.map Dkmeta.LF.encode_rule rs

let add_rule sg r =
  Signature.add_rules sg [(Rule.to_rule_infos r)]

(* Several rules might be bound to different constant *)
let add_rules sg rs = List.iter (add_rule sg) rs

let from_file : Signature.t list -> bool -> string -> (module S) =
  let open Dkmeta in
  fun dep_sgs encode file ->
  let ic = open_in file in
  let md = Env.init file in
  let mk_entry = function
    | Entry.Rules(_,rs) -> rs
    | _ -> assert false
  in
  let entries = Parser.parse_channel md ic in
  let rules = List.fold_left (fun r e -> r@mk_entry e) [] entries in
  let sg = sg (Basic.string_of_mident md) in
  List.iter (fun ext -> Signature.import_signature sg ext) dep_sgs;
  begin
    if encode then
      begin
        Signature.import_signature sg (Dkmeta.LF.signature);
        add_rules sg (metaify rules)
      end
    else
        add_rules sg rules
  end;
  match rules with
  | [] -> assert false
  | _ -> (module struct let meta =
    {
      beta = true;
      encoding = Some (module Dkmeta.LF);
      sg = sg;
      meta_rules = Some (List.map (fun (r:Rule.untyped_rule) -> r.Rule.name) rules)
    } end)
