module B = Basic
module F = Common.Files
module U = Common.Universes

(** The path to the rewrite rules mapping universes from the original theory to the one of Universo *)
let compat_theory = ref ""

(** The path to the rewrite rules mapping constructors of universes to pre-universe variable *)
let compat_input  = ref ""

(** The path to the rewrite rules mapping universo construction to the one of the theory *)
let compat_output = ref ""

(** The path to the rewrite rules that contains additional constraints for some identifiers *)
let constraints_path = ref ""

(** The path that contains the target specification *)
let target_path = ref ""

let mk_constraints : unit -> (B.name, U.pred) Hashtbl.t = fun () ->
  let ic = open_in !constraints_path in
  let md = F.md_of_path !constraints_path in
  let table = Hashtbl.create 11 in
  let mk_rule : Rule.untyped_rule -> unit = fun r ->
    let open Rule in
    let name = match r.pat with
      | Rule.Pattern(_,name,[]) -> name
      | _ -> failwith "Constraints are not in correct format"
    in
    (* let pred = try U.extract_pred (Dkmeta.mk_term meta r.rhs) *)
    let pred = try U.extract_pred r.rhs
      with U.Not_pred -> failwith "Constraints are not in correct format"
    in
    Hashtbl.add table name pred
  in
  let mk_entry = function
    | Entry.Rules(_,rs) ->
      List.iter mk_rule rs
    | _ -> failwith "Constraints are not in correct format"
  in
  Parser.Parse_channel.handle md mk_entry ic;
  table

(** [add_rules sg rs] add the rewrite rules [rs] to the signature [sg] *)
let add_rules sg rs =
  (* Several rules might be bound to different constant *)
  let add_rule sg r =
    Signature.add_rules sg [(Rule.to_rule_infos r)]
  in
  List.iter (add_rule sg) rs

  (** Signature that contains a generic theory of universes for Universo. Imperative features of Dedukti forces us to generate this signature for each file otherwise, this signature would contain too many declarations *)
let universo () = F.signature_of_file "encodings/universo.dk"

(** [theory_sort ()] returns the type of universes in the original theory *)
let theory_sort : unit -> Term.term = fun () ->
  let meta = Dkmeta.meta_of_file Dkmeta.default_config !compat_output in
  let sort = Term.mk_Const B.dloc (U.sort ()) in
  (* compat_output (universo.sort) --> <theory>.sort *)
  Dkmeta.mk_term meta sort

(** [to_elaboration_env f] generates a fresh environement to elaborate file [f]. *)
let to_elaboration_env : F.path -> Elaboration.Elaborate.t = fun in_path ->
  let file = F.out_from_string in_path `Elaboration in
  let meta = Dkmeta.meta_of_file Dkmeta.default_config !compat_input in
  let theory_sort = theory_sort () in
  {file; theory_sort; meta}

(** [mk_theory ()] returns the theory used by universo. *)
let mk_theory : unit -> Signature.t = fun () ->
  let ic = open_in !F.theory in
  let md = F.md_of_path !F.theory in
  let entries = Parser.Parse_channel.parse md ic in
  Entry.to_signature !F.theory entries

(** [elab_signature f] returns the signature containing all the universes declaration associated to
    file [f] *)
let elab_signature : string -> Signature.t = fun in_path ->
  F.signature_of_file (F.get_out_path in_path `Elaboration)

(** [to_checking_env f] returns the type checking environement for the file [f] *)
let to_checking_env : string -> Checking.Checker.t = fun in_path ->
  let theory_signature = mk_theory () in
  let sg = Signature.make (Filename.basename in_path) in
  Signature.import_signature sg theory_signature;
  Signature.import_signature sg (elab_signature in_path);
  let meta_out = Dkmeta.meta_of_file Dkmeta.default_config !compat_output in
  let constraints = mk_constraints () in
  { sg; in_path; meta_out; constraints}

(** [theory_meta f] returns the meta configuration that allows to elaborate a theory for the SMT solver *)
let theory_meta : unit -> Dkmeta.cfg = fun () ->
  let meta = Dkmeta.meta_of_file Dkmeta.default_config !compat_output in
  Dkmeta.meta_of_file meta !target_path
