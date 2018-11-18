module F = Common.Files
module U = Common.Universes

(** The path to the original theory *)
let theory        = ref ""

(** The path to the rewrite rules mapping universes from the original theory to the one of Universo *)
let compat_theory = ref ""

(** The path to the rewrite rules mapping constructors of universes to pre-universe variable *)
let compat_input  = ref ""

(** The path to the rewrite rules mapping universo construction to the one of the theory *)
let compat_output = ref ""


(** [add_rules sg rs] add the rewrite rules [rs] to the signature [sg] *)
let add_rules sg rs =
  (* Several rules might be bound to different constant *)
  let add_rule sg r =
    Signature.add_rules sg [(Rule.to_rule_infos r)]
  in
  List.iter (add_rule sg) rs

  (** Signature that contains a generic theory of universes for Universo. Imperative features of Dedukti forces us to generate this signature for each file otherwise, this signature would contain too many declarations *)
let universo () =
  F.signature_of_file "encodings/universo.dk"

(** [theory_sort ()] returns the type of universes in the original theory *)
let theory_sort : unit -> Term.term = fun () ->
  let meta = Dkmeta.meta_of_file false !compat_output in
  let sort = Term.mk_Const Basic.dloc U.sort in
  (* compat_output (universo.sort) --> <theory>.sort *)
  Dkmeta.mk_term meta sort

(** [to_elaboration_env f] generates a fresh environement to elaborate file [f]. *)
let to_elaboration_env : string -> Elaboration.Elaborate.t = fun in_file ->
  let out_md = F.md_of_file (F.from_string in_file `Elaboration) in
  let out_file = F.from_string in_file `Elaboration in
  let out_fmt = Format.formatter_of_out_channel (open_out out_file) in
  let meta = Dkmeta.meta_of_file false !compat_input in
  let theory_sort = theory_sort () in
  {out_fmt; out_md; theory_sort; meta}

(** [mk_theory meta] returns a signature that corresponds to the original where universes of universo have been plugged in. This allows us to type check as if we were in the original theory but Universo can recognize easily a universe. *)
let mk_theory : Dkmeta.cfg -> Signature.t = fun meta ->
  let ic = open_in !theory in
  let md = F.md_of_file !theory in
  let entries = Parser.Parse_channel.parse md ic in
  let sg = universo () in
  (* The line below does the main trick: it normalizes every entry of the original theory with the universes of Universo *)
  let entries' = List.map (Dkmeta.mk_entry meta md) entries in
  let sg = Dkmeta.to_signature md ~sg entries' in
  (* We include the compat theory so that the type checker transforms automatically a universe from the original theory to the one of Universo. *)
  F.signature_of_file ~sg !compat_theory

(** [elab_signature f] returns the signature containing all the universes declaration associated to
    file [f] *)
let elab_signature : string -> Signature.t = fun in_file ->
  F.signature_of_file (F.from_string in_file `Elaboration)

(** [to_checking_env f] returns the type checking environement for the file [f] *)
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
    md_theory=F.md_of_file !theory;
    md_check=F.md_of_file (F.from_string in_file `Checking);
    md_elab=F.md_of_file (F.from_string in_file `Elaboration);
    meta_out;
    check_fmt
  }

(** [theory_meta f] returns the meta configuration that allows to elaborate a theory for the SMT solver *)
let theory_meta : unit -> Dkmeta.cfg = fun () ->
  let open Dkmeta in
  let meta = Dkmeta.meta_of_file false !compat_theory in
  {sg=mk_theory meta; beta=true;encoding=None;meta_rules=None}
