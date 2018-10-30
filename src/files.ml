(** File utilities *)

let output_directory = ref None

let elaboration_suffix = "_univ"

let checking_suffix = "_cstr"

let solution_suffix = "_sol"

let normal_suffix = ""

type step = [`Normal | `Elaboration | `Checking | `Solution]

let add_suffix : string -> string -> string = fun file suffix ->
  let ext = Filename.extension file in
  let name = Filename.chop_extension file in
  name ^ suffix ^ ext

let add_dir : string -> string -> string = fun dir file ->
  let basename = Filename.basename file in
  dir ^ Filename.dir_sep ^ basename

let suffix_of_step = function
  | `Elaboration -> elaboration_suffix
  | `Checking -> checking_suffix
  | `Solution -> solution_suffix
  | `Normal -> normal_suffix

let from_string : string -> step -> string = fun file step ->
  let file_suffix = add_suffix file (suffix_of_step step) in
  match !output_directory with
  | None -> file_suffix
  | Some dir -> add_dir dir file_suffix

let md_of_file : string -> Basic.mident = fun file ->
  Basic.mk_mident (Filename.basename file)

let entries_of_file : string -> Entry.entry list = fun file ->
  let ic = open_in file in
  let md = md_of_file file in
  Parser.Parse_channel.parse md ic

let signature_of_file : ?sg:Signature.t -> string -> Signature.t = fun ?sg file ->
  let ic = open_in file in
  let md = md_of_file file in
  let entries = Parser.Parse_channel.parse md ic in
  Dkmeta.to_signature ?sg md entries
