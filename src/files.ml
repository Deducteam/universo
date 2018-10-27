(** File utilities *)

let output_directory = ref None

let elaboration_suffix = "elab"

let checking_suffix = "cstr"

let solution_suffix = "sol"

type step = [`Elaboration | `Checking | `Solution]

let add_suffix : string -> string -> string = fun file suffix ->
  let ext = Filename.extension file in
  let name = Filename.chop_extension file in
  name ^ "_" ^ suffix ^ ext

let add_dir : string -> string -> string = fun dir file ->
  let basename = Filename.basename file in
  dir ^ Filename.dir_sep ^ basename

let suffix_of_step = function
  | `Elaboration -> elaboration_suffix
  | `Checking -> checking_suffix
  | `Solution -> solution_suffix

let from_string : string -> step -> string = fun file step ->
  let file_suffix = add_suffix file (suffix_of_step step) in
  match !output_directory with
  | None -> file_suffix
  | Some dir -> add_dir dir file_suffix

let md_of_file : string -> Basic.mident = fun file ->
  Basic.mk_mident (Filename.basename file)
