(** File utilities *)

(** output where new files are created *)
let output_directory = ref None

(** suffix used for files containing universe declarations *)
let elaboration_suffix = "_univ"

(** suffix used for files containing universe constraints *)
let checking_suffix = "_cstr"

(** suffix used for files containing universe solution *)
let solution_suffix = "_sol"

(** suffix used for elaborated file with fresh universe variables *)
let normal_suffix = ""

(** Steps of Universo *)
type step = [`Normal | `Elaboration | `Checking | `Solution]

(** [add_sufix file suffix] returns the string [file'] where suffix is_added at then end of [file] *)
let add_suffix : string -> string -> string = fun file suffix ->
  let ext = Filename.extension file in
  let name = Filename.chop_extension file in
  name ^ suffix ^ ext

(** [add_dir dir file] prefix the filename [file] with the directory [dir] *)
let add_dir : string -> string -> string = fun dir file ->
  dir ^ Filename.dir_sep ^ (Filename.basename file)


(** return the suffix according to the step [s] *)
let suffix_of_step : step -> string = function
  | `Elaboration -> elaboration_suffix
  | `Checking -> checking_suffix
  | `Solution -> solution_suffix
  | `Normal -> normal_suffix

(** [from_string f s] returns the filename that corresponds to the step [s] for file [f] *)
let from_string : string -> step -> string = fun file step ->
  let file_suffix = add_suffix file (suffix_of_step step) in
  match !output_directory with
  | None -> file_suffix
  | Some dir -> add_dir dir file_suffix

(** [md_of_file f] returns the [mident] of the file [f] *)
let md_of_file : string -> Basic.mident = fun file ->
  Basic.mk_mident (Filename.basename file)

(** [signature_of_file ?sg f] returns a signature that contains all the declarations in [f].
    If a signature [sg] is given, then the declarations are append to [sg] *)
let signature_of_file : ?sg:Signature.t -> string -> Signature.t = fun ?sg file ->
  let ic = open_in file in
  let md = md_of_file file in
  let entries = Parser.Parse_channel.parse md ic in
  close_in ic;
  Dkmeta.to_signature ?sg md entries
