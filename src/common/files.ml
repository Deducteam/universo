module B = Basic

(** File utilities *)

type path = string

type cin
type cout

type _ channel =
  | In : in_channel ->  cin channel
  | Out : out_channel * Format.formatter -> cout channel

type 'a t =
  {
    path : path;
    md : B.mident;
    channel : 'a channel
  }

let out_default =
  {
    path = "";
    md = B.mk_mident "";
    channel = Out(stdout, Format.formatter_of_out_channel stdout)}

(** output where new files are created *)
let output_directory : string option ref = ref None

(** output where simplified files are created *)
let simplify_directory : string option ref = ref None

(** suffix used for files containing universe declarations *)
let elaboration_suffix = "_univ"

(** suffix used for files containing universe constraints *)
let checking_suffix = "_cstr"

(** suffix used for files containing universe solution *)
let solution_suffix = "_sol"

(** suffix used for elaborated file with fresh universe variables *)
let normal_suffix = ""

(** Steps of Universo *)
type step = [`Input | `Output | `Elaboration | `Checking | `Solution | `Simplify]

(** [add_sufix file suffix] returns the string [file'] where suffix is_added at then end of [file] *)
let add_suffix : path -> string -> string = fun file suffix ->
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
  | `Input
  | `Simplify
  | `Output -> normal_suffix


let theory : path ref = ref ""

(** [md_of_file f] returns the [mident] of the file [f] *)
let md_of_path : path -> Basic.mident = fun path ->
  Basic.mk_mident (Filename.basename path)

(** [get_out_path p s] returns the path that corresponds to the step [s] for path [p] *)
let get_out_path : path -> step -> path = fun path step ->
  let file_suffix = add_suffix path (suffix_of_step step) in
  match step with
  | `Input -> file_suffix
  | `Simplify ->
    begin
      match !simplify_directory with
      | None -> assert false
      | Some dir -> add_dir dir file_suffix
    end
  | _ ->
    match !output_directory with
    | None -> failwith "Output_directory must be set. See --help for more information"
    | Some dir -> add_dir dir file_suffix

(** [from_string f s] returns the filename that corresponds to the step [s] for file [f] *)
let out_from_string : path -> step -> cout t = fun path step ->
    let path = get_out_path path step in
    let md = md_of_path path in
    let oc = open_out path in
    let fmt = Format.formatter_of_out_channel oc in
    {path;md;channel=Out(oc,fmt)}

(** [from_string f s] returns the filename that corresponds to the step [s] for file [f] *)
let in_from_string : path -> step -> cin t = fun path step ->
    let path = get_out_path path step in
    let md = md_of_path path in
    let ic = open_in path in
    {path;md;channel=In ic}

(** [signature_of_file ?sg f] returns a signature that contains all the declarations in [f].
    If a signature [sg] is given, then the declarations are append to [sg] *)
let signature_of_file : ?sg:Signature.t -> path -> Signature.t = fun ?sg file ->
  let ic = open_in file in
  let md = md_of_path file in
  let entries = Parser.Parse_channel.parse md ic in
  close_in ic;
  Entry.to_signature file ?sg entries

let fmt_of_file : cout t -> Format.formatter = fun file ->
  match file.channel with
  | Out(_,fmt) -> fmt

let in_channel_of_file : cin t -> in_channel = fun file ->
  match file.channel with
  | In ic -> ic

let close_out : cout t -> unit = fun file ->
  match file.channel with
  | Out (oc,_) -> close_out oc

let close_in : cin t -> unit = fun file ->
  match file.channel with
  | In ic -> close_in ic

let md_of : path -> step -> B.mident = fun in_path step ->
  md_of_path (get_out_path in_path step)

let add_requires : Format.formatter ->  B.mident list -> unit = fun fmt mds ->
  List.iter (fun md -> Format.fprintf fmt "#REQUIRE %a.@." Pp.print_mident md)  mds

let export : path -> step -> unit = fun in_path step ->
  let in_file = in_from_string in_path step in
  match step with
  | `Checking ->
    let entries = Parser.Parse_channel.parse in_file.md (in_channel_of_file in_file) in
    let filter = function
      | Entry.Rules(_,r::_) ->
        begin
          match r.pat with
          | Rule.Pattern(_,_,[]) -> true
          | _ -> false
        end
      | _ -> true
    in
    let entries = List.filter filter entries in
    let sg = Entry.to_signature in_file.path entries in
    Signature.export sg
  | _ ->
    let sg = signature_of_file in_file.path in
    Signature.export sg
