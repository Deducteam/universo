module F = Common.Files
module U = Common.Universes

exception Not_uvar

(** prefix for all the universe variables. *)
let basename = "?"

(** Check if a term should be elaborated by a fresh variable *)
let is_pre_var t =
  match t with
  | Term.Const(_,n) when n = U.pvar () -> true
  | _ -> false

(** Check if a term is universe variable, i.e. its ident should be ?11, ?43... *)
let is_uvar t =
  let open Basic in
  match t with
  | Term.Const(_,n) ->
    let s = string_of_ident (id n) in
    let n = String.length basename in
    String.length s > n && String.sub s 0 n = basename
  | _ -> false

(** [name_of_uvar t] returns the name of universe variable if [t] is a universe variable, raise [Not_uvar] otherwise *)
let name_of_uvar t =
  match t with
  | Term.Const(_,n) when is_uvar t -> n
  | _ -> raise Not_uvar


(** Internal counter use by this module to generate fresh variables *)
let counter = ref 0

(** Generate a fresh name for a universe variable *)
let fresh () =
  let name = Format.sprintf "%s%d" basename !counter in
  incr counter; Basic.mk_ident name

(** [fresh_uvar env ()] returns a fresh term representing a universe variable. Add a new declaration into the module env.md *)
let fresh_uvar : F.cout F.t -> unit -> Term.term =
  fun file () ->
  let id = fresh () in
  let uvar = Basic.mk_name file.md id in
  let uterm = Term.mk_Const Basic.dloc uvar in
  let sort_type = Term.mk_Const Basic.dloc
      (Basic.mk_name (F.md_of_path !F.theory) (Basic.mk_ident "Sort")) in
  begin
    Format.fprintf (F.fmt_of_file file) "%a@."
      Pp.print_entry (Entry.Decl(Basic.dloc, id, Signature.Definable, sort_type))
  end;
  uterm
