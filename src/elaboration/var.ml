exception Not_uvar

type t =
  {
    out_fmt:Format.formatter;
    (** Where to print universe variables declarations *)
    out_md:Basic.mident;
    (** mident of the module that contains universe variables declarations *)
    theory_sort:Term.term;
    (** Type of a universe in the original theory *)
  }

(** prefix for all the universe variables. *)
let basename = "?"


(* FIXME: This should be in a common module, maybe Universes? *)
(** Name of a variable before generating a fresh one *)
let pre_var : Basic.name = Basic.(mk_name (mk_mident "universo") (mk_ident "var"))

(** Check if a term should be elaborated by a fresh variable *)
let is_pre_var t =
  match t with
  | Term.Const(_,n) when n = pre_var -> true
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

(* FIXME: should be somewhere else in a common module such as Universes *)
let default_md = Basic.mk_mident "universo"

(** Generate a fresh name for a universe variable *)
let fresh () =
  let name = Format.sprintf "%s%d" basename !counter in
  incr counter; Basic.mk_ident name

(** [fresh_uvar env ()] returns a fresh term representing a universe variable. Add a new declaration into the module env.md *)
let fresh_uvar : t -> unit -> Term.term =
  fun env () ->
  let id = fresh () in
  let uvar = Basic.mk_name env.out_md id in
  let uterm = Term.mk_Const Basic.dloc uvar in
  begin
    Format.fprintf env.out_fmt "%a@."
      Pp.print_entry (Entry.Decl(Basic.dloc, id, Signature.Definable, env.theory_sort))
  end;
  uterm
