exception Not_uvar

type t =
  {
    out_fmt:Format.formatter;
    theory_sort:Term.term;
    out_md:Basic.mident
  }

let basename = "?"

let pre_var : Basic.name = Basic.(mk_name (mk_mident "universo") (mk_ident "var"))

let is_pre_var t =
  match t with
  | Term.Const(_,n) when n = pre_var -> true
  | _ -> false

let is_uvar t =
  let open Basic in
  match t with
  | Term.Const(_,n) ->
    let s = string_of_ident (id n) in
    let n = String.length basename in
    String.length s > n && String.sub s 0 n = basename
  | _ -> false

let name_of_uvar t =
  match t with
  | Term.Const(_,n) when is_uvar t -> n
  | _ -> Format.printf "%a@." Term.pp_term t; raise Not_uvar

let counter = ref 0

let count () = !counter

let default_md = Basic.mk_mident "universo"

let fresh () =
  let name = Format.sprintf "%s%d" basename !counter in
  incr counter; Basic.mk_ident name

let fresh_uvar : t -> unit -> Term.term =
  fun env () ->
  let id = fresh () in
  let name = Basic.mk_name env.out_md id in
  let cst = Term.mk_Const Basic.dloc name in
  begin
    Format.fprintf env.out_fmt "%a@."
      Pp.print_entry (Entry.Decl(Basic.dloc, id, Signature.Definable, env.theory_sort))
  end;
  cst
