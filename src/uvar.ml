open Basic

type uvar = ident

let basename = "?"

exception NotUvar

let is_uvar t =
  match t with
  | Term.Const(_,n) ->
    let s = string_of_ident (id n) in
    let n = String.length basename in
    String.length s > n && String.sub s 0 n = basename
  | _ -> false

let ident_of_uvar t =
  match t with
  | Term.Const(_,n) when is_uvar t -> id n
  | _ -> Format.printf "%a@." Term.pp_term t; raise NotUvar

let counter = ref 0

let count () = !counter

let fresh () =
  let name = Format.sprintf "%s%d" basename !counter in
  incr counter; mk_ident name

(*
let fresh_uvar sg =
  let id = fresh () in
  let md = Signature.get_name sg in
  let name = mk_name md id in
  let cst = Term.mk_Const dloc name in
  Signature.add_declaration sg dloc id Signature.Definable
    (Term.mk_Const dloc (mk_name (mk_mident "cic") (mk_ident "Sort")));
  cst
  *)

let fresh_uvar () =
  let id = fresh () in
  let err_opt = Env.declare dloc id Signature.Definable (Term.mk_Const dloc Cic.sort) in
  match err_opt with
  | OK () ->
    Term.mk_Const dloc (mk_name (Env.get_name ()) id)
  | Err err -> Errors.fail_env_error err
