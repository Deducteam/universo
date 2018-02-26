open Basic
open Parser

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

let next () = !counter

let fresh () =
  let name = Format.sprintf "%s%d" basename !counter in
  incr counter; mk_ident name

let ident_of_counter n =
  mk_ident (Format.sprintf "%s%d" basename n)

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


let extract_counter var =
  let s = string_of_ident var in
  let n = String.length basename in
  if String.length s > n then
    String.sub s n (String.length s - n)
  else
    failwith "should not happen"

let lt id id' =
  let c  = extract_counter id  in
  let c' = extract_counter id' in
  if compare (int_of_string c) (int_of_string c') < 0 then
    true
  else
    false

module Elaboration =
struct

  let prop_elaboration = ref false

  let rec elaboration te =
    let open Term in
    let open Cic  in
    if is_prop te then
      if !prop_elaboration then
        fresh_uvar ()
      else
        te
    else if  is_type te then
     fresh_uvar ()
    else
      match te with
      | App(f, a, al) ->
        let f' = elaboration f in
        let a' = elaboration a in
        let al' = List.map elaboration al in
        mk_App f' a' al'
      | Lam(loc, id, t_opt, t) ->
        let t' = elaboration t in
        begin
          match t_opt with
          | None -> mk_Lam loc id t_opt t'
          | Some x -> let x' = elaboration x in
            mk_Lam loc id (Some x') t'
        end
      | Pi(loc, id, ta, tb) ->
        let ta' = elaboration ta in
        let tb' = elaboration tb in
        mk_Pi loc id ta' tb'
      | _ ->     te

  let generate_var =
    let old_c = ref 0 in
    fun () ->
      let c = next () in
      let rec aux n =
        if n = !old_c then
          (old_c := c; [])
        else
          let d = ident_of_counter n in
          d::(aux (n - 1))
      in
      if c = !old_c then
        []
      else
        List.rev (aux c)

  let elaboration_entry e =
    let open Rule in
    let open Parser in
    match e with
    | Decl(l,id,st,t) -> Decl(l,id,st, elaboration t)
    | Def(l,id,op,pty,te) -> Def(l,id,op, Basic.map_opt elaboration pty, elaboration te)
    | Rules(rs) ->
      let rs' = List.map (fun (r: untyped_rule) -> {r  with rhs = elaboration r.rhs}) rs in
      Rules(rs')
    | Name (l,id) -> Name(l,id)
    | _ -> failwith "unsupported"

  let elaboration_entry e =
    let e' = elaboration_entry e in
    generate_var (), e'
end
