module U = Universes

type t =
  {
    out_fmt:Format.formatter;
    meta:Dkmeta.cfg
  }

(** [print_rule env cstr] prints the constraint [cstr] into the file [env.out_fmt] *)
let print_rule env cstr =
  let normalize t = Dkmeta.mk_term env.meta t in
  match cstr with
  | U.Pred(p) ->
    let left' = normalize (U.term_of_pred p) in
    let right' = normalize U.true_ in
    Format.fprintf env.out_fmt "@.[] %a --> %a.@." Pp.print_term left' Pp.print_term right'
  | U.EqVar(l,r) ->
    Format.fprintf env.out_fmt "@.[] %a --> %a.@." Pp.print_name l Pp.print_name r

let print_cstr env p = print_rule env p

(** [mk_var_cstre env f l r] add the constraint [l =?= r]. Call f on l and r such that
    l >= r. *)
let mk_var_cstr f l r =
  let get_number s =
    int_of_string (String.sub s 1 (String.length s - 1))
  in
  let nl = get_number (Basic.string_of_ident @@ Basic.id l) in
  let nr = get_number (Basic.string_of_ident @@ Basic.id r) in
  if nr < nl then (
    f l r; U.EqVar(l,r))
  else (
    f r l; U.EqVar(r,l))


let mk_cstr env f cstr =
  match cstr with
  | None ->  false
  | Some ((U.Pred _) as cstr) ->
    print_cstr env cstr;
    true
  | Some (U.EqVar(l,r)) ->
    (* print the rule in the correct order. *)
    let cstr' = mk_var_cstr f l r in
    print_cstr env cstr';
    true
