module U = Universes

type t =
  {
    out_fmt:Format.formatter;
    meta:Dkmeta.cfg
  }

(* FIXME: should not be here *)
let print_rule env cstr =
  let normalize t = Dkmeta.mk_term env.meta t in
  match cstr with
  | U.Pred(p) ->
    let left' = normalize (U.term_of_pred p) in
    let right' = normalize U.true_ in
    Format.fprintf env.out_fmt "@.[] %a --> %a.@." Pp.print_term left' Pp.print_term right'
  | U.EqVar(l,r) ->
    Format.fprintf env.out_fmt "@.[] %a --> %a.@." Pp.print_name l Pp.print_name r

let add_cstr env p = print_rule env p

let mk_cstr env l r =
  assert(Term.term_eq r U.true_);
  let p = U.extract_pred l in
  add_cstr env (Pred p)

(** [mk_var_cstre env f l r] add the constraint [l =?= r]. Call f on l and r such that
    l >= r. *)
let mk_var_cstr env f l r =
  let get_number s =
    int_of_string (String.sub s 1 (String.length s - 1))
  in
  let nl = get_number (Basic.string_of_ident @@ Basic.id l) in
  let nr = get_number (Basic.string_of_ident @@ Basic.id r) in
  if nr < nl then
    begin
      f l r; add_cstr env (EqVar(l,r))
    end
  else
    begin
      f r l; add_cstr env (EqVar(r,l))
    end
