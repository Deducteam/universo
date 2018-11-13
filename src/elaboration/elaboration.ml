type t =
  {
    out_fmt:Format.formatter;
    out_md:Basic.mident;
    theory_sort:Term.term;
    meta:Dkmeta.cfg (* Meta rules that translates universes to the Universo constructor "Var" *)
  }

let rec mk_term : t -> Term.term -> Term.term = fun env ->
  let var_env : Var.t = {out_fmt=env.out_fmt;
                         theory_sort=env.theory_sort;
                         out_md=env.out_md} in
  fun t ->
    if Var.is_pre_var t then
      Var.fresh_uvar var_env ()
    else
      match t with
      | Term.Kind
      | Term.Type _
      | Term.DB (_,_,_)
      | Term.Const (_,_) -> t
      | Term.App (f,a,args) ->
        Term.mk_App2 (mk_term env f) (List.map (mk_term env) (a::args))
      | Term.Lam (lc,id,Some ty,te) ->
        Term.mk_Lam lc id (Some (mk_term env ty)) (mk_term env te)
      | Term.Lam (_,_,None,_) -> failwith "Cannot elaborate untyped lambdas"
      | Term.Pi (lc,id,tya,tyb) ->
        Term.mk_Pi lc id (mk_term env tya) (mk_term env tyb)

let mk_term : t -> Term.term -> Term.term = fun env t ->
  (* Make the term independent from the theory first *)
  let t = Dkmeta.mk_term env.meta t in
  mk_term env t

let mk_rule : t -> 'a Rule.rule -> 'a Rule.rule = fun env rule -> Rule.(
  {rule with rhs = mk_term env (Dkmeta.mk_term env.meta rule.rhs)})


let mk_entry : t -> Entry.entry -> Entry.entry = fun env e ->
  let open Entry in
  match e with
  | Decl(lc, id, st, ty) ->
    Format.eprintf "[ELAB] %a@." Pp.print_ident id;
    Format.fprintf env.out_fmt "(; %a ;)@." Pp.print_ident id;
    Decl(lc,id,st, mk_term env ty)
  | Def(lc, id, op, mty, te) ->
    Format.eprintf "[ELAB] %a@." Pp.print_ident id;
    Format.fprintf env.out_fmt "(; %a ;)@." Pp.print_ident id;
    let mty' = match mty with None -> None | Some ty -> Some (mk_term env ty) in
    let te' = mk_term env te in
    Def(lc, id, op, mty', te')
  | Rules(lc, rs) ->
    Format.fprintf env.out_fmt "(; RULES ;)@.";
    Rules(lc,List.map (mk_rule env) rs)
  | _ -> e
