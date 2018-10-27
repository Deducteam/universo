type t =
  {
    out_channel:out_channel;
    out_md:Basic.mident;
    theory_sort:Term.term;
    meta:Dkmeta.cfg (* Meta rules that translates universes to the Universo constructor "Var" *)
  }


let rec mk_term : t -> Term.term -> Term.term = fun env ->
  let var_env : Var.t = {out_channel=env.out_channel;
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
  {rule with rhs = Dkmeta.mk_term env.meta rule.rhs})


let mk_entry : t -> Entry.entry -> Entry.entry = fun env e ->
  let open Entry in
  match e with
  | Decl(lc, id, st, ty) ->
    Format.eprintf "[ELAB] on %a@." Pp.print_ident id;
    Decl(lc,id,st, mk_term env ty)
  | Def(lc, id, op, mty, te) ->
    Format.eprintf "[ELAB] on %a@." Pp.print_ident id;
    let mty' = match mty with None -> None | Some ty -> Some (mk_term env ty) in
    let te' = mk_term env te in
    Def(lc, id, op, mty', te')
  | Rules(lc, rs) ->
    Rules(lc,List.map (mk_rule env) rs)
  | _ -> e

(*
module MakeUniv(U:Uvar.S) : S =
struct
  let rec elab_term env (t:Term.term) =
    if Universes.is_pre_univ t then
      U.fresh_uvar env
    else
      match t with
      | Term.Kind
      | Term.Type _
      | Term.DB (_,_,_)
      | Term.Const (_,_) -> t
      | Term.App (f,a,args) ->
        Term.mk_App2 (elab_term env f) (List.map (elab_term env) (a::args))
      | Term.Lam (lc,id,Some ty,te) ->
        Term.mk_Lam lc id (Some (elab_term env ty)) (elab_term env te)
      | Term.Lam (_,_,None,_) -> failwith "Cannot elab_term untyped lambdas"
      | Term.Pi (lc,id,tya,tyb) ->
        Term.mk_Pi lc id (elab_term env tya) (elab_term env tyb)

  let elab_entry env (e:Entry.entry) =
    let open Entry in
    match e with
    | Decl(lc, id, st, ty) ->
      Format.eprintf "[ELAB Var] on %a@." Pp.print_ident id;
      Decl(lc,id,st, elab_term env ty)
    | Def(lc, id, op, mty, te) ->
      Format.eprintf "[ELAB Var] on %a@." Pp.print_ident id;
      let mty' = match mty with None -> None | Some ty -> Some (elab_term env ty) in
      let te' = elab_term env te in
      Def(lc, id, op, mty', te')
    | Rules(lc, rs) ->
      let open Rule in
      let rhs : untyped_rule -> untyped_rule = fun r -> {r with rhs = elab_term env r.rhs} in
      Rules(lc, List.map rhs rs)
    | _ -> e
end

module MakePre(T:Theory.S) : S =
struct
  let elab_term (cfg:Configuration.t) (t:Term.term) : Term.term =
    let open Dkmeta in
    let open Configuration in
    mk_term T.meta t

  let elab_entry env (e:Entry.entry) =
    let open Entry in
    match e with
    | Decl(lc, id, st, ty) ->
      Format.eprintf "[ELAB Pre] on %a@." Pp.print_ident id;
      Decl(lc,id,st, elab_term env ty)
    | Def(lc, id, op, mty, te) ->
      Format.eprintf "[ELAB Pre] on %a@." Pp.print_ident id;
      let mty' = match mty with None -> None | Some ty -> Some (elab_term env ty) in
      let te' = elab_term env te in
      Def(lc, id, op, mty', te')
    | Rules(lc, rs) -> (* FIXME: Nasty stuff due to optimization. *)
      let open Rule in
      let meta_pattern : Dkmeta.cfg =
        let filter n =
          match n with
          | Gamma(true,_) -> false (* left pattern are not elaborated with variables *)
          | _ -> true
        in
        match T.meta.Dkmeta.meta_rules with
        | None -> T.meta
        | Some rs ->
          {T.meta with Dkmeta.meta_rules = Some (List.filter filter rs)}
      in
      let e' = Dkmeta.mk_entry meta_pattern env.Configuration.md_check e in
      begin
        match e' with
        | Rules(lc,rs) ->
          Rules(lc,
                List.map (fun (r:untyped_rule) -> {r with Rule.rhs = Dkmeta.mk_term T.meta r.rhs}) rs)
        | _ -> assert false
      end
    | _ -> e
end

module Make(T:Theory.S)(U:Uvar.S) : S =
struct
  let (--) f g = fun x -> g (f x)

  module EP = MakePre(T)

  module EU = MakeUniv(U)

  let elab_term env = (EP.elab_term env) -- (EU.elab_term env)

  let elab_entry env = (EP.elab_entry env) -- (EU.elab_entry env)
end
*)
