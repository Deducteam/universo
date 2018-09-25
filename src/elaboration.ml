module type S =
sig
  val elab_term  : Configuration.t -> Term.term -> Term.term

  val elab_entry : Configuration.t -> Entry.entry -> Entry.entry
end


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
    | Rules(lc, rs) ->
      let open Rule in
      let rhs : untyped_rule -> untyped_rule = fun r -> {r with rhs = elab_term env r.rhs} in
      Rules(lc, List.map rhs rs)
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
