module type S =
sig
  val elab_term : Signature.t -> Term.term -> Term.term

  val elab_entry : Signature.t -> Entry.entry -> Entry.entry
end


module MakeUniv(U:Uvar.S) =
struct
  let rec elab_term sg (t:Term.term) =
    if Universes.is_pre_univ t then
      U.fresh_uvar sg
    else
      match t with
      | Term.Kind
      | Term.Type _
      | Term.DB (_,_,_)
      | Term.Const (_,_) -> t
      | Term.App (f,a,args) ->
        Term.mk_App2 (elab_term sg f) (List.map (elab_term sg) (a::args))
      | Term.Lam (lc,id,Some ty,te) ->
        Term.mk_Lam lc id (Some (elab_term sg ty)) (elab_term sg te)
      | Term.Lam (_,_,None,_) -> failwith "Cannot elab_term untyped lambdas"
      | Term.Pi (lc,id,tya,tyb) ->
        Term.mk_Pi lc id (elab_term sg tya) (elab_term sg tyb)

  let elab_entry sg (e:Entry.entry) =
    let open Entry in
    match e with
    | Decl(lc, id, st, ty) ->
      Format.eprintf "on %a@." Pp.print_ident id;
      Decl(lc,id,st, elab_term sg ty)
    | Def(lc, id, op, mty, te) ->
      Format.eprintf "on %a@." Pp.print_ident id;
      let mty' = match mty with None -> None | Some ty -> Some (elab_term sg ty) in
      let te' = elab_term sg te in
      Def(lc, id, op, mty', te')
    | Rules(lc, rs) ->
      let open Rule in
      let rhs : untyped_rule -> untyped_rule = fun r -> {r with rhs = elab_term sg r.rhs} in
      Rules(lc, List.map rhs rs)
    | _ -> e
end

module MakePre(T:Theory.Compat) =
struct

  open Meta

  let _ = Config.(config.encoding <- Some (module Encoding.LF))

  let constructors = List.map Meta.encode_untyped_rule T.constructors

  let elab_term sg (t:Term.term) =
    let open Config in
    Signature.import_signature sg Encoding.LF.signature;
    List.iter (fun r -> Signature.add_rules sg [Rule.to_rule_infos r]) constructors;
    config.meta_rules <- List.map (fun (r:Rule.untyped_rule) -> r.Rule.name) constructors;
    Meta.normalize ~sg:(Some sg) t

  let elab_entry sg (e:Entry.entry) =
    let open Entry in
    match e with
    | Decl(lc, id, st, ty) ->
      Format.eprintf "on %a@." Pp.print_ident id;
      Decl(lc,id,st, elab_term sg ty)
    | Def(lc, id, op, mty, te) ->
      Format.eprintf "on %a@." Pp.print_ident id;
      let mty' = match mty with None -> None | Some ty -> Some (elab_term sg ty) in
      let te' = elab_term sg te in
      Def(lc, id, op, mty', te')
    | Rules(lc, rs) ->
      let open Rule in
      let rhs : untyped_rule -> untyped_rule = fun r -> {r with rhs = elab_term sg r.rhs} in
      Rules(lc, List.map rhs rs)
    | _ -> e
end

module Make(T:Theory.Compat)(U:Uvar.S) : S =
struct

  let (--) f g = fun x -> g (f x)

  module EP = MakePre(T)

  module EU = MakeUniv(U)

  let elab_entry sg = (EP.elab_entry sg) -- (EU.elab_entry sg)

  let elab_term sg = (EP.elab_term sg) -- (EU.elab_term sg)
end
