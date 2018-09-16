type cfg =
  {
    sg:Signature.t;
    oc:out_channel option
  }

module type S =
  sig
    val elab_term  : cfg -> Term.term -> Term.term

    val elab_entry : cfg -> Entry.entry -> Entry.entry
  end


module MakeUniv(U:Uvar.S) : S =
  struct
    let rec elab_term cfg (t:Term.term) =
      if Universes.is_pre_univ t then
        U.fresh_uvar ~oc:cfg.oc cfg.sg
      else
        match t with
        | Term.Kind
          | Term.Type _
          | Term.DB (_,_,_)
          | Term.Const (_,_) -> t
        | Term.App (f,a,args) ->
           Term.mk_App2 (elab_term cfg f) (List.map (elab_term cfg) (a::args))
        | Term.Lam (lc,id,Some ty,te) ->
           Term.mk_Lam lc id (Some (elab_term cfg ty)) (elab_term cfg te)
        | Term.Lam (_,_,None,_) -> failwith "Cannot elab_term untyped lambdas"
        | Term.Pi (lc,id,tya,tyb) ->
           Term.mk_Pi lc id (elab_term cfg tya) (elab_term cfg tyb)

    let elab_entry cfg (e:Entry.entry) =
      let open Entry in
      match e with
      | Decl(lc, id, st, ty) ->
         Format.eprintf "on %a@." Pp.print_ident id;
         Decl(lc,id,st, elab_term cfg ty)
      | Def(lc, id, op, mty, te) ->
         Format.eprintf "on %a@." Pp.print_ident id;
         let mty' = match mty with None -> None | Some ty -> Some (elab_term cfg ty) in
         let te' = elab_term cfg te in
         Def(lc, id, op, mty', te')
      | Rules(lc, rs) ->
         let open Rule in
         let rhs : untyped_rule -> untyped_rule = fun r -> {r with rhs = elab_term cfg r.rhs} in
         Rules(lc, List.map rhs rs)
      | _ -> e
  end

module MakePre(T:Theory.In) : S =
  struct

    open Meta

    let _ = Config.(config.encoding <- Some (module Encoding.LF))

    let constructors = List.map Meta.encode_untyped_rule T.constructors

    let init = ref false

    let elab_term cfg (t:Term.term) =
      let open Config in
      Signature.import_signature cfg.sg Encoding.LF.signature;
      if not !init then (
        List.iter (fun r -> Signature.add_rules cfg.sg [Rule.to_rule_infos r]) constructors;
        init := true );
      config.encoding <- Some (module Encoding.LF);
      config.meta_rules <- List.map (fun (r:Rule.untyped_rule) -> r.Rule.name) constructors;
      Meta.normalize ~sg:(Some cfg.sg) t

    let elab_entry cfg (e:Entry.entry) =
      let open Entry in
      match e with
      | Decl(lc, id, st, ty) ->
         Format.eprintf "on %a@." Pp.print_ident id;
         Decl(lc,id,st, elab_term cfg ty)
      | Def(lc, id, op, mty, te) ->
         Format.eprintf "on %a@." Pp.print_ident id;
         let mty' = match mty with None -> None | Some ty -> Some (elab_term cfg ty) in
         let te' = elab_term cfg te in
         Def(lc, id, op, mty', te')
      | Rules(lc, rs) ->
         let open Rule in
         let rhs : untyped_rule -> untyped_rule = fun r -> {r with rhs = elab_term cfg r.rhs} in
         Rules(lc, List.map rhs rs)
      | _ -> e
  end

module Make(T:Theory.In)(U:Uvar.S) : S =
  struct
    let (--) f g = fun x -> g (f x)

    module EP = MakePre(T)

    module EU = MakeUniv(U)

    let elab_term cfg = (EP.elab_term cfg) -- (EU.elab_term cfg)

    let elab_entry cfg = (EP.elab_entry cfg) -- (EU.elab_entry cfg)
  end
