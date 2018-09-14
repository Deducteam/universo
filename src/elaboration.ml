module type S =
sig
  val elab_term : Signature.t -> Term.term -> Term.term

  val elab_entry : Signature.t -> Entry.entry -> Entry.entry
end


module Make(U:Uvar.S) =
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

  let elab_entry sg (e:Entry.entry) = failwith "todo"
end
