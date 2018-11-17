type t =
  {
    out_fmt:Format.formatter;
    (** Where to print universe variables declarations *)
    out_md:Basic.mident;
    (** mident of the module that contains universe variables declarations *)
    theory_sort:Term.term;
    (** Type of a universe in the original theory *)
    meta:Dkmeta.cfg
    (** Meta rules that translates universes to the pre-universe variable *)
  }

(** Return a var environement using an elaboration environement *)
let var_env : t -> Var.t = fun env ->
  {out_fmt=env.out_fmt;theory_sort=env.theory_sort;out_md=env.out_md}

(** Takes a term [t] where universes are elaborated as pre-universe variables and returns a term where all the pre-universe variables are fresh universe variables *)
let rec mk_term : t -> Term.term -> Term.term = fun env ->
  fun t ->
    if Var.is_pre_var t then
      Var.fresh_uvar (var_env env) ()
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

(** [mk_term env t] replaces all the concrete universes in [t] by a fresh variable
    using the environment env. *)
let mk_term : t -> Term.term -> Term.term = fun env t ->
  (* Generate pre-universe variable first by replacing each universe with a pre-universe variable *)
  (* env.meta maps all the concrete universe to a unique constructor universo.var *)
  let t = Dkmeta.mk_term env.meta t in
  mk_term env t

(** [mkrule env r] replaces all the concrete universes in [rule.rhs] by a fresh variable
    using the environement env. *)
let mk_rule : t -> 'a Rule.rule -> 'a Rule.rule = fun env rule -> Rule.(
  {rule with rhs = mk_term env (Dkmeta.mk_term env.meta rule.rhs)})

(** [mk_entry env entry] replaces all the concrete universes in [entry] by a fresh variable
    using the environment env. Commands are skipped. *)
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
