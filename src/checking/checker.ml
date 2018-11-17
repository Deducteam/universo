module V = Elaboration.Var
module U = Common.Universes

type t =
  {
    sg:Signature.t;
    (** The current signature used for type checking *)
    md:Basic.mident;
    (** mident of the current module being type checked *)
    md_check:Basic.mident;
    (** mident of the file containing constraints for the current module *)
    md_elab:Basic.mident;
    (** mident of the file containing universe variables declaration for the current module *)
    meta_out:Dkmeta.cfg;
    (** Meta configuration to translate back universes of Universo to the original theory universes *)
    check_fmt:Format.formatter;
    (** Formatter where constraints are printed *)
  }

(** Only used as default value for [global_env] *)
let default : t = {sg = Signature.make "";
                   md = Basic.mk_mident "";
                   md_check = Basic.mk_mident "";
                   md_elab = Basic.mk_mident "";
                   meta_out=Dkmeta.default_config;
                   check_fmt=Format.std_formatter}

(** [globel_env] is a reference to the current type checking environment. *)
(* This is a reference because we have to use it in the Reduction Engine and we have no control over
   the interface *)
let global_env : t ref = ref default


module RE : Reduction.RE =
struct
  open Basic

  (** Name for rules that reduce variables. Names are irrelevant for Universo. *)
  let dummy_name = Rule.Gamma(false, mk_name !global_env.md_check (mk_ident "universo"))

  (** [add_rule vl vr] add to the current signature the rule that maps [vl] to [vr]. *)
  (* FIXME: this rules are not exported hence redudant rule might be added when the current module is      impoted somewhere else *)
  let rec add_rule  vl vr =
    let pat = Rule.Pattern(Basic.dloc,vl,[]) in
    let rhs = Term.mk_Const Basic.dloc vr in
    let rule = Rule.(
        {
          ctx = [];
          pat;
          rhs;
          name=dummy_name;
        })
    in
    Signature.add_rules !global_env.sg  [Rule.to_rule_infos rule]

  and whnf sg t =
    Reduction.default_reduction ~conv_test:are_convertible ~match_test:matching_test Reduction.Whnf sg t
  and snf sg t =
    Reduction.default_reduction ~conv_test:are_convertible ~match_test:matching_test Reduction.Snf sg t

  and univ_conversion l r =
    if Term.term_eq l r then (* should not happen *)
      true
    else
      try
        (* FIXME: should not be done in Universes anymore *)
        let uenv = U.({out_fmt= !global_env.check_fmt; meta= !global_env.meta_out}) in
        (* If two universes should be equal, then we add the constraint [l =?= r] AND a rule that
           makes [l] convertible to [r]. Order matters and is handled by the module U. *)
        if V.is_uvar l && V.is_uvar r then
          begin
            let ul = V.name_of_uvar l in
            let ur = V.name_of_uvar r in
            U.mk_var_cstr uenv add_rule ul ur;
            true
          end
          (* The witness of a universe constraint is always I. It's type should should be convertible to true. Knowing Dedukti behavior, the expected type is the left one (true) and the right one is the predicate to satisfy *)
          (* FIXME: we should not rely so tighly to the behavior of Dedukti. Moreover, I don't know how this behavior can be extended to other theories *)
        else if (Term.term_eq U.true_ l) then
          begin
            U.mk_cstr uenv r l;
            true
          end
        else
          false
      with U.Not_pred ->
        (* Encoding of cumulativity uses the rule lift s s a --> a. Hence, sometimes [lift ss a =?= a]. This case is not capture by the cases above. *)
        if U.is_lift l && not (U.is_lift r) then
          (* FIXME: to do stuff here *)
          true
        else if not (U.is_lift l) && (U.is_lift r) then
          (* FIXME: to do stuff here *)
          true
        else
          false

  and are_convertible_lst sg : (Term.term * Term.term) list -> bool = function
    | [] -> true
    | (l,r)::lst ->
      if Term.term_eq l r then are_convertible_lst sg lst
      else
        begin
          let l',r' = whnf sg l, whnf sg r in
          if univ_conversion l' r' then
            are_convertible_lst sg lst
          else
            are_convertible_lst sg (Reduction.conversion_step (l',r') lst)
        end

  and are_convertible sg t1 t2 =
    try are_convertible_lst sg [(t1,t2)]
    with Reduction.NotConvertible -> false

  (* FIXME: This should not be relevant anymore *)
  and matching_test r sg t1 t2 =
    match r with
    | Rule.Gamma(_,rn) ->
      if md rn = Basic.mk_mident "universo" then
        false
      else
        are_convertible sg t1 t2
    | _ -> are_convertible sg t1 t2
end

module T = Typing.Make(RE)

(** [mk_entry env e] type checks the entry e in the same way then dkcheck does. However, the convertibility tests is hacked so that we can add constraints dynamically while type checking the term. This is really close to what is done with typical ambiguity in Coq. *)
let mk_entry : t -> Entry.entry -> unit = fun env e ->
  let open Entry in
  let open Term in
  global_env := env;
  let _add_rules rs =
    let ris = List.map Rule.to_rule_infos rs in
    Signature.add_rules env.sg ris
  in
  match e with
  | Decl(lc,id,st,ty) ->
    Format.eprintf "[CHECK] %a@." Pp.print_ident id;
    Format.fprintf env.check_fmt "@.(; %a ;)@." Pp.print_ident id;
    begin
      match T.inference env.sg ty with
      | Kind | Type _ -> Signature.add_declaration env.sg lc id st ty
      | s -> raise (Typing.TypingError (Typing.SortExpected (ty,[],s)))
    end
  | Def(lc,id,opaque,mty,te) ->
    Format.eprintf "[CHECK] %a@." Pp.print_ident id;
    Format.fprintf env.check_fmt "@.(; %a ;)@." Pp.print_ident id;
    let open Rule in
    begin
      let ty = match mty with
        | None -> T.inference env.sg te
        | Some ty -> T.checking env.sg te ty; ty
      in
      match ty with
      | Kind -> raise (Env.EnvError (lc, Env.KindLevelDefinition id))
      | _ ->
        if opaque then Signature.add_declaration env.sg lc id Signature.Static ty
        else
          let _ = Signature.add_declaration env.sg lc id Signature.Definable ty in
          let cst = Basic.mk_name env.md id in
          let rule =
            { name= Delta(cst) ;
              ctx = [] ;
              pat = Pattern(lc, cst, []);
              rhs = te ;
            }
          in
          _add_rules [rule]
    end
  | Rules(_,rs) ->
    Format.fprintf env.check_fmt "@.(; RULES ;)@.";
    let _ = List.map (T.check_rule env.sg) rs in
    _add_rules rs
  | Require _ -> () (* FIXME: How should we handle a Require command? *)
  | _ -> assert false (* other commands are not supported by this is only by lazyness. *)
