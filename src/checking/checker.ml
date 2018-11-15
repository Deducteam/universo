type t =
  {
    sg:Signature.t;
    md:Basic.mident;
    md_check:Basic.mident;
    md_elab:Basic.mident;
    meta:Dkmeta.cfg;
    meta_out:Dkmeta.cfg;
    check_fmt:Format.formatter;
  }

let default : t = {sg = Signature.make "";
                   md = Basic.mk_mident "";
                   md_check = Basic.mk_mident "";
                   md_elab = Basic.mk_mident "";
                   meta=Dkmeta.default_config;
                   meta_out=Dkmeta.default_config;
                   check_fmt=Format.std_formatter}

let global_env : t ref = ref default

module V = Elaboration.Var

module RE : Reduction.RE =
struct
  open Basic

  let default_name = Rule.Gamma(false, mk_name !global_env.md_check (mk_ident "universo"))

  let rec add_rule  vl vr =
    let pat = Rule.Pattern(Basic.dloc,vl,[]) in
    let rhs = Term.mk_Const Basic.dloc vr in
    let rule = Rule.(
        {
          ctx = [];
          pat;
          rhs;
          name=default_name;
        })
    in
    Signature.add_rules !global_env.sg  [Rule.to_rule_infos rule]

  and whnf sg t =
    Reduction.default_reduction ~conv_test:are_convertible ~match_test:matching_test Reduction.Whnf sg t
  and snf sg t =
    Reduction.default_reduction ~conv_test:are_convertible ~match_test:matching_test Reduction.Snf sg t

  and univ_conversion l r =
    let open Universes in
    if Term.term_eq l r then
      true
    else
      try
        begin
          if V.is_uvar l && V.is_uvar r then
            add_rule (V.name_of_uvar l) (V.name_of_uvar r);
          Universes.mk_var_cstr uenv l r
        end;
        let uenv = Universes.({out_fmt= !global_env.check_fmt; meta= !global_env.meta_out}) in
        Universes.mk_cstr uenv l r
        (* let l' = Universes.to_univ !global_env.md_elab  *)

(*        let l = snf l in
        let r = snf r in
        if l = r then true
        else
          let l' = Universes.extract_univ !global_env.md_elab l in
          let r' = Universes.extract_univ !global_env.md_elab r in
          begin
            if Universes.is_var !global_env.md_elab l && Universes.is_var !global_env.md_elab r then
              if Universes.gt l' r' then
                add_rule l' r'
              else
                add_rule r' l'
          end;
          let uenv = Universes.({out_fmt= !global_env.check_fmt; meta= !global_env.meta_out}) in
          mk_cstr uenv l' r';
          true *)
      with Universes.Not_pred ->
        if is_lift l && not (is_lift r) then
          true
        else if not (is_lift l) && (is_lift r) then
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
  | Require _ ->
    () (* FIXME: right now, only Universo generates REQUIRE commands *)
  | _ -> assert false
