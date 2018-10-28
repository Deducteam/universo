type env =
  {
    sg:Signature.t;
    md:Basic.mident
  }


module RE : Reduction.RE =
struct
  open Basic

  let rec mk_rule env ?(name=Rule.Gamma(false, mk_name env.md (mk_ident "universo"))) l r =
    let pat = failwith "todo" in
    let rhs = failwith "todo" in
    let rule = Rule.(
        {
          ctx = [];
          pat;
          rhs;
          name;
        })
    in
    rule

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
        let l' = Universes.extract_univ l in
        let r' = Universes.extract_univ r in
        ignore(l');
        ignore(r');
        true
      with Universes.Not_univ ->
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

include T

let mk_entry : env -> Entry.entry -> unit = fun env e ->
  let open Entry in
  let open Term in
  let _add_rules rs =
    let ris = List.map Rule.to_rule_infos rs in
    Signature.add_rules env.sg ris
  in
  match e with
  | Decl(lc,id,st,ty) ->
    Format.eprintf "[CHECK] on :%a@." Pp.print_ident id;
    begin
      match inference env.sg ty with
      | Kind | Type _ -> Signature.add_declaration env.sg lc id st ty
      | s -> raise (Typing.TypingError (Typing.SortExpected (ty,[],s)))
    end
  | Def(lc,id,opaque,mty,te) ->
    Format.eprintf "[CHECK] on :%a@." Pp.print_ident id;
    let open Rule in
    begin
      let ty = match mty with
        | None -> inference env.sg te
        | Some ty -> checking env.sg te ty; ty
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
  | Rules(lc,rs) ->
    let _ = List.map (check_rule env.sg) rs in
    _add_rules rs
  | _ -> assert false
