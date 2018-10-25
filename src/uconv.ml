open Basic
open Term

module type S =
  sig
    include Typing.Typer

    val mk_entry : Configuration.t -> Entry.entry -> unit
  end

let sg_check = ref (Signature.make "")
let md_univ = ref (mk_mident "")
(*
module MakeRE(T:Theory.S) : Reduction.RE =
  struct
    open Reduction
    open Configuration

    let metaify t =
      let open Dkmeta in
      Dkmeta.mk_term T.meta t

    let cpt = ref 0

    let rec matching_test r sg t1 t2 =
      match r with
      | Rule.Gamma(_,rn) ->
        if md rn = Basic.mk_mident "cic" then
          false
        else
          are_convertible sg t1 t2
      | _ -> are_convertible sg t1 t2

    and whnf sg t= Reduction.default_reduction ~conv_test:are_convertible ~match_test:matching_test Reduction.Whnf sg t
    and snf sg t = Reduction.default_reduction ~conv_test:are_convertible ~match_test:matching_test Reduction.Snf sg t

    and mk_rule ?(name=Rule.Gamma(false, mk_name !md_univ (mk_ident "universo"))) l r =
      let pat = Universes.pattern_of_univ l in
      let rhs = snf !sg_check (Universes.term_of_univ r) in
      let rule = Rule.(
      {
        ctx = [];
        pat;
        rhs;
        name;
      })
      in
      rule

    and add_rule rule = Signature.add_rules !sg_check [Rule.to_rule_infos rule]

    and univ_conversion l r =
      let snf = snf !sg_check in
      let open Universes in
      if Term.term_eq l r then
          true
      else
        let l = snf l in
        let r = snf r in
        if Term.term_eq l r then
          true
        else
        if Universes.is_uvar l && is_uvar r then (
          let l = Universes.extract_univ l in
          let r = Universes.extract_univ r in
          add_rule (mk_rule l r);
          true)
        else
          try
            (* TODO: Fix Universes.is_lift and elaboration *)
            let l = Universes.extract_univ l in
            let r = Universes.extract_univ r in
            if Universes.gt l r then
              (* add_rule (mk_rule l r); *)
              true
            else if Universes.gt r l then
              true
            else
                assert false (* Order should be total *)
          with Universes.Not_univ ->
            if is_lift l && not (is_lift r) then
              true
            else if not (is_lift l) && (is_lift r) then
              univ_conversion r l
            else
              false

    and are_convertible_lst sg : (term*term) list -> bool = function
      | [] -> true
      | (l,r)::lst ->
         if term_eq l r then are_convertible_lst sg lst
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
      with NotConvertible -> false

  end
  *)

module MakeRE(T:Theory.S) : Reduction.RE =
struct


  let rec mk_rule ?(name=Rule.Gamma(false, mk_name !md_univ (mk_ident "universo"))) l r =
    let pat = Universes.pattern_of_univ l in
    let rhs = Universes.term_of_univ r in
    let rule = Rule.(
        {
          ctx = [];
          pat;
          rhs;
          name;
        })
    in
    rule

  and whnf sg t= Reduction.default_reduction ~conv_test:are_convertible ~match_test:matching_test Reduction.Whnf sg t
  and snf sg t = Reduction.default_reduction ~conv_test:are_convertible ~match_test:matching_test Reduction.Snf sg t

  and add_rule rule = Signature.add_rules !sg_check [Rule.to_rule_infos rule]

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

  and are_convertible_lst sg : (term*term) list -> bool = function
    | [] -> true
    | (l,r)::lst ->
      if term_eq l r then are_convertible_lst sg lst
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
      if md rn = Basic.mk_mident "cic" then (* Bug to fix: cic should not be here *)
        false
      else
        are_convertible sg t1 t2
    | _ -> are_convertible sg t1 t2
end

module Make(T:Theory.S) : S =
  struct

    module R = MakeRE(T)

    module T = Typing.Make(R)

    include T

    let mk_entry : Configuration.t -> Entry.entry -> unit = fun cfg e ->
      let open Entry in
      let open Configuration in
      let sg = cfg.sg_check in
      let md = cfg.md_check in
      sg_check := cfg.sg_check;
      md_univ := cfg.md_univ;
      Universes.md_univ := cfg.md_univ;
      let _add_rules rs =
        let ris = List.map Rule.to_rule_infos rs in
        let ris = List.map (fun rs -> {rs with Rule.constraints = []}) ris in
        Signature.add_rules sg ris
      in
      match e with
      | Decl(lc,id,st,ty) ->
         Format.eprintf "[CHECK] on :%a@." Pp.print_ident id;
         begin
           match inference sg ty with
           | Kind | Type _ -> Signature.add_declaration sg lc id st ty
           | s -> raise (Typing.TypingError (Typing.SortExpected (ty,[],s)))
         end
      | Def(lc,id,opaque,mty,te) ->
        Format.eprintf "[CHECK] on :%a@." Pp.print_ident id;
         let open Rule in
         begin
           let ty = match mty with
             | None -> inference sg te
             | Some ty -> checking sg te ty; ty
           in
           match ty with
           | Kind -> raise (Env.EnvError (lc, Env.KindLevelDefinition id))
           | _ ->
              if opaque then Signature.add_declaration sg lc id Signature.Static ty
              else
                let _ = Signature.add_declaration sg lc id Signature.Definable ty in
                let cst = mk_name md id in
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
         let _ = List.map (check_rule sg) rs in
         _add_rules rs
      | _ -> assert false
  end
