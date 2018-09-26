open Basic
open Term

module type S =
  sig
    include Typing.Typer

    val mk_entry : Configuration.t -> Entry.entry -> unit
  end

let sg_check = ref (Signature.make "")
let md_univ = ref (mk_mident "")

module MakeRE(T:Theory.S) : Reduction.RE =
  struct
    open Reduction
    open Configuration

    let metaify t =
      let open Dkmeta in
      Dkmeta.mk_term T.meta t

    let whnf = Reduction.REDefault.whnf
    let snf = Reduction.REDefault.snf

    let universo = mk_mident "universo"

(*
    let cpt_var = ref 0
    let cpt_var_left = ref 0
    let cpt_var_right = ref 0
    let cpt_max = ref 0
*)
    let rec univ_conversion l r =
      let open Universes in
      let snf = snf !sg_check in
      if Term.term_eq l r then
        true
      else if is_uvar l && is_uvar r then
        (
          Format.eprintf "l:%a@.r:%a@." Pp.print_term (snf l) Pp.print_term (snf r);
          let var = match r with | Const(_,n) -> n | _ -> assert false in
          let lhs = Rule.Pattern(Basic.dloc, var, []) in
          let rhs = l in
          let rule : Rule.untyped_rule = Rule.(
            {
              ctx = [];
              pat = lhs;
              rhs;
              name = Gamma(false, mk_name !md_univ (mk_ident "coucou"))
            })
          in
          Signature.add_rules !sg_check [(Rule.to_rule_infos rule)];
          true
        )
      else if is_uvar l && is_max r then
        (
          Format.eprintf "l:%a@.r:%a@." Pp.print_term (snf l) Pp.print_term (snf r);
          true
        )
      else if is_uvar r && is_max l then
        (
          Format.eprintf "l:%a@.r:%a@." Pp.print_term (snf l) Pp.print_term (snf r);
          univ_conversion r l
        )
      else if is_uvar r then
        (
          Format.eprintf "l:%a@.r:%a@." Pp.print_term (snf l) Pp.print_term (snf r);
          (*  univ_conversion r l *)
          true
        )
      else if is_max l && is_max r then
        (
          Format.eprintf "l:%a@.r:%a@." Pp.print_term (snf l) Pp.print_term (snf r); (*
          let ul = to_universo (snf l) in
          let lhs = pattern_of_univ ul in
          let rhs = snf r in
          let rule : Rule.untyped_rule = Rule.(
            {
              ctx = [];
              pat = lhs;
              rhs;
              name = Gamma(false, mk_name !md_univ (mk_ident "coucou"))
            })
          in
          Signature.add_rules !sg_check [(Rule.to_rule_infos rule)]; *)
          true
        )
      else if is_lift l && not (is_lift r) then
        (
          Format.eprintf "l:%a@.r:%a@." Pp.print_term (snf l) Pp.print_term (snf r);
          true
        )
      else if not (is_lift l) && is_lift r then
        (
          Format.eprintf "l:%a@.r:%a@." Pp.print_term (snf l) Pp.print_term (snf r);
          true
        )
      else
        (
          Format.eprintf "FAIL: l:%a@.r:%a@."
            Pp.print_term (snf l) Pp.print_term (snf r);
          false
        )

    let rec are_convertible_lst sg : (term*term) list -> bool = function
      | [] -> true
      | (l,r)::lst ->
         if term_eq l r then are_convertible_lst sg lst
         else
           begin
             Format.eprintf "left:%a@.right:%a@." Pp.print_term l Pp.print_term r;
             let l',r' = whnf sg l, whnf sg r in
             (*           Format.eprintf "left:%a@.right:%a@." Pp.print_term l' Pp.print_term r'; *)
             if univ_conversion l' r' then
               are_convertible_lst sg lst
             else
               begin
                 (*        Format.eprintf "left:%a@.right:%a@." Pp.print_term l' Pp.print_term r'; *)
                 are_convertible_lst sg (Reduction.conversion_step (l',r') lst)
               end
           end

    let are_convertible sg t1 t2 =
      try are_convertible_lst sg [(t1,t2)]
      with NotConvertible -> false
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
