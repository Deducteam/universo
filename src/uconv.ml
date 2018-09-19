open Basic
open Term

let sg_meta = ref (Signature.make "")
let md_theory = ref (mk_mident "")

let init : Signature.t -> Basic.mident -> unit = fun sg md ->
  sg_meta := sg;
  md_theory := md

module type S =
  sig
    include Typing.Typer

    val mk_entry : Configuration.t -> Entry.entry -> unit
  end

module MakeRE(TH:Theory.Th) : Reduction.RE =
  struct
    open Reduction
    open Configuration

    let theory () = !md_theory

    let metaify t =
      let open Dkmeta in
      let cfg_meta = {default_config with encoding = Some (module LF); sg = !sg_meta} in
      cfg_meta.meta_rules <- Some(List.map (fun (r:Rule.untyped_rule) -> r.Rule.name) TH.rules);
      Dkmeta.mk_term cfg_meta t

    let whnf = Reduction.REDefault.whnf
    let snf = Reduction.REDefault.snf

    let rec zip_lists l1 l2 lst =
      match l1, l2 with
      | [], [] -> lst
      | s1::l1, s2::l2 -> zip_lists l1 l2 ((s1,s2)::lst)
      | _,_ -> raise NotConvertible

    let univ_conversion l r =
      if Term.term_eq l r then
        []
      else
        (
          Format.eprintf "l:%a@.r:%a@." Pp.print_term l Pp.print_term r;
          failwith "yes"
        )

    let rec are_convertible_lst sg : (term*term) list -> bool = function
      | [] -> true
      | (l,r)::lst ->
         if term_eq l r then are_convertible_lst sg lst
         else
           let l',r' = whnf sg l, whnf sg r in
           Format.eprintf "left:%a@.right:%a@." Pp.print_term l' Pp.print_term r';
           are_convertible_lst sg (Reduction.conversion_step (l',r') lst)

    let are_convertible sg t1 t2 =
      try are_convertible_lst sg [(t1,t2)]
      with NotConvertible -> false
  end

module Make(TH:Theory.Th) : S =
  struct

    module R = MakeRE(TH)

    module T = Typing.Make(R)

    include T

    let mk_entry : Configuration.t -> Entry.entry -> unit = fun cfg e ->
      let open Entry in
      let open Configuration in
      let sg = cfg.sg in
      let md = cfg.md in
      let _add_rules rs =
        let ris = List.map Rule.to_rule_infos rs in
        Signature.add_rules sg ris
      in
      match e with
      | Decl(lc,id,st,ty) ->
         begin
           match inference sg ty with
           | Kind | Type _ -> Signature.add_declaration sg lc id st ty
           | s -> raise (Typing.TypingError (Typing.SortExpected (ty,[],s)))
         end
      | Def(lc,id,opaque,mty,te) ->
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
