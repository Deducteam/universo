module B = Basic
module F = Common.Files
module L = Common.Log
module V = Elaboration.Var
module U = Common.Universes
module C = Common.Constraints

type t =
  {
    sg:Signature.t;
    (** The current signature used for type checking *)
    in_path:F.path;
    (** path of the original file that should be typed checked *)
    meta_out:Dkmeta.cfg;
    (** Meta configuration to translate back universes of Universo to the original theory universes *)
    constraints: (B.name, U.pred) Hashtbl.t
    (** additional user constraints *)
  }

(** Only used as default value for [global_env] *)
let default : t = {sg          = Signature.make "";
                   in_path     = "";
                   meta_out    = Dkmeta.default_config;
                   constraints = Hashtbl.create 11}

(** [globel_env] is a reference to the current type checking environment. *)
(* This is a reference because we have to use it in the Reduction Engine and we have no control over
   the interface *)
let global_env : t ref = ref default


module RE : Reduction.RE =
struct
  open Basic

  (** Name for rules that reduce variables. Names are irrelevant for Universo. *)
  let dummy_name = Rule.Gamma(false, mk_name (mk_mident "dummy") (mk_ident "dummy"))

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
        (* If two universes should be equal, then we add the constraint [l =?= r] AND a rule that
           makes [l] convertible to [r]. Order matters and is handled by the module U. *)
        if V.is_uvar l && V.is_uvar r then
          C.mk_cstr add_rule (U.EqVar(V.name_of_uvar l, V.name_of_uvar r))
          (* The witness of a universe constraint is always I. It's type should should be convertible to true. Knowing Dedukti behavior, the expected type is the left one (true) and the right one is the predicate to satisfy *)
          (* FIXME: we should not rely so tighly to the behavior of Dedukti. Moreover, I don't know how this behavior can be extended to other theories *)
        else if (Term.term_eq U.true_ l) then
          C.mk_cstr add_rule (U.Pred(U.extract_pred r))
          (* Encoding of cumulativity uses the rule lift s s a --> a. Hence, sometimes [lift ss a =?= a]. This case is not capture by the cases above. This quite ugly to be so dependent of that rule, but I have found no nice solution to resolve that one. *)
        else if U.is_lift' l && not (U.is_lift' r) then
          let s1,s2 = U.extract_lift' l in
          (* We have to compute there whnf because lift' s s' is already a whnf but not s and s' *)
          let s1,s2 = whnf !global_env.sg s1, whnf !global_env.sg s2 in
          if Reduction.are_convertible !global_env.sg s1 s2 then
            true
          else (
            assert (V.is_uvar s1 && V.is_uvar s2);
            C.mk_cstr add_rule (U.EqVar(V.name_of_uvar s1, V.name_of_uvar s2)))
        else if not (U.is_lift' l) && (U.is_lift' r) then
          let s1,s2 = U.extract_lift' r in
          let s1,s2 = whnf !global_env.sg s1, whnf !global_env.sg s2 in
          if Reduction.are_convertible !global_env.sg s1 s2 then
            true
          else (
            assert (V.is_uvar s1 && V.is_uvar s2);
            C.mk_cstr add_rule (U.EqVar(V.name_of_uvar s1, V.name_of_uvar s2)))
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
    if Term.term_eq t1 t2 then
      true
    else
      match r with
      | Rule.Gamma(_,rn) ->
        (* We need to avoid non linear rule of the theory otherwise we may produce inconsistent constraints: lift s s' a should not always reduce to a. *)
        (* FIXME: this is a bug of dkmeta that rule names are not preserved *)
        if (md rn) = F.md_of_path !F.theory  then
          false
        else
          are_convertible sg t1 t2
      | Rule.Delta(_) ->
        are_convertible sg t1 t2
      | Rule.Beta -> assert false
end

module T = Typing.Make(RE)

let check_user_constraints : (B.name, U.pred) Hashtbl.t -> B.name -> Term.term -> unit =
  fun constraints name ty ->
    let get_uvar ty =
      match ty with
      (* this line higly depends on the encoding :'( *)
      | Term.App(_, (Term.Const(_,name) as t) ,_) when V.is_uvar t -> name
      | _ -> assert false
    in
    if Hashtbl.mem constraints name then
      let pred = Hashtbl.find constraints name in
      let uvar = get_uvar ty in
      let replace_univ : U.univ -> U.univ =
        function
        | Var _ -> Var uvar
        | _ as t -> t
      in
      let replace : U.pred -> U.pred =
        function
        | Axiom(s,s') -> Axiom(replace_univ s, replace_univ s')
        | Cumul(s,s') -> Cumul(replace_univ s, replace_univ s')
        | Rule(s,s',s'') -> Rule(replace_univ s, replace_univ s', replace_univ s'')
      in
      ignore(C.mk_cstr (fun _ -> assert false) (U.Pred (replace pred)))

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
    L.log_check "[CHECK] %a" Pp.print_ident id;
    check_user_constraints env.constraints (Basic.mk_name (F.md_of env.in_path `Output) id) ty;
    (* Format.fprintf env.check_fmt "@.(; %a ;)@." Pp.print_ident id; *)
    begin
      match T.inference env.sg ty with
      | Kind | Type _ -> Signature.add_declaration env.sg lc id st ty
      | s -> raise (Typing.TypingError (Typing.SortExpected (ty,[],s)))
    end
  | Def(lc,id,opaque,mty,te) ->
    L.log_check "[CHECK] %a" Pp.print_ident id;
    (* Format.fprintf env.check_fmt "@.(; %a ;)@." Pp.print_ident id; *)
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
          let cst = Basic.mk_name (F.md_of !global_env.in_path `Output) id in
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
    let _ = List.map (T.check_rule env.sg) rs in
    _add_rules rs
  | Require _ -> () (* FIXME: How should we handle a Require command? *)
  | _ -> assert false (* other commands are not supported by this is only by lazyness. *)
