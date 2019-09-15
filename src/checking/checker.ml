include    Common.Import
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
    constraints: (B.name, U.pred) Hashtbl.t;
    (** additional user constraints *)
    out_file: F.cout F.t
    (** File were constraints are written *)
  }

(** [globel_env] is a reference to the current type checking environment. *)
(* This is a reference because we have to use it in the Reduction Engine *)
let global_env : t option ref = ref None

let get = function None -> failwith "Environment not initialized" | Some env -> env

let of_global_env env = { C.file = env.out_file; C.meta = env.meta_out}

module MakeRE (Conv:Reduction.ConvChecker) : Reduction.S =
struct
  open Basic

  module R = Reduction.Make(Conv)

  include R

  (** Name for rules that reduce variables. Names are irrelevant for Universo. *)
  let dummy_name = Rule.Gamma(false, mk_name (mk_mident "dummy") (mk_ident "dummy"))

  (** [add_rule vl vr] add to the current signature the rule that maps [vl] to [vr]. *)
  (* FIXME: this rules are not exported hence redundant rules might be added when the current module is      impoted somewhere else *)
  let rec add_rule vl vr =
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
    Signature.add_rules (get !global_env).sg  [Rule.to_rule_infos rule]

  and univ_conversion l r =
    if Term.term_eq l r then
      true
    else
        (* If two universes should be equal, then we add the constraint [l =?= r] AND a rule that
           makes [l] convertible to [r]. Order matters and is handled by the module U. *)
        if V.is_uvar l && V.is_uvar r then
          C.mk_cstr (of_global_env (get !global_env))  add_rule (U.EqVar(V.name_of_uvar l, V.name_of_uvar r))

        else if V.is_uvar l && U.is_enum r then
          let r = U.extract_univ r in
          ignore(C.mk_cstr (of_global_env (get !global_env)) add_rule (U.Pred(U.Cumul(Var (V.name_of_uvar l),r))));
          C.mk_cstr (of_global_env (get !global_env)) add_rule (U.Pred(U.Cumul(r, Var (V.name_of_uvar l))))
        else if V.is_uvar r && U.is_enum l then
          let l = U.extract_univ l in
          ignore(C.mk_cstr (of_global_env (get !global_env)) add_rule (U.Pred(U.Cumul(Var (V.name_of_uvar r),l))));
          C.mk_cstr (of_global_env (get !global_env)) add_rule (U.Pred(U.Cumul(l, Var (V.name_of_uvar r))))
          (* The witness of a universe constraint is always I. It's type should should be convertible to true. Knowing Dedukti behavior, the expected type is the left one (true) and the right one is the predicate to satisfy *)
        else if (Term.term_eq (U.true_ ()) l) then
          if U.is_subtype r then
            let s = U.extract_subtype r in
            are_convertible (get !global_env).sg  (U.true_ ()) s
          else if U.is_forall r then
            let s = U.extract_forall r in
            are_convertible (get !global_env).sg (U.true_ ()) s
          else
            C.mk_cstr (of_global_env (get !global_env)) add_rule (U.Pred(U.extract_pred r))
          (* Encoding of cumulativity uses the rule cast _ _ A A t --> t. Hence, sometimes [lift ss a =?= a]. This case is not capture by the cases above. This quite ugly to be so dependent of that rule, but I have found no nice solution to resolve that one. *)
        else if U.is_cast' l && not (U.is_cast' r) then
          let _,_,a,b,t = U.extract_cast' l in
          are_convertible (get !global_env).sg a b &&
          are_convertible (get !global_env).sg t r
        else if not (U.is_cast' l) && (U.is_cast' r) then
          let _,_,a,b,t = U.extract_cast' r in
          are_convertible (get !global_env).sg a b &&
          are_convertible (get !global_env).sg l t
        else
          false

  and are_convertible_lst sg : (Term.term * Term.term) list -> bool = function
    | [] -> true
    | (l,r)::lst ->
      if Term.term_eq l r then  are_convertible_lst sg lst
      else
        begin
          (*Format.printf "l:%a@." Pp.print_term l;
            Format.printf "r:%a@." Pp.print_term r; *)
          let l',r' = whnf sg l, whnf sg r in
          (* Format.printf "l':%a@." Pp.print_term l';
             Format.printf "r':%a@." Pp.print_term r'; *)
          if univ_conversion l' r' then
            are_convertible_lst sg lst
          else
            are_convertible_lst sg (R.conversion_step (l',r') lst)
        end

  and are_convertible sg t1 t2 =
    try are_convertible_lst sg [(t1,t2)]
    with Reduction.NotConvertible ->
      false

  and matching_test cstr r sg t1 t2 =
    if Term.term_eq t1 t2 then
      true
    else
      match cstr,r with
      | Rule.Linearity _, Rule.Gamma(_,rn) ->
        (* We need to avoid non linear rule of the theory otherwise we may produce inconsistent constraints: lift s s' a should not always reduce to a.*)
        if not (Str.string_match (Str.regexp "cast'") (string_of_ident (id rn)) 0) then
          are_convertible sg t1 t2
        else
          false
      | Rule.Bracket _, _ -> are_convertible sg t1 t2
      | _ -> assert false
end

module rec RE : Reduction.S = MakeRE(RE)

module T = Typing.Make(RE)

let check_user_constraints : (B.name, U.pred) Hashtbl.t -> B.name -> Term.term -> unit =
  fun constraints name ty ->
    let get_uvar ty =
      match ty with
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
      ignore(C.mk_cstr (of_global_env (get !global_env)) (fun _ -> assert false) (U.Pred (replace pred)))

(** [mk_entry env e] type checks the entry e in the same way then dkcheck does. However, the convertibility tests is hacked so that we can add constraints dynamically while type checking the term. This is really close to what is done with typical ambiguity in Coq. *)
let mk_entry : t -> Entry.entry -> unit = fun env e ->
  let open Entry in
  let open Term in
  global_env := Some env;
  let _add_rules rs =
    let ris = List.map Rule.to_rule_infos rs in
    Signature.add_rules env.sg ris
  in
  match e with
  | Decl(lc,id,st,ty) ->
    L.log_check "[CHECK] %a" Pp.print_ident id;
    check_user_constraints env.constraints (Basic.mk_name (F.md_of env.in_path `Output) id) ty;
    Format.fprintf (F.fmt_of_file env.out_file) "@.(; %a ;)@." Pp.print_ident id;
    begin
        match T.inference env.sg ty with
        | Kind | Type _ -> Signature.add_declaration env.sg lc id st ty
        | s -> raise (Typing.TypingError (Typing.SortExpected (ty,[],s)))
    end
  | Def(lc,id,opaque,mty,te) ->
    L.log_check "[CHECK] %a" Pp.print_ident id;
    Format.fprintf (F.fmt_of_file env.out_file) "@.(; %a ;)@." Pp.print_ident id;
    let open Rule in
    begin
      let ty = match mty with
        | None -> T.inference env.sg te
        | Some ty -> T.checking env.sg te ty; ty
      in
      match ty with
      | Kind -> raise (Env.EnvError (Some(EE.get_name ()), lc, Env.KindLevelDefinition id))
      | _ ->
        if opaque then Signature.add_declaration env.sg lc id Signature.Static ty
        else
          let _ = Signature.add_declaration env.sg lc id Signature.Definable ty in
          let cst = Basic.mk_name (F.md_of (get !global_env).in_path `Output) id in
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
    let open Rule in
    let _ = List.map (fun (r: untyped_rule)  ->
         Format.fprintf (F.fmt_of_file env.out_file) "@.(; %a ;)@." Rule.pp_rule_name r.name;
        T.check_rule env.sg r) rs in
    _add_rules rs
  | Require _ -> () (* FIXME: How should we handle a Require command? *)
  | _ -> assert false (* other commands are not supported by this is only by lazyness. *)
