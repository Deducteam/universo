open Basic


type constraints =
  | Univ of ident * int
  | Eq of ident * ident
  | Max of ident * ident * ident
  | Succ of ident * ident
  | Rule of ident * ident * ident


module type Generation =
sig

  type t
  val generate : Basic.ident list -> Parser.entry -> t

end

(* This module is here because I have to go manually under the context using are_convertible *)
module Dk =
struct
  let is_prod t =
    match t with
    | Term.Pi _ -> true
    | _ -> false

  let extract_prod t =
    match t with
    | Term.Pi(_,x,ty,te) -> x,ty,te
    | _ -> failwith "is not a dedukti product"

  let is_app t =
    match t with
    | Term.App _ -> true
    | _ -> false

    let extract_app t =
    match t with
    | Term.App(f,a,args) -> f,a,args
    | _ -> failwith "is not a dedukti application"
end

module Red =
struct

  open Term

  let whnf te =
    Env.unsafe_reduction te

  let rec add_to_list2 l1 l2 lst =
    match l1, l2 with
    | [], [] -> OK lst
    | s1::l1, s2::l2 -> add_to_list2 l1 l2 ((s1,s2)::lst)
    | _,_ -> assert false

  let rec are_convertible_lst to_check not_convertible =
    match to_check with
    | [] -> not_convertible
    | (t1,t2)::lst ->
      begin
        match (
          if term_eq t1 t2 then OK lst
          else
            match whnf t1, whnf t2 with
            | Kind, Kind | Type _, Type _ -> OK lst
            | Const (_,n), Const (_,n') when name_eq n n' -> OK lst
            | DB (_,_,n), DB (_,_,n') when  n==n' -> OK lst
            | App (f,a,args), App (f',a',args') when List.length args = List.length args' ->
              add_to_list2 args args' ((f,f')::(a,a')::lst)
            | Lam (_,_,_,b), Lam (_,_,_,b') -> OK ((b,b')::lst)
            | Pi (_,_,a,b), Pi (_,_,a',b') -> OK ((a,a')::(b,b')::lst)
            | t1, t2 -> Err(t1,t2)
        ) with
        | Err(t1,t2) -> are_convertible_lst lst ((t1,t2)::not_convertible)
        | OK lst2 -> are_convertible_lst lst2 not_convertible
      end

  let are_convertible l r =
    let l = are_convertible_lst [(l,r)] [] in
    match l with
    | [] -> None
    | _ -> Some l
end

module CS  = Set.Make(struct type t = constraints let compare = compare end)

module Naive : Generation with type t = CS.t =
struct
  open Basic
  open Parser
  open Rule

  type t = CS.t

  let rec pattern_of_term t =
    match t with
    | Term.Const(lc, name) ->
      Pattern(lc, name, [])
    | Term.App(Term.Const(lc,name),a,args) ->
      let pats = List.map pattern_of_term (a::args) in
      Pattern(lc, name, pats)
    | _ -> failwith "pattern reconstruction failure"

  let mk_rule l r =
    let r =
      {
        name = Gamma(true, mk_name (mk_mident "to fix") (mk_ident "to fix"));
        ctx = [];
        pat = pattern_of_term l;
        rhs = r
      }
    in
    match Env.add_rules [r] with
    | OK _ -> ()
    | Err err ->  Errors.fail_env_error err

  (* Translate a predicative universe to a integer *)
  let rec int_of_type te =
    if Cic.is_z te then
      0
    else if Cic.is_s te then
      let te' = Cic.extract_s te in
      1+(int_of_type te')
    else
      assert false

  let are_convertible  l r =
    match Red.are_convertible l r with
    | Some l -> false
    | None -> true


  (* Create as many necessary variable to handle complicated constraints *)
  let rec to_variable t =
    let term_of_ident id = Term.mk_Const dloc (mk_name (Env.get_name ()) id) in
    let open Uvar in
    let open Cic  in
    if is_uvar t then
      ident_of_uvar t, CS.empty
    else if is_succ t then
      let t', cs = to_variable (extract_succ t) in
      let nv = ident_of_uvar @@ Uvar.fresh_uvar () in
      mk_rule t (term_of_ident nv);
      nv, CS.add (Succ(t',nv)) cs
    else if is_rule t then
      let l,r = extract_rule t in
      let l',cs = to_variable l in
      let r',cs' = to_variable r in
      let nv = ident_of_uvar @@ Uvar.fresh_uvar () in
      mk_rule t (term_of_ident nv);
      nv, CS.add (Rule(l',r',nv)) (CS.union cs cs')
    else if is_max t then
      let l,r = extract_max t in
      let l',cs = to_variable l in
      let r',cs' = to_variable r in
      let nv = ident_of_uvar @@ Uvar.fresh_uvar () in
      mk_rule t (term_of_ident nv);
      nv, CS.add (Max(l',r',nv)) (CS.union cs cs')
    else if is_type t then
      let t' = extract_type t in
      let i = int_of_type t' in
      let nv = ident_of_uvar @@ Uvar.fresh_uvar () in
      mk_rule (term_of_ident nv) t;
      nv, CS.singleton (Univ(nv,i))
    else
      begin
        Format.printf "%a@." Pp.print_term t;
        assert false
      end

  (* Invariant : l and r are two terms that represent a universe on the algebra Succ, Max, Rule *)
  let rec get_constraint l r : CS.t option =
(*    Format.printf "left: %a@." Pp.print_term l;
n      Format.printf "right: %a@." Pp.print_term r; *)
    let open Cic in
    let open Uvar in
    let l,r =
      if Cic.is_univ l && Cic.is_univ r then
        let l' = Cic.extract_univ l in
        let r' = Cic.extract_univ r in
        l',r'
      else l,r
    in
    if are_convertible l r then None
    else
      begin
        if is_uvar l && is_uvar r then
          begin
            mk_rule l r;
            let ul = ident_of_uvar l in
            let ur = ident_of_uvar r in
            Some(CS.singleton @@ Eq(ul,ur))
          end
        else if is_succ l && is_uvar r then
          begin
            mk_rule l r;
            let l = extract_succ l in
            let ul,cs = to_variable l in
            let ur = ident_of_uvar r in
            Some(CS.add (Succ(ul,ur)) cs)
          end
        else if is_uvar l && is_succ r then
          get_constraint r l
        else if is_uvar l && is_type r then
          begin
            mk_rule l r;
            let ul = ident_of_uvar l in
            let r' = extract_type r in
            let i = int_of_type r' in
            Some(CS.singleton @@ Univ(ul,i))
          end
        else if is_type l && is_uvar r then
          get_constraint r l
        else if is_rule l && is_uvar r then
          begin
            mk_rule l r;
            let l',cs  = to_variable l  in
            let r' = ident_of_uvar r    in
            Some(CS.add (Eq(l',r')) cs)
        end
        else if is_uvar l && is_rule r then
          get_constraint r l
        else if is_rule l && is_rule r then
          begin
            mk_rule l r;
            let l',cs  = to_variable l in
            let r',cs' = to_variable r in
            Some(CS.add (Eq(l',r')) (CS.union cs cs'))
          end
        else if is_uvar l && is_max r then
          begin
            mk_rule r l;
            let l' = ident_of_uvar l in
            let r', cs = to_variable r in
            Some(CS.add (Eq(l',r')) cs)
          end
        else if is_max l && is_uvar r then
          get_constraint r l
        else if is_max l && is_max r then
          begin
            mk_rule l r;
            let l',cs  = to_variable l in
            let r',cs' = to_variable r in
            Some(CS.add (Eq(l',r')) (CS.union cs cs'))
          end
        else
          begin
            Format.printf "left:%a@." Pp.print_term l;
            Format.printf "right:%a@." Pp.print_term r;
            failwith "todo constraints"
          end
      end

  let add_opt cs mc =
    match mc with
    | None -> cs
    | Some c -> CS.union c cs

  (* Two cases might happen when the convertibility fails *)
  let check_convertibility l r =
    match Red.are_convertible l r with
    | None -> assert false
    | Some l -> Some (List.fold_left (fun cs (l,r) -> add_opt cs (get_constraint l r)) CS.empty l)

  (* A wrapper around the convertibility error of the kernel *)
  let rec constraint_of_error err =
    match err with
    | Typing.ConvertibilityError(term,ctx,l,r) ->
(*      Format.printf "left:%a@." Pp.print_term l;
        Format.printf "right:%a@." Pp.print_term r; *)
      check_convertibility l r
    | _ -> Errors.fail_typing_error err

  (* Generate constraints from an entry *)
  let rec generate e cs =
    match e with
    | Decl(l,id,st,t) ->
      begin
        match Env.declare l id st t with
        | OK () -> cs
        | Err(Env.EnvErrorType(ty)) ->
          let mc = constraint_of_error ty in
          generate e (add_opt cs mc)
        | Err(_) -> assert false
      end
    | Def(l,id,op,pty,te) ->
      let define = if op then Env.define_op else Env.define in
      begin
        match define l id te pty with
        | OK () -> cs
        | Err(Env.EnvErrorType(ty)) ->
          let mc = constraint_of_error ty in
          generate e (add_opt cs mc)
        | Err(_) -> assert false
      end
    | Rules(rs) ->
      begin
        match Env.add_rules rs with
        | OK _ -> cs
        | Err(Env.EnvErrorType(ty)) ->
          let mc = constraint_of_error ty in
          generate e (add_opt cs mc)
        | Err _ -> assert false
      end
    | Name (l,id) -> cs
    | _ -> failwith "unsupported"

  let generate _ e = generate e CS.empty
end

module Test =
struct

  open Parser

  type t = unit

  let sg = ref (Signature.make (mk_mident "no name"))

  let rec univ_of_int n =
    if n = 0 then
      Term.mk_Const dloc Cic.z
    else
      Term.mk_App (Term.mk_Const dloc Cic.s) (univ_of_int (n-1)) []

  let univ_of_int n =
    if n = -1 then
      Term.mk_Const dloc Cic.prop
    else
      Term.mk_App (Term.mk_Const dloc Cic.typ) (univ_of_int n) []

  let rule_name id i =
    mk_ident (string_of_ident id ^ (string_of_int i))

  let uvar_md = mk_mident "uvar"

  let mk_rule id i =
    let open Rule in
    let md = Env.get_name () in
    let r : untyped_rule =
      {
        name = Gamma(true, mk_name uvar_md (rule_name id i));
        ctx = [];
        pat = Pattern(dloc, (mk_name md id), []);
        rhs = univ_of_int i
      }
    in
    match Env.add_rules [r] with
    | OK _ -> ()
    | Err err ->  Errors.fail_env_error err

  let red_cfg = ref {Reduction.default_cfg with Reduction.select = Some (fun _ -> true)}

  let ignore_rule ident i =
    let open Reduction in
    let open Rule in
    let f =
      match !red_cfg.select with
      | Some f -> f
      | None -> assert false
    in
    red_cfg := {!red_cfg with
                select = Some (fun x -> x <> Gamma(true, mk_name uvar_md (rule_name ident i)) && f x)}

  let rec int_of_univ te =
    if Cic.is_prop te then
      -1
    else if Cic.is_z te then
      0
    else if Cic.is_s te then
      let te' = Cic.extract_s te in
      1+(int_of_univ te')
    else
      assert false

  let incr l =
    let u = Env.unsafe_reduction l in
    let i = int_of_univ u in
    let ident = Uvar.ident_of_uvar l in
    mk_rule ident (i+1);
    ignore_rule ident i

  let get_constraint l r =
    let open Cic in
    let open Uvar in
    if is_uvar l && is_succ r then
      incr l
    else
      failwith "todo"

  let get_constraint_wrapper l r =
    if Cic.is_univ l && Cic.is_univ r then
      let l = Cic.extract_univ l in
      let r = Cic.extract_univ r in
      get_constraint l r
    else
      begin
        Format.printf "left:%a@." Pp.print_term l;
        Format.printf "right:%a@." Pp.print_term r;
        assert false
      end

  let error_handler err =
    match err with
    | Typing.ConvertibilityError(term,ctx,l,r) ->
      Format.printf "left:%a@." Pp.print_term l;
      Format.printf "right:%a@." Pp.print_term r;
      get_constraint_wrapper l r
    | _ -> Errors.fail_typing_error err

  let rec generate e =
    match e with
    | Decl(l,id,st,t) ->
      begin
        match Env.declare l id st t with
        | OK () -> ()
        | Err(Env.EnvErrorType(ty)) ->
          error_handler ty;
          generate e
        | Err(_) -> assert false
      end
    | Def(l,id,op,pty,te) ->
      let define = if op then Env.define_op else Env.define in
      begin
        match define l id te pty with
        | OK () -> ()
        | Err(Env.EnvErrorType(ty)) ->
          error_handler ty;
          generate e
        | Err(_) -> assert false
      end
    | Rules(rs) ->
      begin
        match Env.add_rules rs with
        | OK _ -> ()
        | Err(Env.EnvErrorType(ty)) ->
          error_handler ty;
          generate e
        | Err _ -> assert false
      end
    | Name (l,id) -> ()
    | _ -> failwith "unsupported"

  let generate vars e =
    if Env.get_name () <> Signature.get_name !sg then
      sg := Signature.make (Env.get_name ());
    List.iter (fun id -> mk_rule id (-1)) vars;
    generate e


end
