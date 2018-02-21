module type Generation =
sig

  type constraints

  module CS : Set.S with type elt = constraints

  val generate : Parser.entry -> CS.t

end

module Basic =
struct
  open Basic
  open Parser
  open Rule

  type constraints =
    | Univ of ident * int
    | Eq of ident * ident
    | Max of ident * ident * ident
    | Succ of ident * ident
    | Rule of ident * ident * ident

  module CS = Set.Make(struct type t = constraints let compare = compare end)

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
    | Err err -> Errors.fail_env_error err

  let rec int_of_type te =
    if Cic.is_z te then
      0
    else if Cic.is_s te then
      let te' = Cic.extract_s te in
      1+(int_of_type te')
    else
      assert false

  let are_convertible ?ctx:(ctx=[]) l r =
    match Env.are_convertible ~ctx:ctx l r with
    | OK t -> t
    | Err err -> false

  let rec get_constraint l r =
    let open Cic in
    let open Uvar in
    let l,r =
      match Env.reduction l, Env.reduction r with
      | OK l, OK r -> (l,r)
      | _, _ -> assert false
    in
    if are_convertible l r then None
    else
      begin
        mk_rule l r;
        if is_uvar l && is_uvar r then
          begin
            let ul = ident_of_uvar l in
            let ur = ident_of_uvar r in
            Some(Eq(ul,ur))
          end
        else if is_succ l && is_uvar r then
          begin
            let l = extract_succ l in
            let ul = ident_of_uvar l in (* might fail *)
            let ur = ident_of_uvar r in
            Some(Succ(ul,ur))
          end
        else if is_uvar l && is_succ r then
          get_constraint r l
        else if is_uvar l && is_type r then
          let ul = ident_of_uvar l in
          let r' = extract_type r in
          let i = int_of_type r' in
          Some(Univ(ul,i))
        else if is_type l && is_uvar r then
          get_constraint r l
        else if is_rule l && is_uvar r then
          let s1,s2 = extract_rule l in
          let u1 = ident_of_uvar s1  in
          let u2 = ident_of_uvar s2  in
          let r = ident_of_uvar r    in
          Some(Rule(u1,u2,r))
        else if is_uvar l && is_rule r then
          get_constraint r l
        else
          failwith "todo constraints"
      end

  let rec constraint_of_error err =
    match err with
    | Typing.ConvertibilityError(term,ctx,l,r) ->
      Format.printf "left:%a@." Pp.print_term l;
      Format.printf "right:%a@." Pp.print_term r;
      constraint_of_convertibility_test ctx l r
    | _ -> assert false

  and constraint_of_convertibility_test ctx l r =
    if Cic.is_univ l && Cic.is_univ r then
        let l' = Cic.extract_univ l in
        let r' = Cic.extract_univ r in
        get_constraint l' r'
    else if Cic.is_term l && Cic.is_term r then
      let s,l' = Cic.extract_term l in
      let s',r' = Cic.extract_term r in
      if are_convertible ~ctx:ctx l' r' then
        get_constraint s s'
      else
        match Env.are_convertible ~ctx:ctx l' r' with
        | Err(Env.EnvErrorType(ty)) -> constraint_of_error ty
        | OK false -> constraint_of_convertibility_test ctx l' r'
        | _ -> assert false
    else if Cic.is_univ l && Cic.is_univ r then
      let l' = Cic.extract_univ l in
      let r' = Cic.extract_univ r in
      get_constraint l' r'
    else
      match Env.reduction ~ctx:ctx l, Env.reduction ~ctx:ctx r with
      | OK(l'), OK(r') ->
        if Term.term_eq l l' && Term.term_eq r r' then
          begin
            Format.printf "left:%a@." Pp.print_term l;
            Format.printf "right:%a@." Pp.print_term r;
            failwith "todo"
          end
        else
          constraint_of_convertibility_test ctx l' r'
      | _,_ -> assert false

  let rec generate e cs =
    let add_opt mc =
      match mc with
      | None -> cs
      | Some c -> CS.add c cs
    in
    match e with
    | Decl(l,id,st,t) ->
      begin
        match Env.declare l id st t with
        | OK () -> cs
        | Err(Env.EnvErrorType(ty)) ->
          let mc = constraint_of_error ty in
          generate e (add_opt mc)
        | Err(_) -> assert false
      end
    | Def(l,id,op,pty,te) -> failwith "todo def"
    | Rules(rs) ->
      begin
        match Env.add_rules rs with
        | OK _ -> cs
        | Err err -> Errors.fail_env_error err
      end
    | Name (l,id) -> cs
    | _ -> failwith "unsupported"

  let generate e = generate e CS.empty
end
