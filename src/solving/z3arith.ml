module B = Basic
module U = Common.Universes
module Z = Z3
module ZA = Z.Arithmetic
module ZB = Z.Boolean
module ZI = ZA.Integer


module Make(Spec:Utils.SOLVER_SPECIFICATION) =
struct
  type t = Z.Expr.expr
  type model = Z.Model.model
  type ctx = Z.context

  let mk_name : B.name -> string = fun name ->
    B.string_of_mident (B.md name) ^ (B.string_of_ident (B.id name))

  let int_sort ctx = ZI.mk_sort ctx

  let mk_var  : ctx -> string -> t = fun ctx s ->
    Z.Expr.mk_const_s ctx s (int_sort ctx)

  let to_int : ctx -> int -> t = fun ctx i -> ZI.mk_numeral_i ctx i

  let mk_univ : ctx -> U.univ -> t = fun ctx u ->
    match u with
    | Var name -> mk_var ctx (mk_name name)
    | Enum n -> to_int ctx n

  let mk_max : ctx -> t -> t -> t = fun ctx l r ->
    ZB.mk_ite ctx (ZA.mk_le ctx l r) r l

  let mk_imax : ctx -> t -> t -> t = fun ctx l r ->
    ZB.mk_ite ctx (ZB.mk_eq ctx r (to_int ctx 0))
      (to_int ctx 0)
      (mk_max ctx l r)


  module Reification =
  struct

    type z

    type 'a s = S

    type 'a fin =
      | F1 : ('a s) fin
      | FS : 'a fin -> ('a s) fin

    type 'a term =
      | Var   : 'a fin -> 'a term
      | True  : 'a term
      | False : 'a term
      | Succ  : 'a term           -> 'a term
      | Eq    : 'a term * 'a term -> 'a term
      | Max   : 'a term * 'a term -> 'a term
      | IMax  : 'a term * 'a term -> 'a term
      | Le    : 'a term * 'a term -> 'a term

    let rec check : type a. t list -> a fin -> int -> t =  fun env n i ->
      let lookup env i = List.nth env i in
      match n with
      | F1   -> lookup env i
      | FS n -> check env n (i+1)

    let check env n = check env n 0

    let rec apply ctx env = fun t ->
      match t with
      | Var n      -> check env n
      | True       -> ZB.mk_true ctx
      | False      -> ZB.mk_false ctx
      | Succ l     -> ZA.mk_add ctx [apply ctx env l;to_int ctx 1]
      | Eq(l,r)    -> ZB.mk_eq ctx (apply ctx env l) (apply ctx env r)
      | Max(l,r)   -> mk_max ctx (apply ctx env l) (apply ctx env r)
      | IMax(l,r)  -> mk_imax ctx (apply ctx env l) (apply ctx env r)
      | Le(a,b)    -> ZA.mk_le ctx (apply ctx env a) (apply ctx env b)

    let rec reify : (string * 'a fin) list -> Term.term -> 'a term = fun env t ->
      let of_str s = Basic.mk_name (Basic.mk_mident "z3") (Basic.mk_ident s) in
      match t with
      | DB(_,a,_) ->
        begin
          try
            Var(List.assoc (Basic.string_of_ident a) env)
          with Not_found -> failwith "Wrong configuration pattern for rule"
        end
      | Const(_,n) ->
        if Basic.name_eq n (of_str "true") then
          True
        else if Basic.name_eq n (of_str "false") then
          False
        else
          failwith "Wrong configuration pattern for rule"
      | App(Const(_,n),l,[]) ->
        if Basic.name_eq n (of_str "succ") then
          Succ(reify env l)
        else
          failwith "Wrong configuration pattern for rule"
      | App(Const(_,n),l,[r]) ->
        if Basic.name_eq n (of_str "eq") then
          Eq(reify env l, reify env r)
        else if Basic.name_eq n (of_str "max") then
          Max(reify env l, reify env r)
        else if Basic.name_eq n (of_str "imax") then
          IMax(reify env l, reify env r)
        else if Basic.name_eq n (of_str "le") then
          Le(reify env l, reify env r)
        else
          failwith "Wrong configuration pattern for rule"
      | _ -> failwith "Wrong configuration pattern for rule"

    let mk_env2 l a' b' =
      match l with
      | [a;b] ->
        [(a, F1); (b, (FS F1))], [a';b']
      | _ -> assert false

    let mk_env3 l a' b' c' =
      match l with
      | [a;b;c] ->
        [(a, F1); (b, (FS F1)); (c, (FS (FS F1)))], [a';b';c']
      | _ -> assert false

    let mk_axiom =
      let mk_axiom = ref (fun _ _ _ -> assert false) in
      fun  ctx a b ->
        let built = ref false in
        if !built then
          !mk_axiom ctx a b
        else
          begin
            built := true;
            let args,term = Spec.axiom_specification in
            let env_reify,env_apply = mk_env2 args a b in
            apply ctx env_apply (reify env_reify term)
          end

    let mk_rule =
      let mk_rule = ref (fun _ _ _ _ -> assert false) in
      fun  ctx a b c ->
        let built = ref false in
        if !built then
          !mk_rule ctx a b c
        else
          begin
            built := true;
            let args,term = Spec.rule_specification in
            let env_reify,env_apply = mk_env3 args a b c in
            apply ctx env_apply (reify env_reify term)
          end

    let mk_cumul ctx a b =
      let mk_cumul = ref (fun _ _ _ -> assert false) in
      let built = ref false in
      if !built then
        !mk_cumul ctx a b
      else
        begin
          built := true;
          let args,term = Spec.cumul_specification in
          let env_reify,env_apply = mk_env2 args a b in
          apply ctx env_apply (reify env_reify term)
        end
  end

  let mk_axiom = Reification.mk_axiom


  let mk_rule = Reification.mk_rule


  let mk_cumul = Reification.mk_cumul


  let mk_bounds ctx string up =
    let right =
      if up > 0 then
        ZA.mk_le ctx (mk_var ctx string) (to_int ctx (up-1))
      else
        ZB.mk_true ctx
    in
    let left = ZA.mk_le ctx (to_int ctx 0) (mk_var ctx string) in
    ZB.mk_and ctx [left;right]

  let solution_of_var : ctx -> int -> Z.Model.model -> string -> U.univ option =
    fun ctx _ model var ->
    match Z.Model.get_const_interp_e model (mk_var ctx var) with
    | None -> assert false
    | Some e ->
      let v = Big_int.int_of_big_int (ZI.get_big_int e) in
      Some (U.Enum v)

  let mk_theory = false

end
