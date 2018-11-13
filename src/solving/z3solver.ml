module U = Checking.Universes

type model = Basic.name -> U.univ

type t =
  {
    md_elab:Basic.mident;
    md_check:Basic.mident
  }


module type SOLVER =
sig
  val parse   : t -> string -> unit

  val solve   : unit -> int * model

  val reset   : unit -> unit
end

module SSet = Set.Make(struct type t = string let compare = compare end)

open Z3

type cfg_item = [`Model of bool | `Proof of bool | `Trace of bool | `TraceFile of string]

type cfg = cfg_item list

let cfg = [`Model(true);
           `Proof(true);
           `Trace(false)]

let string_of_cfg_item item =
  match item with
  | `Model(b) -> ("model", string_of_bool b)
  | `Proof(b) -> ("proof", string_of_bool b)
  | `Trace(b) -> ("trace", string_of_bool b)
  | `TraceFile(file) -> ("trace_file_name", file)

let string_of_cfg cfg = List.map string_of_cfg_item cfg

let ctx = mk_context (string_of_cfg cfg)


module Syn =
struct

  type t = Expr.expr

  let vars = ref SSet.empty

  let solver = Z3.Solver.mk_simple_solver ctx

  let sort      = Sort.mk_uninterpreted_s ctx "Univ"

  (* Type 0 is impredictive *)
  let mk_univ i = Expr.mk_const_s ctx ("type"^(string_of_int i)) sort

  let mk_succ   = FuncDecl.mk_func_decl_s ctx "S" [sort] sort

  let mk_max    = FuncDecl.mk_func_decl_s ctx "M" [sort;sort] sort

  let mk_rule   = FuncDecl.mk_func_decl_s ctx "R" [sort;sort] sort

  let mk_eq l r = Boolean.mk_eq ctx l r

  let mk_succ l  =
    Expr.mk_app ctx mk_succ [l]

  let mk_max l1 l2 =
    Expr.mk_app ctx mk_max [l1;l2]

  let mk_rule l1 l2 =
   Expr.mk_app ctx mk_rule [l1;l2]

  let mk_var s =
    vars := SSet.add s !vars;
    Expr.mk_const_s ctx s sort

  let mk_type i = mk_univ (i + 1)

  let mk_prop = mk_univ 0

  let mk_set = mk_univ (-1)

  let add expr =
    Z3.Solver.add solver [expr]

  let mk_eq l r = add (mk_eq l r)

  let mk_neq l r = add (Boolean.mk_not ctx (Boolean.mk_eq ctx l r))

  let mk_axiom_succ i max =
    mk_eq (mk_succ (mk_univ i)) (mk_univ (i+1));
    for j = 0 to max
    do
      if 1 + i <> j then
        mk_neq (mk_succ (mk_univ i)) (mk_univ j)
    done

  let mk_axiom_max i j m =
    mk_eq (mk_max (mk_univ i) (mk_univ j)) (mk_univ (max i j));
    for k = 0 to m
    do
      if k <> max i j then
        mk_neq (mk_max (mk_univ i) (mk_univ j)) (mk_univ k)
    done

  let mk_axiom_rule i j m =
    if j = 0 then
      begin
        mk_eq (mk_rule (mk_univ i) (mk_univ 0)) (mk_univ 0);
        for k = 1 to m
        do
          mk_neq (mk_rule (mk_univ i) (mk_univ 0)) (mk_univ k)
        done
      end
    else
      begin
        mk_eq (mk_rule (mk_univ i) (mk_univ j)) (mk_univ (max i j));
        for k = 0 to m
        do
          if k <> (max i j) then
            mk_neq (mk_rule (mk_univ i) (mk_univ j)) (mk_univ k)
        done
      end


  let register_axioms max =
    for i = 0 to max
    do
      mk_axiom_succ i max;
      for j = 0 to max
      do
        mk_axiom_max i j max;
        mk_axiom_rule i j max;
        if i <> j then
          mk_neq (mk_univ i) (mk_univ j);
      done;
    done

  let rec range i j =
    if i = j then
      []
    else
      i::(range (i+1) j)

  let register_vars vars i =
    let register_vars var =
      let eqs = List.map
          (fun i -> Boolean.mk_eq  ctx (Expr.mk_const_s ctx var sort) (mk_univ i)) (range 0 (i+1)) in
      add (Boolean.mk_or ctx eqs)
    in
    SSet.iter register_vars vars

  let solution_of_var model var =
    let univ_of_int i =
      if i = 0 then
        U.Prop
      else
        U.Type (i - 1)
    in
    let rec find_univ e i  =
      match Model.get_const_interp_e model (mk_univ i) with
      | None -> assert false
      | Some u ->
        if e = u then i else find_univ e (i+1)
    in
    match Model.get_const_interp_e model (mk_var var) with
    | None -> assert false
    | Some e -> univ_of_int (find_univ e 0)


  let reset () =
    vars := SSet.empty;
    Z3.Solver.reset solver

  let var_of_name cst = Basic.string_of_ident (Basic.id cst)

  let rec check i =
    Z3.Solver.push solver;
    register_axioms i;
    register_vars !vars i;
    if i > 5 then failwith "Probably the Constraints are inconsistent";
    match Z3.Solver.check solver [] with
    | Z3.Solver.UNSATISFIABLE ->
      Format.eprintf "No solution found with %d universes@." i;
      Z3.Solver.pop solver 1; check (i+1)
    | Z3.Solver.UNKNOWN -> failwith "This bug should be reported (check)"
    | Z3.Solver.SATISFIABLE ->
      match Z3.Solver.get_model solver with
      | None -> assert false
      | Some model ->
        let hmodel = Hashtbl.create 10001 in
        let find var =
          try
            (solution_of_var model var)
          with _ -> U.Prop
        in
        i+1,
        fun (cst:Basic.name) : U.univ ->
          let var = var_of_name cst in
          if Hashtbl.mem hmodel var then
            Hashtbl.find hmodel var
          else
            let t = find var in
            Hashtbl.add hmodel var t;
            t

  let solve () = check 1

  let rec from_univ  = fun t ->
    let open U in
    match t with
    | Var cst -> mk_var (var_of_name cst)
    | Prop -> mk_prop
    | Set -> mk_set
    | Type(i) -> mk_type i
    | Succ(s) -> mk_succ (from_univ s)
    | Max(l,r) -> mk_max (from_univ l) (from_univ r)
    | Rule(l,r) -> mk_rule (from_univ l) (from_univ r)

  let from_rule : Basic.mident -> Rule.pattern -> Term.term -> unit =
    fun md pat right ->
      let left   = Rule.pattern_to_term pat in
      let left'  = U.extract_univ md left in
      let right' = U.extract_univ md right in
      let left'' = from_univ left' in
      let right'' = from_univ right' in
      mk_eq left'' right''

  let parse : Basic.mident -> Basic.mident -> string -> string -> unit =
    fun md_elab md_check compat s ->
    let meta = Dkmeta.meta_of_file false compat in
    let mk_entry = function
      | Entry.Rules(_,rs) ->
        Rule.(List.iter (fun r -> from_rule md_elab r.pat r.rhs) rs)
      | _ -> assert false
    in
    let mk_entry e = mk_entry (Dkmeta.mk_entry meta md_elab e) in
    Parser.Parse_channel.handle md_check mk_entry (open_in s)

end
