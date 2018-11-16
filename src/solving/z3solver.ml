module U = Checking.Universes

type model = Basic.name -> U.univ

type t =
  {
    md_elab:Basic.mident;
    md_check:Basic.mident
  }


module type SOLVER =
sig
  val parse   : Dkmeta.cfg -> string -> unit

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
  type t =
    {
      model : Dkmeta.cfg
    }


  let vars = ref SSet.empty

  let solver = Z3.Solver.mk_simple_solver ctx

  let sort      = Sort.mk_uninterpreted_s ctx "Sort"

  let reset () =
    vars := SSet.empty;
    Z3.Solver.reset solver

  let var_of_name cst = Basic.string_of_ident (Basic.id cst)

  let mk_prop =
    Expr.mk_const_s ctx "Prop" sort

  let mk_set =
    Expr.mk_const_s ctx "Set" sort

  let mk_type i =
    Expr.mk_const_s ctx ("Type"^string_of_int i) sort

  let mk_var s =
    vars := SSet.add s !vars;
    Expr.mk_const_s ctx s sort

  let mk_univ  = fun t ->
    let open U in
    match t with
    | Var cst -> mk_var (var_of_name cst)
    | Prop -> mk_prop
    | Set -> mk_set
    | Type(i) -> mk_type i

  let solution_of_var i model var =
    let exception Found of Checking.Universes.univ in
    let univs = Checking.Universes.enumerate i in
    let find_univ e u  =
      match Model.get_const_interp_e model (mk_univ u) with
      | None -> assert false
      | Some u' ->
        if e = u' then raise (Found u) else ()
    in
    match Model.get_const_interp_e model var with
    | None -> assert false
    | Some e ->
      try
        List.iter (find_univ e) univs;
        None
      with Found(u) -> Some u


  let bool_sort = Boolean.mk_sort ctx

  let mk_axiom s s' =
    let axiom = FuncDecl.mk_func_decl_s ctx "A" [sort;sort] bool_sort in
    Expr.mk_app ctx axiom [s;s']

  let mk_cumul s s' =
    let cumul = FuncDecl.mk_func_decl_s ctx "C" [sort;sort] bool_sort in
    Expr.mk_app ctx cumul [s;s']

  let mk_rule s s' s'' =
    let cumul = FuncDecl.mk_func_decl_s ctx "R" [sort;sort;sort] bool_sort in
    Expr.mk_app ctx cumul [s;s';s'']

  let add expr =
    Z3.Solver.add solver [expr]

  let mk_pred = fun p ->
    let open U in
    match p with
    | Axiom(s,s') -> mk_axiom (mk_univ s) (mk_univ s')
    | Cumul(s,s') -> mk_cumul (mk_univ s) (mk_univ s')
    | Rule(s,s',s'') -> mk_rule (mk_univ s) (mk_univ s') (mk_univ s'')

  let mk_model m =
    List.iter (fun (p,b) ->
        if b then
          add (mk_pred p)
        else
          add (Boolean.mk_not ctx (mk_pred p))) m

  let register_vars vars i =
    let univs = Checking.Universes.enumerate i in
    SSet.iter (fun var ->
        let or_eqs = List.map (fun u -> Boolean.mk_eq ctx (mk_var var) (mk_univ u)) univs in
        add (Boolean.mk_or ctx or_eqs)) vars

  let rec check meta i =
    Z3.Solver.push solver;
    let model = Checking.Universes.mk_model meta i in
    mk_model model;
    register_vars !vars i;
    Format.eprintf "%s@." (Z3.Solver.to_string solver);
    if i > 6 then failwith "Probably the Constraints are inconsistent";
    match Z3.Solver.check solver [] with
    | Z3.Solver.UNSATISFIABLE ->
      Format.eprintf "No solution found with %d universes@." i;
      Z3.Solver.pop solver 1; check meta (i+1)
    | Z3.Solver.UNKNOWN -> failwith "This bug should be reported (check)"
    | Z3.Solver.SATISFIABLE ->
      match Z3.Solver.get_model solver with
      | None -> assert false
      | Some model ->
        let hmodel = Hashtbl.create 10001 in
        Format.eprintf "%s@." (Z3.Model.to_string model);
        let find var =
          match solution_of_var i model var with
          | None -> U.Prop
          | Some u -> u
        in
        i,
        fun (cst:Basic.name) : U.univ ->
          let var = var_of_name cst in
          if Hashtbl.mem hmodel var then
            Hashtbl.find hmodel var
          else
            let t = find (mk_var var) in
            Hashtbl.add hmodel var t;
            t

  let solve env = check env 1

  let from_rule : Rule.pattern -> Term.term -> unit = fun pat right ->
    let left   = Rule.pattern_to_term pat in
    try
      let pred'  = U.extract_pred left in
      let pred'' = mk_pred pred' in
      add pred''
    with Checking.Universes.Not_pred ->
      let left' = Elaboration.Var.name_of_uvar left in
      let right' = Elaboration.Var.name_of_uvar right in
      add (Boolean.mk_eq ctx (mk_var (var_of_name left')) (mk_var (var_of_name right')))

  let parse : Basic.mident -> Basic.mident -> string -> string -> unit =
    fun md_elab md_check compat s ->
    let meta = Dkmeta.meta_of_file false compat in
    let mk_entry = function
      | Entry.Rules(_,rs) ->
        Rule.(List.iter (fun r -> from_rule r.pat r.rhs) rs)
      | _ -> assert false
    in
    let mk_entry e = mk_entry (Dkmeta.mk_entry meta md_elab e) in
    Parser.Parse_channel.handle md_check mk_entry (open_in s)
end
