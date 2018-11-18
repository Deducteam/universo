module L = Common.Log
module O = Common.Oracle
module U = Common.Universes
module F = Common.Files

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

(** Set containing all the variables used by Z3 *)
module SSet = Set.Make(struct type t = string let compare = compare end)

open Z3

(** Z3 configuration *)
type cfg_item = [`Model of bool | `Proof of bool | `Trace of bool | `TraceFile of string]

type cfg = cfg_item list

(** Concrete configuration. *)
let cfg = [`Model(true); (* Generate a model *)
           `Proof(true); (* Give a proof if unsatisfiable *)
           `Trace(false); (* Do not generate trace *)
          ]

let string_of_cfg_item item =
  match item with
  | `Model(b) -> ("model", string_of_bool b)
  | `Proof(b) -> ("proof", string_of_bool b)
  | `Trace(b) -> ("trace", string_of_bool b)
  | `TraceFile(file) -> ("trace_file_name", file)

let string_of_cfg cfg = List.map string_of_cfg_item cfg

(** Z3 context elaborated from a Z3 configuration *)
let ctx = mk_context (string_of_cfg cfg)

(** Z3 Solver with non interpreted symbol Functions *)
module Syn =
struct

  type t =
    {
      model : Dkmeta.cfg (* used to elaborate a model *)
    }

  (* Set of Z3 variables *)
  let vars = ref SSet.empty

  (* Z3 Solver *)
  let solver = Z3.Solver.mk_simple_solver ctx

  (* Z3 type for universes *)
  let sort      = Sort.mk_uninterpreted_s ctx "Sort"

  (** [reset ()] resets the Z3 solver *)
  let reset () =
    vars := SSet.empty;
    Z3.Solver.reset solver

  (** [var_of_name name] returns a variable string for Z3. ASSUME that all identifiers representing fresh universe variables are unique, hence the module can be forgetten. *)
  let var_of_name cst = Basic.string_of_ident (Basic.id cst)

  (** non-interpreted symbol for Prop *)
  (* FIXME: should be given by the model *)
  let mk_prop =
    Expr.mk_const_s ctx "Prop" sort

  (** non-interpreted symbol for Set *)
  (* FIXME: should be given by the model *)
  let mk_set =
    Expr.mk_const_s ctx "Set" sort

  (** non-interpreted symbol for Type i *)
  (* FIXME: should be given by the model *)
  let mk_type i =
    Expr.mk_const_s ctx ("Type"^string_of_int i) sort

  (** [mk_var s] construct a Z3 expression from the Z3 variable [s]. *)
  let mk_var s =
    vars := SSet.add s !vars;
    Expr.mk_const_s ctx s sort

  (** [mk_univ u] construct a Z3 expression from a universe. *)
  let mk_univ  = fun t ->
    let open U in
    match t with
    | Var cst -> mk_var (var_of_name cst)
    | Prop -> mk_prop
    | Set -> mk_set
    | Type(i) -> mk_type i

  (** [solution_of_var univs model var] looks for the concrete universe associated to [var]
      in the [model]. Such universe satisfy that model(univ) = model(var). *)
  let solution_of_var univs model var =
    let exception Found of U.univ in
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

  (** [mk_axiom s s'] construct the Z3 predicate associated to the Axiom Predicate *)
  let mk_axiom s s' =
    let axiom = FuncDecl.mk_func_decl_s ctx "A" [sort;sort] bool_sort in
    Expr.mk_app ctx axiom [s;s']

  (** [mk_cumul s s'] construct the Z3 predicate associated to the Cumul Predicate *)
  let mk_cumul s s' =
    let cumul = FuncDecl.mk_func_decl_s ctx "C" [sort;sort] bool_sort in
    Expr.mk_app ctx cumul [s;s']

  (** [mk_rule s s' s''] construct the Z3 predicate associated to the Rule Predicate *)
  let mk_rule s s' s'' =
    let cumul = FuncDecl.mk_func_decl_s ctx "R" [sort;sort;sort] bool_sort in
    Expr.mk_app ctx cumul [s;s';s'']

  (** [mk_pred p] construct the Z3 predicate from a universe predicate *)
  let mk_pred = fun p ->
    let open U in
    match p with
    | Axiom(s,s') -> mk_axiom (mk_univ s) (mk_univ s')
    | Cumul(s,s') -> mk_cumul (mk_univ s) (mk_univ s')
    | Rule(s,s',s'') -> mk_rule (mk_univ s) (mk_univ s') (mk_univ s'')

  (** [mk_cstr c] construct the Z3 constraint from the universe constraint [c] *)
  let mk_cstr = fun c ->
    let open U in
    match c with
    | Pred p -> mk_pred p
    | EqVar(l,r) -> Boolean.mk_eq ctx (mk_var (var_of_name l)) (mk_var (var_of_name r))

  (** [add expr] add the asserition [expr] in the Z3 solver. [expr] should be a predicate. *)
  let add expr =
    Z3.Solver.add solver [expr]

  (** [mk_theory m] construct a Z3 theory for the non-interpreted predicate using the theory [t]. *)
  let mk_theory t =
    List.iter (fun (p,b) ->
        if b then
          add (mk_pred p)
        else
          add (Boolean.mk_not ctx (mk_pred p))) t

  (** [register_vars vars i] give bound for each variable [var] between [0] and [i] *)
  let register_vars vars i =
    let univs = O.enumerate i in
    SSet.iter (fun var ->
        let or_eqs = List.map (fun u -> Boolean.mk_eq ctx (mk_var var) (mk_univ u)) univs in
        add (Boolean.mk_or ctx or_eqs)) vars

  (** [check theory_of i] solves the current constraints with at most [i] universes. If no solution is found, [check] is called recursively on [i+1]. *)
  let rec check theory_of i =
    Z3.Solver.push solver;
    let theory = theory_of i in
    mk_theory theory;
    register_vars !vars i;
    (* Format.eprintf "%s@." (Z3.Solver.to_string solver); *)
    (* FIXME: hard coded upper bound *)
    if i > 6 then failwith "Probably the Constraints are inconsistent";
    match Z3.Solver.check solver [] with
    | Z3.Solver.UNSATISFIABLE ->
      L.log_solver "[SOLVER] No solution found with %d universes" i;
      Z3.Solver.pop solver 1; check theory_of (i+1)
    | Z3.Solver.UNKNOWN -> assert false
    | Z3.Solver.SATISFIABLE ->
      match Z3.Solver.get_model solver with
      | None -> assert false (* the context says that we want a model *)
      | Some model ->
        (* FIXME: This is not useful anymore *)
        let hmodel = Hashtbl.create 10001 in
        (* Format.eprintf "%s@." (Z3.Model.to_string model); *)
        let find var =
          let univs = O.enumerate i in
          match solution_of_var univs model var with
          | None -> U.Prop
          | Some u -> u
        in
        let model (cst:Basic.name) : U.univ =
          let var = var_of_name cst in
          if Hashtbl.mem hmodel var then
            Hashtbl.find hmodel var
          else
            let t = find (mk_var var) in
            Hashtbl.add hmodel var t;
            t
        in
        (i,model)

  (** [solve mk_theory] tries to solve the constraints *)
  let solve mk_theory = check mk_theory 1

  let add : U.cstr -> unit = fun cstr -> add (mk_cstr cstr)
end
