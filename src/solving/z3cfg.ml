module B = Basic
module L = Common.Log
module O = Common.Oracle
module U = Common.Universes
module Z = Z3

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
let ctx = Z.mk_context (string_of_cfg cfg)

module type ALGEBRAIC =
sig
  type t = Z.Expr.expr

  val mk_name : B.name -> string
  val mk_var  : string -> t
  val mk_univ : U.univ -> t
  val mk_axiom : t -> t -> t
  val mk_cumul : t -> t -> t
  val mk_rule : t -> t -> t -> t
end


module Make(S:ALGEBRAIC) =
struct

  (** Set containing all the variables used by Z3 *)
  module SSet = Set.Make(struct type t = string let compare = compare end)

  (* Set of Z3 variables *)
  let vars = ref SSet.empty

  (* Z3 Solver *)
  let solver = Z3.Solver.mk_simple_solver ctx

  (** [add expr] add the asserition [expr] in the Z3 solver. [expr] should be a predicate. *)
  let add expr =
    Z3.Solver.add solver [expr]

  (** [mk_var s] construct a Z3 expression from the Z3 variable [s]. *)
  let mk_var s =
    vars := SSet.add s !vars;
    S.mk_var s

  (** [mk_pred p] construct the Z3 predicate from a universe predicate *)
  let mk_pred = fun p ->
    let open U in
    match p with
    | Axiom(s,s') -> S.mk_axiom (S.mk_univ s) (S.mk_univ s')
    | Cumul(s,s') -> S.mk_cumul (S.mk_univ s) (S.mk_univ s')
    | Rule(s,s',s'') -> S.mk_rule (S.mk_univ s) (S.mk_univ s') (S.mk_univ s'')

  (** [mk_theory m] construct a Z3 theory for the non-interpreted predicate using the theory [t]. *)
  let mk_theory t =
    List.iter (fun (p,b) ->
        if b then
          add (mk_pred p)
        else
          add (Z.Boolean.mk_not ctx (mk_pred p))) t

  (** [register_vars vars i] give bound for each variable [var] between [0] and [i] *)
  let register_vars vars i =
    let univs = O.enumerate i in
    SSet.iter (fun var ->
        let or_eqs = List.map (fun u -> Z.Boolean.mk_eq ctx (mk_var var) (S.mk_univ u)) univs in
        add (Z.Boolean.mk_or ctx or_eqs)) vars

  (** [mk_cstr c] construct the Z3 constraint from the universe constraint [c] *)
  let mk_cstr = fun c ->
    let open U in
    match c with
    | Pred p -> mk_pred p
    | EqVar(l,r) -> Z.Boolean.mk_eq ctx (mk_var (S.mk_name r)) (mk_var (S.mk_name l))

  (** [solution_of_var univs model var] looks for the concrete universe associated to [var]
      in the [model]. Such universe satisfy that model(univ) = model(var). *)
  let solution_of_var univs model var =
    let exception Found of U.univ in
    let find_univ e u  =
      match Z.Model.get_const_interp_e model (S.mk_univ u) with
      | None -> assert false
      | Some u' ->
        if e = u' then raise (Found u) else ()
    in
    match Z.Model.get_const_interp_e model var with
    | None -> assert false
    | Some e ->
      try
        List.iter (find_univ e) univs;
        None
      with Found(u) -> Some u

  (** [check theory_of i] solves the current constraints with at most [i] universes. If no solution is found, [check] is called recursively on [i+1]. *)
  let rec check theory_of i =
    Z3.Solver.push solver;
    let theory = theory_of i in
    mk_theory theory;
    register_vars !vars i;
    if i = 3 then
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
          let var = S.mk_name cst in
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

include Z3
