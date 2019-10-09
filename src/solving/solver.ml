module B = Kernel.Basic
module E = Parsers.Entry
module F = Common.Files
module L = Common.Log
module O = Common.Oracle
module P = Parsers.Parser
module R = Kernel.Rule
module S = Kernel.Signature
module T = Kernel.Term
module U = Common.Universes

open Utils

(** [from_rule pat rhs] add the assertion [pat = rhs]. *)
let from_rule : R.pattern -> T.term -> U.cstr = fun pat right ->
  let left   = R.pattern_to_term pat in
  try (* the constraint is a predicate *)
    U.Pred (U.extract_pred left)
  with U.Not_pred ->
    (* the constraint is en equality between variables *)
    let left' = Elaboration.Var.name_of_uvar left in
    let right' = Elaboration.Var.name_of_uvar right in
    U.EqVar(left',right')

module Make(S:SMTSOLVER) : SOLVER =
struct

  (** [parse meta s] parses a constraint file. *)
  let parse : string -> unit =
    fun in_path ->
      let module P =
        struct
          type t = U.cstr list

          let cstrs = ref []

          let handle_entry _ = function
            | E.Rules(_,rs) ->
              cstrs := (List.map (fun (r:R.untyped_rule) -> from_rule r.pat r.rhs) rs)::!cstrs
            | E.Require _ -> ()
            | _ -> assert false

          let get_data () = List.flatten !cstrs
        end
      in
      let cstr_file = F.get_out_path in_path `Checking in
      let cstrs = Api.Processor.handle_files [cstr_file] (module P) in
      List.iter S.add cstrs

  (** [print_model meta model f] print the model associated to the universes elaborated in file [f]. Each universe are elaborated to the original universe theory thanks to the dkmeta [meta] configuration. *)
  let print_model meta model in_path =
    let elab_file = F.get_out_path in_path `Elaboration in
    let sol_file = F.out_from_string in_path `Solution in
    let fmt = F.fmt_of_file sol_file in
    let md_theory = P.md_of_file (F.get_theory ()) in
    F.add_requires fmt [F.md_of in_path `Elaboration; md_theory];
    let module P =
    struct

      type t = unit

      let handle_entry env =
        let (module Printer) = Api.Env.get_printer env in
        function
        | E.Decl(_,id,_,_) ->
          let name = B.mk_name (Api.Env.get_name env) id in
          let sol = model name in
          let rhs = U.term_of_univ sol in
          let rhs' = Dkmeta.mk_term meta rhs in
          Format.fprintf fmt "[] %a --> %a.@." Printer.print_name name Printer.print_term rhs'
        | _ -> ()

      let get_data () = ()
    end
    in
    Api.Processor.handle_files [elab_file] (module P);
    F.close sol_file

  let solve = S.solve
end

(** Performance are bad with LRA *)
module MakeUF(Solver:SMTSOLVER) : SOLVER =
struct

  module SP = Set.Make(struct type t = U.pred let compare = compare end)

  let env = Api.Env.init (P.input_from_string (B.mk_mident "solver") "")

  let sp = ref SP.empty

  let mk_rule : R.untyped_rule -> unit = fun r ->
    let sg = Api.Env.get_signature env in
    match from_rule r.pat r.rhs with
    | U.EqVar _ -> S.add_rules sg [(R.to_rule_infos r)]
    | U.Pred p -> sp := SP.add p !sp

  (** [parse meta s] parses a constraint file. *)
  let parse : string -> unit =
    fun in_path ->
      let module P =
        struct
          type t = unit

          let handle_entry _ = function
            | E.Rules(_,rs) -> List.iter mk_rule rs
            | E.Require _ -> ()
            | _ -> assert false

          let get_data () = ()
        end
      in
      let cstr_file = F.get_out_path in_path `Checking in
      Api.Processor.handle_files [cstr_file] (module P)

  (* List.iter S.add entries' *)
  (* TODO: This should be factorized. the normalization should be done after solve and return a correct model *)
  (** [print_model meta model f] print the model associated to the universes elaborated in file [f]. Each universe are elaborated to the original universe theory thanks to the dkmeta [meta] configuration. *)
  let print_model meta_output model in_path  =
    let elab_file = F.get_out_path in_path `Elaboration in
    let sol_file = F.out_from_string in_path `Solution in
    let fmt = F.fmt_of_file sol_file in
    let md_theory = P.md_of_file (F.get_theory ()) in
    F.add_requires fmt [F.md_of in_path `Elaboration; md_theory];
    let meta_constraints = Dkmeta.meta_of_files [F.get_out_path in_path `Checking] in
    let module P =
    struct

      type t = unit

      let handle_entry env =
        let (module Printer) = Api.Env.get_printer env in
        let find =
          fun name ->
            match Dkmeta.mk_term meta_constraints (T.mk_Const B.dloc name) with
            | T.Const(_,name) -> name
            | _ -> assert false
        in
        function
        | E.Decl(_,id,_,_) ->
          let name = B.mk_name (Api.Env.get_name env) id in
          let sol = model (find name) in
          let rhs = U.term_of_univ sol in
          let rhs' = Dkmeta.mk_term meta_output rhs in
          Format.fprintf fmt "[] %a --> %a.@." Printer.print_name name Printer.print_term rhs'
        | _ -> ()

      let get_data () = ()
    end
    in
    Api.Processor.handle_files [elab_file] (module P);
    F.close sol_file

  let solve solver_env =
    let meta = {Dkmeta.default_config with env = env} in
    let normalize : U.pred -> U.pred = fun p ->
      U.extract_pred (Dkmeta.mk_term meta (U.term_of_pred p))
    in
    L.log_solver "[NORMALIZE CONSTRAINTS...]";
    let sp' = SP.map normalize !sp in
    L.log_solver "[NORMALIZE DONE]";
    SP.iter (fun p -> Solver.add (Pred p)) sp';
    Solver.solve solver_env
end
