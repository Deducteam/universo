open Utils

module type Z3LOGIC = Utils.LOGIC with type t = Z3.Expr.expr
                                   and type model = Z3.Model.model
                                   and type ctx = Z3.context

module Make(ZL:Z3LOGIC) : SMTSOLVER

module Syn : Z3LOGIC
module Arith(S:SOLVER_SPECIFICATION) : Z3LOGIC
