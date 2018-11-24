module B = Basic
module F = Files
module U = Universes

type t =
  {
    file: F.cout F.t;
    meta:Dkmeta.cfg
  }

type print_cstrs =
  {
    eqvar: (B.name * B.name) list;
    axiom: (U.univ * U.univ) list;
    cumul: (U.univ * U.univ) list;
    rule: (U.univ * U.univ * U.univ) list;
  }

let empty = {eqvar=[];axiom=[];cumul=[];rule=[]}

let constraints : print_cstrs ref = ref empty

let register_cstr : U.cstr -> unit = function
  | U.EqVar(l,r) ->
    constraints := {!constraints with eqvar = (l,r)::!constraints.eqvar}
  | U.Pred(Axiom(l,r)) ->
    constraints := {!constraints with axiom = (l,r)::!constraints.axiom}
  | U.Pred(Cumul(l,r)) ->
    constraints := {!constraints with cumul = (l,r)::!constraints.cumul}
  | U.Pred(Rule(l,m,r)) ->
    constraints := {!constraints with rule = (l,m,r)::!constraints.rule}

let dummy_name = Rule.Gamma(false, B.mk_name (B.mk_mident "dummy") (B.mk_ident "dummy"))

(* FIXME: copy/paste from checker.ml *)
let add_rule  sg vl vr =
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
  Signature.add_rules sg  [Rule.to_rule_infos rule]

let print_constraints env =
  let fmt = F.fmt_of_file env.file in
  let normalize t = Dkmeta.mk_term env.meta t in
  let print_rule pp l r =
    Format.fprintf fmt "@.[] %a --> %a" pp l pp r
  in
  let print_dot () = Format.fprintf fmt "." in
  let print_eq_var (l,r) =
    add_rule env.meta.sg l r;
    print_rule Pp.print_name l r; print_dot ()
  in
  let print_predicate p =
    let l' = normalize (U.term_of_pred p) in
    let r' = normalize U.true_ in
    print_rule Pp.print_term l' r'
  in
  List.iter print_eq_var !constraints.eqvar;
  List.iter (fun (l,r) -> print_predicate (Axiom(l,r))) !constraints.axiom;
  if List.length !constraints.axiom <> 0 then
    print_dot ();
  List.iter (fun (l,r) -> print_predicate (Cumul(l,r))) !constraints.cumul;
  if List.length !constraints.cumul <> 0 then
    print_dot ();
  List.iter (fun (l,m,r) -> print_predicate (Rule(l,m,r))) !constraints.rule;
  if List.length !constraints.rule <> 0 then
    print_dot ();
  Format.fprintf fmt "@." (* flush last dot *)

(** [mk_var_cstre env f l r] add the constraint [l =?= r]. Call f on l and r such that
    l >= r. *)
let mk_var_cstr f l r =
  let get_number s =
    int_of_string (String.sub s 1 (String.length s - 1))
  in
  let nl = get_number (Basic.string_of_ident @@ Basic.id l) in
  let nr = get_number (Basic.string_of_ident @@ Basic.id r) in
  if nr < nl then (
    f l r; U.EqVar(l,r))
  else (
    f r l; U.EqVar(r,l))


let mk_cstr f cstr =
  match cstr with
  | (U.Pred _) as cstr ->
    register_cstr cstr;
    true
  | U.EqVar(l,r) ->
    let cstr' = mk_var_cstr f l r in
    register_cstr cstr';
    true

let flush env = print_constraints env; constraints := empty
