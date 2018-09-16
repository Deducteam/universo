open Basic

type Debug.flag += D_cfg

type t =
  {
    uvar         : (module Uvar.S);
    theory_input : (module Theory.Compat);
    elaboration  : (module Elaboration.S)
  }


let run_on_compatibility_input_file file : (module Theory.Compat) =
  let ic = open_in file in
  let md = Env.init file in
  let mk_entry = function
    | Entry.Rules(_,rs) -> rs
    | _ -> assert false
  in
  let entries = Parser.parse_channel md ic in
  let rules = List.fold_left (fun r e -> r@mk_entry e) [] entries in
  let (module T:Theory.Compat) =
  match rules with
  | [] -> assert false
  (* the first rule is assumed to be the one that rewrite the Sort type *)
  | r::rs ->
    (module struct
       let sorts = [r]
       let constructors = rs
     end)
  in
  (module T)

let mk_theory_compat_input : string -> (module Theory.Compat) = fun compat ->
   if compat = "" then
      (
        Debug.debug D_cfg "No compatibility file given.";
        (module struct let sorts = [] let constructors = [] end)
      )
    else
      run_on_compatibility_input_file compat

let mk_configuration compat_input =
  let (module U:Uvar.S) = (module Uvar.Uvar) in
  let (module TI) = mk_theory_compat_input compat_input in
  let (module E:Elaboration.S) = (module Elaboration.Make(TI)(U)) in
  {uvar         = (module U);
   theory_input = (module TI);
   elaboration  = (module E)}
