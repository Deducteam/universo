open Basic

type Debug.flag += D_cfg
let _ = Debug.register_flag D_cfg "c"

type t =
  {
    uvar          : (module Uvar.S);
    theory_input  : (module Theory.In);
    theory_output : (module Theory.Out);
    elaboration   : (module Elaboration.S)
  }


let run_on_compatibility_input_file file : (module Theory.In) =
  let ic = open_in file in
  let md = Env.init file in
  let mk_entry = function
    | Entry.Rules(_,rs) -> rs
    | _ -> assert false
  in
  let entries = Parser.parse_channel md ic in
  let rules = List.fold_left (fun r e -> r@mk_entry e) [] entries in
  let (module TI:Theory.In) =
  match rules with
  | [] -> assert false
  (* the first rule is assumed to be the one that rewrite the Sort type *)
  | r::rs ->
    (module struct
       let sorts = [r]
       let constructors = rs
     end)
  in
  (module TI)

let run_on_compatibility_output_file file : (module Theory.Out) =
  let ic = open_in file in
  let md = Env.init file in
  let mk_entry = function
    | Entry.Rules(_,rs) -> rs
    | _ -> assert false
  in
  let entries = Parser.parse_channel md ic in
  let rules = List.fold_left (fun r e -> r@mk_entry e) [] entries in
  let (module TO:Theory.Out) =
  match rules with
  | [] -> assert false
  (* the first rule is assumed to be the one that rewrite the Sort type *)
  | _ -> (module struct let out = rules end)
  in
  (module TO)


let mk_theory_compat_input : string -> (module Theory.In) = fun compat ->
   if compat = "" then
      (
        Debug.debug D_cfg "No compatibility input file given.";
        (module struct let sorts = [] let constructors = [] end)
      )
    else
      run_on_compatibility_input_file compat

let mk_theory_compat_output : string -> (module Theory.Out) = fun compat ->
   if compat = "" then
      (
        Debug.debug D_cfg "No compatibility output file given.";
        (module struct let out = [] end)
      )
    else
      run_on_compatibility_output_file compat


let mk_configuration compat_input compat_output =
  let (module TO) = mk_theory_compat_output compat_output in
  let (module U:Uvar.S) = (module Uvar.Make(TO)) in
  let (module TI) = mk_theory_compat_input compat_input in
  let (module E:Elaboration.S) = (module Elaboration.Make(TI)(U)) in
  {uvar         = (module U);
   theory_input = (module TI);
   theory_output = (module TO);
   elaboration  = (module E)}
