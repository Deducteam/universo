module type S =
sig
  val rules : Rule.untyped_rule list
end

module type In = S

module type Th = S

module type Out = S

module Default : In =
  struct
    let rules = []
  end

let from_file : string -> (module S) = fun file ->
  if file = "" then (module Default) else
  let ic = open_in file in
  let md = Env.init file in
  let mk_entry = function
    | Entry.Rules(_,rs) -> rs
    | _ -> assert false
  in
  let entries = Parser.parse_channel md ic in
  let rules = List.fold_left (fun r e -> r@mk_entry e) [] entries in
  match rules with
  | [] -> assert false
  | _ -> (module struct let rules = rules end)
