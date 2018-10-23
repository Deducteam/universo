open Entry

let rec reconstruction model term =
  let open Term in
  if Universes.is_uvar term then
    begin
      let t' = (Env.unsafe_reduction term) in
      if Universes.is_uvar t' then
        let var = failwith "todo" in
        model var
      else
        t'
  end
  else
    match term with
    | App(f, a, al) ->
      let f' = reconstruction model f in
      let a' = reconstruction model a in
      let al' = List.map (reconstruction model) al in
      mk_App f' a' al'
    | Lam(loc, id, t_opt, t) ->
      let t' = reconstruction model t in
      begin
        match t_opt with
        | None -> mk_Lam loc id t_opt t'
        | Some x -> let x' = reconstruction model x in
          mk_Lam loc id (Some x') t'
      end
    | Pi(loc, id, ta, tb) ->
      let ta' = reconstruction model ta in
      let tb' = reconstruction model tb in
      mk_Pi loc id ta' tb'
    | _ ->     term

let entry_reconstruction model e =
  let open Rule in
  let open Parser in
  match e with
  | Decl(l,id,st,t) -> Decl(l,id,st, reconstruction model t)
  | Def(l,id,op,pty,te) -> Def(l,id,op,
                               Basic.map_opt (reconstruction model) pty, reconstruction model te)
  | Rules(sub,rs) ->
    let rs' = List.map (fun (r: untyped_rule) -> {r  with rhs = reconstruction model r.rhs}) rs in
    Rules(sub,rs')
  | Name (l,id) -> Name(l,id)
  | _ -> failwith "unsupported"
