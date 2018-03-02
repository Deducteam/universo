open Entry
open Basic
open Cic
open Term

let mk_rule s1 s2 =
  mk_App (mk_Const dloc Cic.rule) s1 [s2]

let mk_term s t =
  mk_App (mk_Const dloc Cic.term) s [t]

let mk_cuni t =
  mk_App (mk_Const dloc Cic.cuni) t []

let mk_lam x ty te =
  mk_Lam dloc x (Some ty) te

let mk_univ t =
  mk_App (mk_Const dloc Cic.univ) t []

let mk_prod s1 s2 a f =
  mk_App (mk_Const dloc Cic.prod) s1 [s2;a;f]

let mk_lift s1 s2 a =
  mk_App (mk_Const dloc Cic.lift) s1 [s2;a]

let mk_prop = mk_Const dloc Cic.prop

let mk_app = mk_App

let term_of_type n =
  if n = 0 then
    mk_App (mk_Const dloc typ) (mk_Const dloc z) []
  else
    begin
      assert (n = 1);
      mk_App (mk_Const dloc typ) (mk_App (mk_Const dloc s) (mk_Const dloc z) []) []
    end

let infer ctx t =
  match Env.infer ~ctx:ctx t with
  | OK ty -> Env.unsafe_reduction ty
  | Err err -> Errors.fail_env_error err

let is_user_const t =
  match t with
  | Term.Const (lc, name) ->
    begin
      match Env.get_type lc name with
      | OK _ -> true
      | Err _ -> false
    end
  | _ -> false


let rec elab_term ctx t =
  if is_var t then
    t
  else if is_cuni t then
    let t' = extract_cuni t in
    if is_prop t' then
      t
    else
      mk_cuni (term_of_type 0)
  else if is_lift t then
    let _,_,t' = extract_lift t in
    elab_term ctx t'
  else if is_prod t then
    let _,_,a,b = extract_prod t in
    let a' = elab_term ctx a in
    let tya = infer ctx a' in
    if is_univ tya then
      let s1 = extract_univ tya in
      let x,_,b = extract_lam b in
      let ctx' = (dloc,x, mk_term s1 a')::ctx in
      let b' = elab_term ctx' b in
      let tyb = infer ctx' b' in
      if is_univ tyb then
        let s2 = extract_univ tyb in
        mk_prod s1 s2 a' (mk_lam x (mk_term s1 a') b')
      else
        assert false
    else
      assert false
  else if is_app t then
    let f,a,args = extract_app t in
    let f' = elab_term ctx f in
    let a' = elab_term ctx a in
    let args' = List.map (elab_term ctx) args in
    mk_app f' a' args'
  else if is_lam t then
    let x, ty, te = extract_lam t in
    let ty' = elab_type ctx ty in
    let ctx' = (dloc, x, ty')::ctx in
    let te' = elab_term ctx' te in
    mk_lam x ty' te'
  else if is_user_const t then
    t
  else
    begin
      Format.printf "%a@." pp_term t;
      assert false
    end

and elab_type ctx t =
  if is_univ t then
    let t' = extract_univ t in
    if is_prop t' then
      t
    else(* if then
      mk_cuni mk_prop
           else *)
      mk_univ (term_of_type 0)
  else if is_term t then
    let _,a = extract_term t in
    let a' = elab_term ctx a in
    let ty = infer ctx a' in
    if is_univ ty then
      let s = extract_univ ty in
      mk_term s a'
    else
      assert false
  else
    assert false

let rec elaborate t =
  if is_cuni t then
    mk_cuni (Uvar.fresh_uvar ())
  else
    match t with
    | App(f, a, al) ->
      let f' = elaborate f in
      let a' = elaborate a in
      let al' = List.map elaborate al in
      mk_App f' a' al'
    | Lam(loc, id, t_opt, t) ->
      let t' = elaborate t in
      begin
        match t_opt with
        | None -> mk_Lam loc id t_opt t'
        | Some x -> let x' = elaborate x in
          mk_Lam loc id (Some x') t'
      end
    | Pi(loc, id, ta, tb) ->
      let ta' = elaborate ta in
      let tb' = elaborate tb in
      mk_Pi loc id ta' tb'
    | _ -> t

let get_constraint sl sr =
  if is_type sl && is_succ sr then
    let sr = extract_succ sr in
    let v = Uvar.ident_of_uvar sr in
    if term_eq (term_of_type 0) sl then
      v, mk_prop
    else if term_eq (term_of_type 1) sl then
      v, term_of_type 0
    else
      assert false
  else
    failwith "todo constraint"

let reconstruct (v,s) t =
  if Uvar.is_uvar t then
    let v' = Uvar.ident_of_uvar t in
    if ident_eq v v' then
      s
    else
      t
  else
    match t with
    | App(f, a, al) ->
      let f' = elaborate f in
      let a' = elaborate a in
      let al' = List.map elaborate al in
      mk_App f' a' al'
    | Lam(loc, id, t_opt, t) ->
      let t' = elaborate t in
      begin
        match t_opt with
        | None -> mk_Lam loc id t_opt t'
        | Some x -> let x' = elaborate x in
          mk_Lam loc id (Some x') t'
      end
    | Pi(loc, id, ta, tb) ->
      let ta' = elaborate ta in
      let tb' = elaborate tb in
      mk_Pi loc id ta' tb'
    | _ -> t

let rec recover_from_error err =
  match err with
  | Env.EnvErrorType(Typing.ConvertibilityError(t,ctx,tyl,tyr)) ->
    let t' = elaborate t in
    begin
      match Env.infer ~ctx:ctx t' with
      | OK _ -> t'
      | Err err ->
        let v,s = recover_from_universe_error err in
        reconstruct (v,s) t'
    end
  | _ -> assert false

and recover_from_universe_error err =
  match err with
  | Env.EnvErrorType(Typing.ConvertibilityError(t,ctx,tyl,tyr)) ->
    assert (is_univ tyl && is_univ tyr);
    let sl = extract_univ tyl in
    let sr = extract_univ tyr in
    get_constraint sl sr
  | _ -> assert false

let rec mk_entry fmt e =
  let pp e = Pp.print_entry fmt e in
  match e with
  | Decl(l,id,st,t) ->
    let t' = elab_type [] t in
    begin
      match Env.declare l id st t' with
      | OK () -> pp @@ Decl(l,id,st,t')
      | Err err -> Errors.fail_env_error err
    end;
  | Def(l,id,op,None,te) ->
    let te' = elab_term [] te in
    begin
      match Env.define l id te' None with
      | OK () -> pp @@ Def(l,id,op,None, te')
      | Err err -> Errors.fail_env_error err
    end;
  | Def(l,id,op,Some ty,te) ->
    let ty' = elab_type [] ty in
    let te' = elab_term [] te in
    begin
      match Env.define l id te' (Some ty') with
      | OK () -> pp @@ Def(l,id, op, Some ty', te')
      | Err err -> Errors.fail_env_error err
    end;
  | Rules(rs) ->
    let elab_rule (r : Rule.typed_rule) =
      let r = {r with Rule.rhs =  elab_term r.Rule.ctx r.Rule.rhs} in
      {r with Rule.ctx = List.map (fun (a,b,c) -> (a,b)) r.Rule.ctx}
    in
    begin
      match Env.add_rules rs with
      | OK rs' ->
        let rs' = List.map elab_rule rs' in
        pp @@ Rules(rs')
      | Err err -> Errors.fail_env_error err
    end;
  | Name (l,id) -> pp @@ (Name(l, id))
  | _ -> failwith "unsupported"

let run_on_file output export file =
  let input = open_in file in
  debug 1 "Processing file '%s'..." file;
  let md = mk_mident file in
  let fmt =
    if output = "" then Format.std_formatter
    else
      let oc = open_out (Filename.concat output file) in
      Format.formatter_of_out_channel oc
  in
  Env.init md;
  begin
    match Env.import dloc (mk_mident "cic") with
    | OK () -> ()
    | Err err -> Errors.fail_signature_error err
  end;
  Parser.handle_channel md (mk_entry fmt) input;
  Errors.success "File '%s' was successfully parsed." file;
  if export && not (Env.export ()) then
    Errors.fail dloc "Fail to export module '%a@." pp_mident (Env.get_name ());
  close_in input



let _ =
  let export = ref false in
  let output_dir = ref "" in
  let set_output_dir s = output_dir := s in
  let options = Arg.align
      [ ( "-d"
        , Arg.Int Basic.set_debug_mode
        , "N sets the debuging level to N" )
      ; ( "-e"
      , Arg.Set export
        , " Generates an object file (\".dko\")" )
      ; ( "--output-dir"
      , Arg.String set_output_dir
      , " Directory to print the files" )
      ; ( "-errors-in-snf"
      , Arg.Set Errors.errors_in_snf
      , " Normalize the types in error messages" )]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  Rule.allow_non_linear := true;
  try
    List.iter (run_on_file !output_dir !export) files;
  with
  | Sys_error err -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit          -> exit 3
