open Basic
open Cic

module UVarElaboration =
struct
  let rec elaboration term =
    let open Term in
    if is_prop term then
      term
    else if  is_type term then
      Uvar.fresh_uvar ()
    else
      match term with
      | App(f, a, al) ->
        let f' = elaboration f in
        let a' = elaboration a in
        let al' = List.map elaboration al in
        mk_App f' a' al'
      | Lam(loc, id, t_opt, t) ->
        let t' = elaboration t in
        begin
          match t_opt with
          | None -> mk_Lam loc id t_opt t'
          | Some x -> let x' = elaboration x in
            mk_Lam loc id (Some x') t'
        end
      | Pi(loc, id, ta, tb) ->
        let ta' = elaboration ta in
        let tb' = elaboration tb in
        mk_Pi loc id ta' tb'
      | _ ->     term

  let elaboration_entry e =
    let open Parser in
    match e with
    | Decl(l,id,st,t) -> Decl(l,id,st, elaboration t)
    | Def(l,id,op,pty,te) -> Def(l,id,op, Basic.map_opt elaboration pty, elaboration te)
    | Rules(rs) -> failwith "todo rules"
    | _ -> failwith "unsupported"
end

let mk_entry md =

let run_on_file file =
  let input = open_in file in
  debug 1 "Processing file '%s'..." file;
  let md = mk_mident file in
  Env.init md;



let _ =
  let options = Arg.align
      [ ( "-d"
        , Arg.Int Basic.set_debug_mode
        , "N sets the debuging level to N" ) ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  try
    List.iter run_on_file files;
  with
  | Sys_error err -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit          -> exit 3
