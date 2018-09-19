open Basic
open Entry

exception Not_uvar

module type S =
sig
  val is_uvar      : Term.term -> bool

  val name_of_uvar : Term.term -> name

  val fresh_uvar   : Configuration.t ->  Term.term

  val count        : unit -> int
end

module Make(TO:Theory.Out) : S =
  struct
  let basename = "?"

  let is_uvar t =
    match t with
    | Term.Const(_,n) ->
      let s = string_of_ident (id n) in
      let n = String.length basename in
      String.length s > n && String.sub s 0 n = basename
    | _ -> false

  let name_of_uvar t =
    match t with
    | Term.Const(_,n) when is_uvar t -> n
    | _ -> Format.printf "%a@." Term.pp_term t; raise Not_uvar

  let counter = ref 0

  let count () = !counter

  let default_md = mk_mident "universo"

  let fresh () =
    let name = Format.sprintf "%s%d" basename !counter in
    incr counter; mk_ident name

  let fresh_uvar : Configuration.t -> Term.term = fun cfg ->
    let open Configuration in
    let open Rule in
    let id = fresh () in
    let name = mk_name cfg.md_univ id in
    let cst = Term.mk_Const dloc name in
    let ty = Term.mk_Const dloc (mk_name (mk_mident "universo") (mk_ident "Sort")) in
    Signature.add_declaration cfg.sg_univ dloc id Signature.Definable ty;
    let cfg_meta = Dkmeta.({default_config with encoding = None; sg = cfg.sg_meta}) in
    Dkmeta.(cfg_meta.meta_rules <- Some (List.map (fun (r:untyped_rule) -> r.name) TO.rules));
    let ty' = Dkmeta.mk_term cfg_meta ty in
    begin
      Format.fprintf (Format.formatter_of_out_channel cfg.oc_univ) "%a@."
        Pp.print_entry (Entry.Decl(dloc, id, Signature.Definable, ty'))
    end;
    cst
  end
