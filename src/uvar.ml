open Basic
open Entry


exception Not_uvar

module type S =
sig
  val is_uvar      : Term.term -> bool

  val name_of_uvar : Term.term -> name

  val fresh_uvar   : ?oc:out_channel option -> Signature.t ->  Term.term

  val count        : unit -> int
end

module Uvar =
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

  let fresh_uvar ?(oc=None) sg =
    let id = fresh () in
    let md = Signature.get_name sg in
    let name = mk_name md id in
    let cst = Term.mk_Const dloc name in
    let ty = Term.mk_Const dloc (mk_name (mk_mident "universo") (mk_ident "Sort")) in
    Signature.add_declaration sg dloc id Signature.Definable ty;
    cst
end

module Make(TO:Theory.Out) : S =
  struct
    include Uvar

    let init = ref false

    let fresh_uvar ?(oc=None) sg =
      let open Config in
      let id = fresh () in
      let md = Signature.get_name sg in
      let name = mk_name md id in
      let cst = Term.mk_Const dloc name in
      let ty = Term.mk_Const dloc (mk_name (mk_mident "universo") (mk_ident "Sort")) in
      Signature.add_declaration sg dloc id Signature.Definable ty;
      if not !init then
        begin
          List.iter (fun r -> Signature.add_rules sg [Rule.to_rule_infos r]) TO.out;
          init := true
        end;
      config.encoding <- None;
      config.meta_rules <-  List.map (fun (r:Rule.untyped_rule) -> r.Rule.name) TO.out;
      let ty' = Meta.normalize ~sg:(Some sg) ty in
      begin
        match oc with
        | None -> assert false;
        | Some oc ->
           Format.fprintf (Format.formatter_of_out_channel oc) "%a@."
             Pp.print_entry (Entry.Decl(dloc, id, Signature.Definable, ty'))
      end;
      cst
  end
