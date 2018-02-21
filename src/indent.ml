open Basic
open Parser
open Pp

let indent_entry e =
  let open Format in
  match e with
  | Decl(_,id,stat,ty)      ->
      let stat = if stat = Signature.Definable then "def " else "" in
      printf "@[<2>%s%a :@ %a.@]@.@." stat print_ident id print_term ty
  | Def(_,id,opaque,ty,te)  ->
      let key = if opaque then "thm" else "def" in
      begin
        match ty with
        | None    -> printf "@[<hv2>%s %a@ :=@ %a.@]@.@." key
                       print_ident id print_term te
        | Some ty -> printf "@[<hv2>%s %a :@ %a@ :=@ %a.@]@.@." key
                       print_ident id print_term ty print_term te
      end
  | Rules(rs)               ->
      printf "@[<v0>%a@].@.@." (print_list "" print_untyped_rule) rs
  | Eval(_,cfg,te)          ->
      printf "#EVAL%a %a.@." print_red_cfg cfg print_term te
  | Infer(_,cfg,te)         ->
      printf "#INFER%a %a.@." print_red_cfg cfg print_term te
  | Check(_,assrt,neg,test) ->
      let cmd = if assrt then "#ASSERT" else "#CHECK" in
      let neg = if neg then "NOT" else "" in
      begin
        match test with
        | Convert(t1,t2) ->
            printf "%s%s %a ==@ %a.@." cmd neg print_term t1 print_term t2
        | HasType(te,ty) ->
            printf "%s%s %a ::@ %a.@." cmd neg print_term te print_term ty
      end
  | DTree(_,m,v)            ->
      begin
        match m with
        | None   -> printf "#GDT %a.@." print_ident v
        | Some m -> printf "#GDT %a.%a.@." print_mident m print_ident v
      end
  | Print(_, str)           ->
      printf "#PRINT %S.@." str
  | Name(_,_)               ->
      ()
