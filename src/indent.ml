open Basic
open Parser
open Pp

let indent_entry fmt e =
  let open Format in
  match e with
  | Decl(_,id,stat,ty)      ->
      let stat = if stat = Signature.Definable then "def " else "" in
      fprintf fmt "@[<2>%s%a :@ %a.@]@.@." stat print_ident id print_term ty
  | Def(_,id,opaque,ty,te)  ->
      let key = if opaque then "thm" else "def" in
      begin
        match ty with
        | None    -> fprintf fmt "@[<hv2>%s %a@ :=@ %a.@]@.@." key
                       print_ident id print_term te
        | Some ty -> fprintf fmt "@[<hv2>%s %a :@ %a@ :=@ %a.@]@.@." key
                       print_ident id print_term ty print_term te
      end
  | Rules(rs)               ->
      fprintf fmt "@[<v0>%a@].@.@." (print_list "" print_untyped_rule) rs
  | Eval(_,cfg,te)          ->
      fprintf fmt "#EVAL%a %a.@." print_red_cfg cfg print_term te
  | Infer(_,cfg,te)         ->
      fprintf fmt "#INFER%a %a.@." print_red_cfg cfg print_term te
  | Check(_,assrt,neg,test) ->
      let cmd = if assrt then "#ASSERT" else "#CHECK" in
      let neg = if neg then "NOT" else "" in
      begin
        match test with
        | Convert(t1,t2) ->
            fprintf fmt "%s%s %a ==@ %a.@." cmd neg print_term t1 print_term t2
        | HasType(te,ty) ->
            fprintf fmt "%s%s %a ::@ %a.@." cmd neg print_term te print_term ty
      end
  | DTree(_,m,v)            ->
      begin
        match m with
        | None   -> fprintf fmt "#GDT %a.@." print_ident v
        | Some m -> fprintf fmt "#GDT %a.%a.@." print_mident m print_ident v
      end
  | Print(_, str)           ->
      fprintf fmt "#PRINT %S.@." str
  | Name(_,_)               ->
      ()
