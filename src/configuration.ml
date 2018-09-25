(* Configuration is used everywhere in Universo. *)
type t =
  {
    md_univ    : Basic.mident;
    oc_univ    : out_channel;
    md_check   : Basic.mident;
    sg_check   : Signature.t; (* Used for type checking *)
    sg_univ    : Signature.t; (* Signature for the universe module *)
  }

let default =
  let open Basic in
  {
    md_univ  = mk_mident "";
    oc_univ  = stdout ;
    md_check = mk_mident "";
    sg_check  = Signature.make "";
    sg_univ   = Signature.make "";
  }
