type t =
  {
    md_theory : Basic.mident;
    sg_meta   : Signature.t;
    md        : Basic.mident;
    sg        : Signature.t;
    md_univ   : Basic.mident;
    sg_univ   : Signature.t;
    oc_univ   : out_channel
  }

let default =
  let open Basic in
  { md_theory = mk_mident "";
    sg_meta = Signature.make "";
    md = mk_mident "";
    sg = Signature.make "";
    md_univ = mk_mident "";
    sg_univ = Signature.make "";
    oc_univ = stdout
  }
