module D = Basic.Debug

type D.flag += D_universo
let _ = D.register_flag D_universo "Universo"

(** Format transformers (colors). *)
let red fmt = "\027[31m" ^^ fmt ^^ "\027[0m%!"
let gre fmt = "\027[32m" ^^ fmt ^^ "\027[0m%!"
let yel fmt = "\027[33m" ^^ fmt ^^ "\027[0m%!"
let blu fmt = "\027[34m" ^^ fmt ^^ "\027[0m%!"
let mag fmt = "\027[35m" ^^ fmt ^^ "\027[0m%!"
let cya fmt = "\027[36m" ^^ fmt ^^ "\027[0m%!"

let log color  fmt = D.debug D_universo (color fmt)

let log_check  fmt = log mag fmt

let log_elab   fmt = log cya fmt

let log_solver fmt = log yel fmt

let log_univ   fmt = log gre fmt

(** [enable_flag str] actives flags present in [str] *)
let enable_flag : string -> unit = fun str ->
  try Env.set_debug_mode str
  with Env.DebugFlagNotRecognized c -> if c = 's' then D.enable_flag D_universo
