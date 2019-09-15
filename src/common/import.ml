module EE = Env.Make(Reduction.Default)

module Pp = EE.Printer

module Errors = Errors.Make(EE)

module SB = Processor.SignatureBuilder(EE)

module TC = Processor.TypeChecker(EE)
