module Token               = Token
module Tokenizing          = Tokenizing
module Value               = Value
module Parser              = Parsing
module Environment         = Environment
module EnvironmentBuilder  = EnvironmentBuilder
module Evaluation          = Evaluation
module EvaluationContext   = EvaluationContext
module Converters          = Converters
module Multimethods        = Multimethods
module Prelude             = Prelude
module Exception           = Exception


let prelude = Prelude.prelude

let extend_environment = EnvironmentBuilder.extend_environment

let run_asts environment asts =
  EvaluationContext.run_with_state (Evaluation.evaluate_many asts) environment

let run_string environment string =
  let asts = Parser.parse_string string
  in
  run_asts environment asts
