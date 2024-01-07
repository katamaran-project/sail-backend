module Tokenizer           = Tokenizer
module Value               = Value
module Parser              = Parsing
module Environment         = Environment
module Environment_builder = Environment_builder
module Evaluation          = Evaluation
module Evaluation_context  = Evaluation_context
module Types               = Types


let prelude = Prelude.prelude

let extend_environment = Environment_builder.extend_environment

let run_asts environment asts =
  Evaluation_context.run (Evaluation.evaluate_many asts) environment

let run_string environment string =
  let asts = Parser.parse_string string
  in
  run_asts environment asts
