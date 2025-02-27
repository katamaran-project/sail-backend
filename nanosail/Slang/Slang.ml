(*

   Slang (S-Expression Language)

   Used to configure the generation of muSail code.
   
*)

module Token               = Token
module Tokenizing          = Tokenizing
module Value               = Value
module Parser              = Parser
module Environment         = Environment
module EnvironmentBuilder  = EnvironmentBuilder
module Evaluation          = Evaluation
module EvaluationContext   = EvaluationContext
module Converters          = Converters
module Prelude             = Prelude
module Exception           = Exception
module Functions           = Functions
module Destructuring       = Destructuring
module Helpers             = Helpers
module Error               = Error



let extend_environment = EnvironmentBuilder.extend_environment

let run_asts environment asts =
  EvaluationContext.run_with_state (Evaluation.evaluate_sequentially asts) environment

let run_string environment string =
  let asts = Parser.parse_string string
  in
  run_asts environment asts
