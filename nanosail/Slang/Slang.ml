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
module State               = State
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

let run_asts state asts =
  EvaluationContext.run_with_state (Evaluation.evaluate_sequentially asts) state

let run_string state string =
  let asts = Parser.parse_string string
  in
  run_asts state asts
