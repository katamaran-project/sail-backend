module Tokenizer    = Tokenizer
module Value        = Value
module Parser       = Parsing
module Environment  = Environment
module Evaluation   = Evaluation
module Types        = Types



let run asts =
  Evaluation_context.run (Evaluation.evaluate_many asts) Prelude.prelude
