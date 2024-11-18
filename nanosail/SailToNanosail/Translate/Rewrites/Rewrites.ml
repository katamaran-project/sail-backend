module BitvectorComparisons = BitvectorComparisons


let statement_rewrites : (Ast.Statement.t -> Ast.Statement.t) list = [
  BitvectorComparisons.rewrite
]
