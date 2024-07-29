open Base
open OUnit2


module ListMonadNotations = struct
  let (let*) x f = List.bind x ~f
  let (and*) x y = let* x in let* y in List.return (x, y)
  let return     = List.return
end


let test_run input expected =
  let open Monads.Notations.Star(Slang.EvaluationContext)
  in
  input >:: fun _ -> begin
      let program =
        let* _ = Slang.Prelude.initialize
        in
        Slang.Evaluation.evaluate_string input
      in
      let (actual, _)  = Slang.EvaluationContext.run program
      in
      match actual with
      | Slang.EvaluationContext.Success actual -> begin
          let msg =
            Printf.sprintf "expected = %s != %s = actual" (Slang.Value.to_string expected) (Slang.Value.to_string actual)
          in
          assert_equal ~msg expected actual
        end
      | Slang.EvaluationContext.Failure error -> begin
          assert_failure @@ "evaluation failed:" ^ error
        end
    end
