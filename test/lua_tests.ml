open Lib
open Helpers

let infix_plus () =
  let code = "(+) : Int -> Int -> Int
(+) x y = @lua.+ x y
result = 12 + 13" in

  exec_with_type_and_res code "result" "(+)"
  |> must_succeed (fun (res_string, type_str) ->
         Alcotest.(check string) "Infix (+) Result" res_string "25";
         Alcotest.(check string) "Infix (+) Type" type_str "Int -> Int -> Int")

let infix_minus () =
  let code = "(-) : Int -> Int -> Int
(-) x y = @lua.- x y
result = 13 - 4" in
  exec_with_type_and_res code "result" "(-)"
  |> must_succeed (fun (res_string, type_str) ->
         Alcotest.(check string) "Infix (-) Result" "9" res_string;
         Alcotest.(check string) "Infix (+) Type" type_str "Int -> Int -> Int")

let test_set = [
  "Infix (+)", `Slow, infix_plus;
  "Infix (-)", `Slow, infix_minus;
]