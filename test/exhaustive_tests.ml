open Lib
open Helpers


let exhaustive_bools () =
  exec_code "main x = match x with true -> 1 end"
  |> must_fail (function
      | {Lib.Ast.Error.error = Lib.Ast.Error.Inexhaustive _} -> ()
      | _ -> Alcotest.fail "Expected type error")

let exhaustive_captures () =
  let code = "main = match 2 with 1 -> 1 | x -> 2 end" in
  exec_with_type_and_res code "main" "main"
  |> must_succeed (fun (res_string, type_str) ->
        Alcotest.(check string) "Result should be 2" "2" res_string)

let exhaustive_tuples () =
  exec_code
    "main x =
      match x with
        (true, false) ->
          true
      | (x, false) -> false end"
  |> must_fail (function
      | {Lib.Ast.Error.error = Lib.Ast.Error.Inexhaustive _} -> ()
      | _ -> Alcotest.fail "Expected type error")

let exhaustive_lists () =
  let code =
    "main x =
      match x with
        [] -> false
      | h :: t -> true
      end" in
  exec_with_type_and_res code "main" "main"
  |> must_succeed (fun (res_string, type_str) ->
         Alcotest.(check string) "Type should match" "List a -> Bool" type_str)

let exhaustive_generic_captures () =
  let code = "main x = match x with a -> 1 end" in
  exec_with_type_and_res code "main" "main"
  |> must_succeed (fun (res_string, type_str) ->
         Alcotest.(check string) "Type should match" "a -> Int" type_str)

let exhaustive_records () =
  exec_code
    "main x =
       match x with
         { age = true } -> true
       end"
  |> must_fail (function
        | {Lib.Ast.Error.error = Lib.Ast.Error.Inexhaustive _}-> ()
        | e -> Alcotest.fail @@ Lib.Ast.Error.format_error e);

  let code = "main x =
                match x with
                  { age = true } -> true
                | { age = false } -> false
                end" in
  exec_with_type_and_res code "main" "main"
  |> must_succeed (fun (res_string, type_str) ->
         Alcotest.(check string) "Type should match" "{ age : Bool, .. } -> Bool" type_str)




let test_set = [
  "Exhaustiveness - booleans", `Slow, exhaustive_bools;
  "Exhaustiveness - generic captures", `Slow, exhaustive_generic_captures;
  "Exhaustiveness - captures", `Slow, exhaustive_captures;
  "Exhaustiveness - tuples", `Slow, exhaustive_tuples;
  "Exhaustiveness - lists", `Slow, exhaustive_lists;
  "Exhaustiveness - records", `Slow, exhaustive_records;
]
