open Lib
open Helpers


let infer_recur () =
  let code = "map f l = match l with [] -> [] | h :: t -> (f h) :: map f t end" in
  exec_with_type_and_res code "map" "map"
  |> must_succeed (fun (res_string, type_str) ->
         Alcotest.(check string)
                 "List Map Type"
                 "(a -> b) -> List a -> List b"
                 type_str)

let infer_recur2 () =
  let code = "foldl f init items =
  match items with
      [] -> init
    | h :: t -> foldl f (f init h) t
  end
map f = foldl (\\acc i -> (f i) :: acc) []" in

  exec_with_type_and_res code "map" "map"
  |> must_succeed (fun (res_string, type_str) ->
         Alcotest.(check string)
                 "Foldl version of map"
                 "(a -> b) -> List a -> List b"
                 type_str)


let adt () =
  let code = "type Jim = Bob String | Steve (Int -> String)\nhello = Steve\nmain = hello (\\x -> \"nope\")" in
  exec_with_type_and_res code "hello" "hello"
  |> must_succeed (fun (res_string, type_str) ->
         Alcotest.(check string) "Constructor yields correct type" type_str "(Int -> String) -> Jim")

let poly_constructor () =
  let code = "type Option a = Some a | None\nmain = Some 42" in
  exec_with_type_and_res code "main" "main"
  |> must_succeed (fun (res_string, type_str) ->
         Alcotest.(check string) "Constructor yields correct type" type_str "Option Int")


let adt_match () =
  let code = "type AdtTest = Only Int\nmain = match (Only 10) with Only 10 -> 10|Only x -> 0 end\n" in
  exec_with_type_and_res code "main" "main"
  |> must_succeed (fun (res_string, type_str) ->
        Alcotest.(check string) "Constructor result is 10" "10" res_string)

let poly_adt_unification () =
  let code = "type Test a = Test a\ncompare : a -> a -> Bool\ncompare x y = @lua.+ x y\nmain = compare (Test 42) (Test false)" in
  let res = exec_code code in
  must_fail (function
      | {Lib.Ast.Error.error = Lib.Ast.Error.TypeMismatch _} -> ()
      | _ -> Alcotest.fail "Expected type error") res

let adt_type_sig () =
  let open Lib.Ast.Error in
  let code = 
    [ "type WorAdt = WorAdt Int";
      "pairWorAdt : WorAdt -> (WorAdt, WorAdt)";
      "pairWorAdt x = (x, x)"
    ] |> String.concat "\n" in
  exec_with_type_and_res code "pairWorAdt" "pairWorAdt"
  |> must_succeed (fun (res_string, type_str) ->
       Alcotest.(check string) 
        "Should have adopted the type from the sig" 
        "WorAdt -> (WorAdt, WorAdt)" 
        type_str);
  (* Is the signature respected? *)
  code ^ "\nmain = pairWorAdt 15"
  |> exec_code
  |> must_fail (function
                | { error = TypeMismatch _ } -> () 
                | err ->
                    Alcotest.fail
                      ("Expected TypeMismatch, got " ^ (format_error err)))

let adt_type_sig_with_args () =
  let open Lib.Ast.Error in
  let code = 
    [ "type WorAdt a = WorAdt a";
      "pairWorAdt : (WorAdt String) -> ((WorAdt String), (WorAdt String))";
      "pairWorAdt x = (x, x)"
    ] |> String.concat "\n" in
  let success = "\nmain = pairWorAdt (WorAdt \"aye that'll dee\")" in
  exec_with_type_and_res (code ^ success) "main" "main"
  |> must_succeed (fun (res_string, type_str) ->
       Alcotest.(check string) 
        "Type should match" 
        "(WorAdt String, WorAdt String)" 
        type_str);
  (* Is the signature respected? *)
  code ^ "\nmain = pairWorAdt (WorAdt true)"
  |> exec_code
  |> must_fail (function
                | { error = TypeMismatch _ } -> ()
                | err ->
                    Alcotest.fail
                      ("Expected TypeMismatch, got " ^ (format_error err)))

let adt_type_sig_with_args_list () =
  let open Lib.Ast.Error in
  let code = 
    [ "type WorAdt a = WorAdt a";
      "listWorAdt : (WorAdt String) -> List (WorAdt String)";
      "listWorAdt x = [x]"
    ] |> String.concat "\n" in
  let success = "\nmain = listWorAdt (WorAdt \"aye that'll dee\")" in
  exec_with_type_and_res (code ^ success) "main" "main"
  |> must_succeed (fun (res_string, type_str) ->
       Alcotest.(check string) 
        "Type should match" 
        "List WorAdt String"
        type_str)

let record_update_unify () =
  let code = "main x = { x | num = 42 }" in
  exec_with_type_and_res code "main" "main"
  |> must_succeed (fun (res_string, type_str) ->
       Alcotest.(check string)
        "Type should match"
        "{ num : Int, .. } -> { num : Int, .. }"
        type_str)

let record_type_sig () =
  let code =
    [ "main : { name : String } -> { name : String }";
      "main x = { x | name = \"michael\"}" ] |> String.concat "\n" in
  let fail =
     "\nres = main { name = \"steve\", age = 42 }"  in
  exec_with_type_and_res code "main" "main"
  |> must_succeed (fun (res_string, type_str) ->
       Alcotest.(check string)
        "Type should match"
        "{ name : String } -> { name : String }"
        type_str);
  let open Lib.Ast.Error in
  (code ^ fail)
  |> exec_code
  |> must_fail (function
                | { error = FieldMismatch _ } -> ()
                | err ->
                    Alcotest.fail
                      ("Exepected FieldMismatch, got " ^ (format_error err)))

let typedef_test () =
  let code =
    [ "typedef DoubleInt = (Int, Int)";
      "main : Int -> Int -> DoubleInt";
      "main x y = (x, y)" ] |> String.concat "\n" in
  exec_with_type_and_res code "main" "main"
  |> must_succeed (fun (res_string, type_str) ->
       Alcotest.(check string)
        "Type should match"
        "Int -> Int -> DoubleInt"
        type_str)

let polytypedef_tuple_test () =
  let open Lib.Ast.Error in
  [ "typedef Pair a = (a, a)";
    "main : Int -> String -> Pair Int";
    "main x y = (x, y)" ] |> String.concat "\n"
  |> exec_code
  |> must_fail (function
                | { error = TypeSignatureMismatch _ } -> ()
                | err ->
                    Alcotest.fail
                      ("Expected TypeSignatureMismatch, got " ^ (format_error err)));
  let code = [ "typedef Pair a = (a, a)";
    "main : Int -> Int -> Pair Int";
    "main x y = (x, y)" ] |> String.concat "\n" in
    exec_with_type_and_res code "main" "main"
    |> must_succeed (fun (res_string, type_str) ->
       Alcotest.(check string)
        "Type should match"
        "Int -> Int -> Pair Int"
        type_str)



let polytypedef_record_test () =
  let open Lib.Ast.Error in
  [ "typedef MyRecord a = { value : a}";
    "main : String -> MyRecord Int";
    "main x = { value = x }" ] |> String.concat "\n"
  |> exec_code
  |> must_fail (function
                | { error = TypeSignatureMismatch _ } -> ()
                | err ->
                    Alcotest.fail
                      ("Expected TypeMismatch, got " ^ (format_error err)));

  let code =
    [ "typedef MyRecord a = { value : a }";
      "main : Int -> MyRecord Int";
      "main x = { value = x }" ] |> String.concat "\n" in
  exec_with_type_and_res code "main" "main"
  |> must_succeed (fun (res_string, type_str) ->
       Alcotest.(check string)
        "Type should match"
        "Int -> MyRecord Int"
        type_str)

let polytypedef_list_test () =
  let open Lib.Ast.Error in
  [ "typedef MyType a = ((List a), a)";
    "main : String -> MyType Int";
    "main x = ([x], 5)" ] |> String.concat "\n"
  |> exec_code
  |> must_fail (function
                | { error = TypeMismatch _ } -> ()
                | err ->
                    Alcotest.fail
                      ("Expected TypeMismatch, got " ^ (format_error err)));

  let code = [ "typedef MyType a = ((List a), a)";
    "main : Int -> MyType Int";
    "main x = ([x], x)" ] |> String.concat "\n" in

  exec_with_type_and_res code "main" "main"
  |> must_succeed (fun (res_string, type_str) ->
       Alcotest.(check string)
        "Type should match"
        "Int -> MyType Int"
        type_str)

let recursive_type_def () =
  let open Lib.Ast.Error in
  [ "type Cons a = Cons (a, (Cons a)) | Nil";
    "main x = Cons (true, (Cons (42, Nil)))"] |> String.concat "\n"
  |> exec_code
  |> must_fail (function
                | { error = TypeMismatch _ } -> ()
                | err ->
                    Alcotest.fail
                      ("Expected TypeMismatch, got " ^ (format_error err)))

let test_set = [
  "Inference with recursive funs", `Slow, infer_recur;
  "Inference with recursive funs 2", `Slow, infer_recur2;
  "Adt Constructor", `Slow, adt;
  "Poly constructor", `Slow, poly_constructor;
  "Adt match", `Slow, adt_match;
  "Poly Adt Unification", `Slow, poly_adt_unification;
  "ADT in type sig", `Slow, adt_type_sig;
  "ADT with args in type sig", `Slow, adt_type_sig_with_args;
  "ADT with args in List type sig", `Slow, adt_type_sig_with_args_list;
  "Record update unification", `Slow, record_update_unify;
  "Record type sig", `Slow, record_type_sig;
  "Typedef", `Slow, typedef_test;
  "Poly typedef tuple", `Slow, polytypedef_tuple_test;
  "Poly typedef record", `Slow, polytypedef_record_test;
  "Recursive type def", `Slow, recursive_type_def;
	]
