open Lib
open Helpers

let breaks () =
  let code = "(+) : Int -> Int -> Int\n(+) x y =\n  @lua.+ x y\nadd10 = (+) 10" in
              (*add10 = (+) 10 in *)
  exec_with_type_and_res code "add10" "add10"
  |> must_succeed (fun (res_string, type_str) ->
         Alcotest.(check string) "Inferred Breaks" type_str "Int -> Int")

let test_set = [
  "Basic inference of breaks", `Slow, breaks;
]
