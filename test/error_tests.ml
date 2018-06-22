open Lib
open Helpers

let adt_missing_param_error () =
  let open Lib.Ast.Error in
  "type Maybe = Just a | Nothing"
  |> exec_code
  |> must_fail (function
      | { error = TypeParamMissing _} -> ()
      | _ -> Alcotest.fail "Expected error")

let adt_unknown_error () =
  let open Lib.Ast.Error in
  [ "main x =";
    "  match x with";
    "    WorAdt v -> true";
    "  end"
  ] |> String.concat "\n"
  |> exec_code
  |> must_fail (function
      | { error = IdentifierNotFound _ } -> ()
      | err ->
        Alcotest.fail 
          ("Expected IdentifierNotFound, got " ^ (format_error err)))

let cyclical_type_error () =
  let open Lib.Ast.Error in
  "append x y = (x, y) :: y"
  |> exec_code
  |> must_fail (function
      | { error = CyclicalType _ } -> ()
      | err ->
          Alcotest.fail ("Expected CyclicalType, got " ^ (format_error err)))


let test_set = [
  "Type param missing error", `Slow, adt_missing_param_error;
  "Unknown ADT error", `Slow, adt_unknown_error;
  "Cyclical type error", `Slow, cyclical_type_error;
]