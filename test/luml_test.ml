(* luml Test Suite Entry Point *)

let () =
  Alcotest.run "luml Compiler" [
    "Typing Tests", Typing_tests.test_set;
    "Lua Compile Tests", Lua_tests.test_set;
    "Exhaustiveness tests", Exhaustive_tests.test_set;
    "Error Tests", Error_tests.test_set;
    "Parsing Tests", Parsing_tests.test_set;
  ]
