let () =
  print_endline "Building stdlib" ;
  Lib.Project.build_tree ["stdlib/src"] [] "stdlib/build" ;
  Lib.Project.link ["stdlib/build"] "stdlib/build/stdlib.lua"
