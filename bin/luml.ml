open Core_kernel

let build args =
  (* Find a luml.project file *)
  let project =
    Sexplib.Sexp.load_sexp_conv_exn "luml.project"
      Lib.Project.Project.t_of_sexp
  in
  Lib.Project.build_project project

let print_usage () =
  print_endline
    "Usage:\n\n    \
     luml        -> run the interactive shell\n    \
     luml build  -> run a project build\n    \
     luml help   -> print this help text\n    \
     "

let run_shell () =
  (* See if we are running standalone or in a project *)
  if Sys.file_exists "luml.project" then
    let project = build [] in
    Lwt_main.run (Shell.run_shell ~project ())
  else print_endline "No 'luml.project' file - running standalone shell" ;
  Lwt_main.run (Shell.run_shell ())

let () =
  match Array.to_list Sys.argv with
  | [_path] -> run_shell ()
  | _ :: "build" :: rest ->
      let _ = build rest in
      ()
  | [_; "help"] -> print_usage ()
  | _ -> print_usage ()
