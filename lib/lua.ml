type lua

external run_lua : lua -> string -> unit = "caml_exec_lua"

external new_lua : unit -> lua = "caml_new_lua"

external get_global_string : lua -> string -> string = "caml_get_string"

let exec_lua lua code = run_lua lua code
