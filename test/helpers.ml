open Core_kernel
open Lib

let get_type_str env name =
  let _, type_ = String.Map.find_exn env.Lib.Typer.Env.symbols name in
  Ast.Type.format type_

let (>>=) res f =
  match res with
  | Ok data -> f data
  | Error err -> Error err

let (<*>) res f =
  match res with
  | Ok data -> Ok (f data)
  | Error err -> Error err

let safe_type (env, mod_) statement =
  try Ok (Lib.Typer.from_statements statement)
  with
    Ast.CompileError err -> Error err

let exec_code code =
  let (env, mod_) = ( Lib.Typer.Env.empty,
                      Lib.Ast.Module.empty 
                    ) in
  code
  |>  Lexing.from_string
  |>  Parsing.parse_all_with_error
  <*> List.map ~f: Ast.Statement.post_process
  <*> List.concat
  >>= safe_type (env, mod_)
  >>= fun (env, mod_) -> Exhaustive.check_module mod_
  >>= fun (mod_) ->
    begin
      let lua_code = Lib.Compile.make_module mod_ in
      let lua = Lib.Lua.new_lua () in
      Lib.Lua.exec_lua lua (lua_code ^ "\nTESTS = Main\n");
      Ok (lua, env)
    end

let exec_with_type_and_res code fun_name type_name =
  exec_code code (* define module globally *)
  >>= (fun (lua, env) ->
         Lib.Lua.exec_lua lua ("__RESULT__=tostring(TESTS." ^ fun_name ^ ")");
         let res_string = Lib.Lua.get_global_string lua "__RESULT__" and
             type_str = get_type_str env type_name in
         Ok (res_string, type_str))

let must_succeed f res =
  match res with
  | Ok value -> f value
  | Error err -> Alcotest.fail (Ast.Error.format_error err)

let must_fail f res =
  match res with
  | Ok value -> Alcotest.fail "Code compile did not fail"
  | Error err -> f err
