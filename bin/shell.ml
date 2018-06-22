open Core_kernel

open Lexing
open Printf

open Lwt
open React
open LTerm_style
open LTerm_text
open LTerm_geom

open Lib.Ast
open Lib.Lexer
open Lib.Parsing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let make_prompt state =
  let prompt = "[luml]" in
  eval [ B_fg(index 14); S prompt; B_fg(index 38); S " => "]

let make_err err =
  eval [ B_fg(index 1); S err; ]

let make_value typ_ value =
  eval [ B_fg(index 2); S typ_; S " == "; S value ]

class read_line ~term ~history ~symbols = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method! completion =
    let prefix  = Zed_rope.to_string self#input_prev in
    let symbols = List.filter ~f: (fun symbol -> Zed_utf8.starts_with symbol prefix) symbols in
    self#set_completion 0 (List.map ~f: (fun symbol -> (symbol, "")) symbols)


  initializer
    self#set_prompt (S.const (make_prompt state))
end

(* Simple state machine to determine expressions vs statements *)
let is_expr command =
  let open Lib.Parser in
  let buf = Lexing.from_string command in
  let rec next () = 
    match Lib.Lexer.read buf with
    | WS -> next ()
    | tok -> tok in
  let rec binding () =
    match next () with
    | EQUALS -> false
    | IDENT _ -> binding ()
    | _ -> true
  in
  let infix_close () =
    match next () with
    | RIGHT_PARENS -> binding ()
    | _ -> false
  in
  let infix_binding () =
    match next () with
    | INFIX _ -> infix_close ()
    | INFIXR _ -> infix_close ()
    | CONS_OP -> infix_close ()
    | _ -> true
  in    
  (* Initial state- type def or (possible) binding *)
  match next () with
  | TYPE -> false
  | TYPEDEF -> false
  | IDENT _ -> binding ()
  | LEFT_PARENS -> infix_binding ()
  | _ -> true

let get_type_str env name =
  let _, type_ = String.Map.find_exn env.Lib.Typer.Env.symbols name in
  Type.format type_

let rec loop term history lua env =
  Lwt.catch (fun () ->
    (* Gather symbols from environment *)
    let local_symbols = String.Map.keys env.Lib.Typer.Env.symbols in
    let imported_symbols = env.Lib.Typer.Env.qualified_imports |> List.map ~f: (fun (_, name) -> name) in
    let foreign_symbols =
      env.Lib.Typer.Env.modules
      |> List.map ~f: (fun (mod_name, module_) ->
             module_.Lib.Ast.Module.bindings
             |> List.map ~f: (fun (binding: Lib.Ast.Binding.t) -> mod_name ^ "." ^ binding.name))
      |> List.concat in
    let symbols = List.concat [foreign_symbols; imported_symbols; local_symbols]
                  |> List.rev
                  |> List.dedup_and_sort ~compare: (String.compare) in
    (new read_line ~term ~history:(LTerm_history.contents history) ~symbols)#run
    >|= fun command -> Some command)
    (function
      | Sys.Break -> return None
      | exn -> Lwt.fail exn)
  >>= function
  | Some command ->
     begin
       LTerm_history.add history command;
       (* we keep the env, but create a fresh mod each time *)
       let mod_ = Lib.Ast.Module.empty in
       (* Determine whether we want to record a statement or eval an expression *)
       let use_result, command = if is_expr command then
                                     (true, "result = " ^ command)
                                 else
                                     (false, command) in

       let parsed_buf = command
                      |> Lexing.from_string
                      |> parse_single_with_error in

       let env, term_res = (match parsed_buf with
        | Ok None ->
              env, LTerm.eprint "[Nothing entered]"

        | Ok (Some statement) ->
            (try let (env, mod_) = statement
                                   |> Lib.Ast.Statement.post_process 
                                   |> List.fold_left ~f: (fun (env, mod_) statement ->
                                        Lib.Typer.from_statement failwith (env, mod_) statement)
                                                     ~init: (env, mod_) in
                let lua_code = Lib.Compile.make_module mod_  ^ "\nfor k, v in pairs(__empty__) do _G[k] = v\n end" in

                  Lib.Lua.exec_lua lua lua_code;
                 (match use_result with

                   | true -> Lib.Lua.exec_lua lua "merle_output = Base.toString(result);";
                             let output_value = Lib.Lua.get_global_string lua "merle_output" in
                             let type_str = get_type_str env "result" in
                             env,
                             LTerm.printls (make_value type_str output_value)
                   | false -> env, LTerm.print "")
             with
               CompileError err -> env, LTerm.printls (make_err (Error.format_error err)))

        | Error err ->
            env, LTerm.printls (make_err (Error.format_error err))) in

       term_res >>= fun () -> loop term history lua env
      end
  | None ->
    loop term history lua env

let from_statements (env: Lib.Typer.Env.t) statements =
  let resolver name = 
    let n, m = List.find_exn ~f: (fun (n, m) -> n = name) env.modules in
    m in

  List.fold_left
      ~f: (Lib.Typer.from_statement resolver)
      ~init: (env, Lib.Ast.Module.empty)
      statements

let load_modules deps_dirs main prelude = 
  let mlo_files = deps_dirs |> List.concat_map ~f: (Lib.Project.gather ".mlo") in 
  let loader = Fn.compose Lib.Project.CompiledModule.t_of_sexp Sexp.load_sexp in
  let built_modules = List.map ~f: loader mlo_files in
  let lua_code = In_channel.read_all main in
  let prelude = In_channel.read_all prelude in
  (built_modules, lua_code, prelude)

let rec gather_prelude lexbuf =
  let reader = Lib.Parsing.reader () in
  let rec inner lexbuf =
    match parse_with_error reader lexbuf with
    | Ok None -> []
    | Ok (Some statement) -> statement :: inner lexbuf
    | Error err -> failwith ("Got an error: " ^ (Error.format_error err)) in
  inner lexbuf

let apply_prelude lexbuf lua modules env =
  let statements = gather_prelude lexbuf in
  let env, mod_ = from_statements env statements in
  let lua_code = Lib.Compile.make_module mod_ in
  Lib.Lua.exec_lua lua (lua_code ^ "\nfor k, v in pairs(__empty__) do _G[k] = v end");
  env

let init_env lua (env: Lib.Typer.Env.t) (project : Lib.Project.Project.t option): Lib.Typer.Env.t =
  let stdlib_path = 
    (*
    Sys.argv 
    |> Array.to_list
    |> List.hd_exn
    |> Filename.dirname 
    |> Str.split (Str.regexp_string (Filename.dir_sep))
    |> List.rev
    |> (fun x -> List.drop x 1)
    |> List.rev
    |> (fun x -> x @ ["lib"; "luml"; "stdlib"])
    |> String.concat ~sep: Filename.dir_sep in
    *)
    "/usr/local/lib/luml/stdlib" in
  let prelude_path = stdlib_path ^ Filename.dir_sep ^ "base.prelude" in
  let prelude_build_path = stdlib_path ^ Filename.dir_sep ^ "build" in
  let prelude_build_artefact = prelude_build_path ^ Filename.dir_sep ^ "stdlib.lua" in

  let mods, prelude, raw_mods = match project with
  | Some project ->
      let artefact_path = "build/" ^ project.name ^ "/" ^ project.name ^ ".lua" in
      let build_path = "build/" ^ project.name in

      let open Lib.Project.CompiledModule in
      let raw_mods, code, prelude = load_modules [prelude_build_path; build_path] artefact_path prelude_path in
      let mods = List.map ~f: (fun m -> (m.name, Option.value_exn m.module_)) raw_mods in
      Lib.Lua.exec_lua lua ("local result = (function () " ^ code ^ "end)()\nfor k, v in pairs(result) do _G[k] = v end");
      mods, prelude, raw_mods
  | None ->
      let open Lib.Project.CompiledModule in
      let raw_mods, code, prelude = load_modules [prelude_build_path] prelude_build_artefact prelude_path in
      let mods = List.map ~f: (fun m -> (m.name, Option.value_exn m.module_)) raw_mods in
    (* Exec prebuilt Lua code as a chunk and inject all modules into the global environment *)
      Lib.Lua.exec_lua lua ("local result = (function () " ^ code ^ "end)()\nfor k, v in pairs(result) do _G[k] = v end");
      mods, prelude, raw_mods in
  (* Inject all modules into the environment so we know how to type them *)

  let env = { env with modules = mods } in
  (* Apply the stdlib prelude *)
  apply_prelude (Lexing.from_string prelude) lua raw_mods env

let run_shell ?project () =
  Lazy.force LTerm.stdout
  >>= (fun term ->
  let lua = Lib.Lua.new_lua () in
  let env = Lib.Typer.Env.empty in
  let env = init_env lua env project in
  loop term (LTerm_history.create []) lua env)
 
