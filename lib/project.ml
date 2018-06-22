open Core

(* Handle the building of source trees *)

module Project = struct
  type t =
    {name: string; prelude: string sexp_option; output: string sexp_option}
  [@@deriving sexp]
end

module CompiledModule = struct
  type t =
    { name: string
    ; imports: string list
    ; statements: Ast.Statement.t list
    ; module_: Ast.Module.t option
    ; result: string option }
  [@@deriving sexp]
end

let rec gather extension dir =
  Caml.Sys.readdir dir |> Array.to_list
  |> List.concat_map ~f:(fun f ->
         let path = Filename.concat dir f in
         if Sys.is_directory path = `Yes then gather extension path
         else if Filename.check_suffix path extension then [path]
         else [] )

let parse_file f =
  In_channel.read_lines f |> String.concat ~sep:"\n" |> Lexing.from_string
  |> Parsing.parse_all_with_error

let abort fn error =
  let open ANSITerminal in
  eprintf [Foreground Red] "Error: couldn't compile '%s':\n%s\n\n" fn
    (Ast.Error.format_error error) ;
  exit 1

let rec parse_files = function
  | [] -> []
  | h :: t ->
    match parse_file h with
    | Error err -> abort h err
    | Ok sts -> (h, sts) :: parse_files t

let gather_imports (statements: Ast.Statement.t list) : string list =
  List.filter_map statements (function
    | Ast.Statement.Import import -> Some import.module_
    | _ -> None )

let rec gather_mod_name = function
  | Ast.Statement.Module module_ :: _ -> module_.name
  | _ :: t -> gather_mod_name t
  | [] -> failwith "No module name"

let rec init_module (fn, sts) : CompiledModule.t =
  let imports = gather_imports sts in
  let mod_name = gather_mod_name sts in
  {name= mod_name; statements= sts; module_= None; imports; result= None}

let module_resolver mods name =
  let (mod_ : CompiledModule.t) =
    List.find_exn ~f:(fun m -> m.CompiledModule.name = name) mods
  in
  mod_.module_ |> Option.value_exn

let type_module (built: CompiledModule.t list) (mod_: CompiledModule.t) :
    CompiledModule.t list =
  match
    Typer.type_statements mod_.statements
      ~module_resolver:(module_resolver built)
  with
  | Error err -> abort mod_.name err
  | Ok (env, typed_mod) -> {mod_ with module_= Some typed_mod} :: built

let rec type_modules (mods: CompiledModule.t list) : CompiledModule.t list =
  List.fold_left ~f:type_module ~init:[] mods |> List.rev

let compile_module mods (mod_: CompiledModule.t) : CompiledModule.t list =
  let result = Some (Compile.make_module (Option.value_exn mod_.module_)) in
  {mod_ with result} :: mods

let rec compile_modules (mods: CompiledModule.t list) : CompiledModule.t list =
  List.fold_left ~init:[] ~f:compile_module mods

let dependency_order (modules: CompiledModule.t list) =
  let module G = Graph.Imperative.Digraph.Abstract (struct
    type t = string
  end) in
  let module TP = Graph.Topological.Make (G) in
  let graph = G.create () in
  (* Create all vertices and store in a map *)
  let node_set =
    List.fold_left
      ~f:(fun acc (module_: CompiledModule.t) ->
        match String.Map.mem acc module_.name with
        | true -> acc
        | false ->
            let vec = G.V.create module_.name in
            G.add_vertex graph vec ;
            String.Map.set acc module_.name vec )
      ~init:String.Map.empty modules
  in
  (* Add all edges - imperatively, hence iter *)
  List.iter modules ~f:(fun {name; imports} ->
      List.iter imports ~f:(fun import ->
          let v1 = String.Map.find_exn node_set name in
          let v2 = String.Map.find_exn node_set import in
          G.add_edge graph v1 v2 ) ) ;
  match modules with
  | [] -> []
  | [mod_] -> [mod_.name]
  | _ -> TP.fold List.cons graph [] |> List.map ~f:G.V.label

let build_tree ?(prelude: string option) src_dirs build_dirs output_dir =
  (* gather .ml files in src_dirs *)
  let source_files = src_dirs |> List.concat_map ~f:(gather ".ml") in
  (* gather .mlo files in build dirs*)
  let mlo_files = build_dirs |> List.concat_map ~f:(gather ".mlo") in
  let loader = Fn.compose CompiledModule.t_of_sexp Sexp.load_sexp in
  let built_modules = List.map ~f:loader mlo_files in
  (* TODO - determine if we have builds more recent than src *)
  (* TODO - can we avoid parsing source if the mlo_files are more recent? *)
  (* run an initial parse on new src to build, aborting if any syntax errors *)
  let parsed_statements = parse_files source_files in
  (* If we have a prelude, open it and prepend its statements to each module *)
  let prelude_statements =
    match prelude with
    | Some filename -> parse_file filename |> Result.ok |> Option.value_exn
    | _ -> []
  in
  let parsed_statements =
    parsed_statements
    |> List.map ~f:(fun (name, statements) ->
           let statements =
             List.concat_map ~f:Ast.Statement.post_process statements
           in
           (name, prelude_statements @ statements) )
  in
  (* TODO - determine dependencies from already built artefacts too *)
  (* TODO be aware of conflicts *)
  (* TODO determine if any dependencies now dirty and build artefacts require rebuilding *)
  (* Construct module objects from parsed statements *)
  let parsed_modules = List.map ~f:init_module parsed_statements in
  let all_modules = parsed_modules @ built_modules in
  (* Determine dependency order *)
  let deps_order = dependency_order all_modules in
  let ordered_modules =
    List.map deps_order (fun mod_name ->
        List.find_exn all_modules ~f:(fun {CompiledModule.name} ->
            mod_name = name ) )
  in
  let typed_modules = type_modules ordered_modules in
  (* TODO - exhaustive check here!! *)
  let compiled_modules =
    compile_modules typed_modules
    |> List.filter ~f:(fun {CompiledModule.name} ->
           List.exists parsed_modules (fun mod_ ->
               mod_.CompiledModule.name = name ) )
  in
  List.iter
    ~f:(fun m ->
      print_endline ("Compiled " ^ m.name) ;
      let filename = Filename.concat output_dir (m.name ^ ".mlo") in
      m |> CompiledModule.sexp_of_t |> Sexp.save_hum filename )
    compiled_modules

let link build_dirs output =
  let mlo_files = build_dirs |> List.concat_map ~f:(gather ".mlo") in
  let loader = Fn.compose CompiledModule.t_of_sexp Sexp.load_sexp in
  let modules = List.map ~f:loader mlo_files in
  let deps_order = dependency_order modules in
  let ordered_modules =
    List.map deps_order (fun mod_name ->
        let mod_ =
          List.find_exn modules ~f:(fun {CompiledModule.name} ->
              mod_name = name )
        in
        Option.value_exn mod_.result )
  in
  let mod_exports =
    deps_order |> List.map ~f:(fun s -> s ^ "=" ^ s) |> String.concat ~sep:","
  in
  let module_export = "\n\n return {" ^ mod_exports ^ "}" in
  module_export
  |> ( ^ ) (String.concat ~sep:"\n\n" ordered_modules)
  |> Out_channel.write_all output

let build_project (project: Project.t) =
  let () = Unix.mkdir_p ("build/" ^ project.name) in
  let prelude, prelude_dirs =
    match project.prelude with
    | None ->
        ( "/usr/local/lib/luml/stdlib/base.prelude"
        , ["/usr/local/lib/luml/stdlib/build"] )
    | Some prelude -> (prelude, [])
  in
  build_tree ~prelude ["src"] ("build" :: prelude_dirs)
    ("build/" ^ project.name) ;
  link ("build" :: prelude_dirs)
    ("build/" ^ project.name ^ "/" ^ project.name ^ ".lua") ;
  project
