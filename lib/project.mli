open Core

module CompiledModule : sig
  type t = {
    name: string;
    imports: string list;
    statements: Ast.Statement.t list;
    module_: Ast.Module.t option;
    result: string option} [@@deriving sexp]
end

module Project : sig
  type t = {
    name: string;
    prelude: string sexp_option;
    output: string sexp_option
  } [@@deriving sexp]
end

val gather : string -> string -> string list
val build_tree :  ?prelude:(string) -> string list -> string list -> string -> unit
val link : string list -> string -> unit 
val module_resolver : CompiledModule.t list -> string -> Ast.Module.t
val build_project : Project.t -> Project.t
