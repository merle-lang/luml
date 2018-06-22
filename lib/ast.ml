open Core_kernel
open Printf

module Pos = struct
  type t = {file: string; line: int; char: int} [@@deriving sexp]
end

module Source = struct
  type t = None | Pos of Pos.t [@@deriving sexp]
end

(* To produce Core Erlang compatible identifiers, we need to know a little
   bit about what type it refers to. All functions in Merle are single arity;
   top level functions are constructed as zero-arity functions when compiled *)
module IdentType = struct
  type t = Unknown | OuterFun | OuterVal | Var [@@deriving sexp]
end

module Identifier = struct
  type t = {name: string; type_: IdentType.t ref; module_: string option ref}
  [@@deriving sexp]

  let make name type_ =
    if String.contains name '.' && not (String.contains name '(') then
      let parts = String.split ~on:'.' name |> List.rev in
      match parts with
      | [] -> failwith ("Bad module identifier " ^ name)
      | ident_name :: mod_parts ->
          let module_ = ref (Some (mod_parts |> String.concat ~sep:".")) in
          {name= ident_name; type_; module_}
    else {name; type_; module_= ref None}
end

module rec TypeArg : sig
  type t = {id: int; name: string option; instance: Type.t option ref}
  [@@deriving sexp]

  val id_counter : int ref

  val make : unit -> t
end = struct
  type t = {id: int; name: string option; instance: Type.t option ref}
  [@@deriving sexp]

  let id_counter = ref 0

  let make () =
    id_counter := !id_counter + 1 ;
    {id= !id_counter; name= None; instance= ref None}
end

and Variant : sig
  type t = {name: string; args: Type.t list} [@@deriving sexp]
end = struct
  type t = {name: string; args: Type.t list} [@@deriving sexp]
end

and Adt : sig
  type t = {name: string; args: Type.t list; variants: Variant.t list}
  [@@deriving sexp]
end = struct
  type t = {name: string; args: Type.t list; variants: Variant.t list}
  [@@deriving sexp]
end

and SelfRef : sig
  type t = {name: string; args: Type.t list} [@@deriving sexp]
end = struct
  type t = {name: string; args: Type.t list} [@@deriving sexp]
end

and UserType : sig
  type t = {name: string; args: Type.t list; type_: Type.t option}
  [@@deriving sexp]
end = struct
  type t = {name: string; args: Type.t list; type_: Type.t option}
  [@@deriving sexp]
end

and Record : sig
  type t = {fields: (string * Type.t) list} [@@deriving sexp]
end = struct
  type t = {fields: (string * Type.t) list} [@@deriving sexp]
end

and PolyRecord : sig
  type t = {fields: (string * Type.t) list; poly: t option ref}
  [@@deriving sexp]

  val get_base : t -> t
end = struct
  type t = {fields: (string * Type.t) list; poly: t option ref}
  [@@deriving sexp]

  let rec get_base r =
    match !(r.poly) with Some poly -> get_base poly | None -> r
end

and Alias : sig
  type t = {name: string; type_: Type.t; args: Type.t list} [@@deriving sexp]
end = struct
  type t = {name: string; type_: Type.t; args: Type.t list} [@@deriving sexp]
end

and Type : sig
  type t =
    | Arrow of (t * t)
    | Int
    | Float
    | String
    | Bool
    | TypeArg of TypeArg.t
    | Tuple of t list
    | List of t
    | Adt of Adt.t
    | SelfRef of SelfRef.t
    | Record of Record.t
    | PolyRecord of PolyRecord.t
    | Alias of Alias.t
    | UserType of UserType.t
    | Unit
  [@@deriving sexp]

  type type_map = string Int.Map.t

  val format : ?ugly_args:bool -> t -> string

  val copy : t -> Type.t list -> t
end = struct
  type t =
    | Arrow of (t * t)
    | Int
    | Float
    | String
    | Bool
    | TypeArg of TypeArg.t
    | Tuple of t list
    | List of t
    | Adt of Adt.t
    | SelfRef of SelfRef.t
    | Record of Record.t
    | PolyRecord of PolyRecord.t
    | Alias of Alias.t
    | UserType of UserType.t
    | Unit
  [@@deriving sexp]

  type type_map = string Int.Map.t

  let format ?ugly_args t =
    let rec inner (arg_map: type_map) targ_index t =
      let add_parens str =
        if String.contains str ' ' then
          "(" ^ str ^ ")"
        else
          str in
      let only x = (arg_map, targ_index, x) in
      let add_arg id =
        let name = 97 + targ_index |> Caml.Char.chr |> Caml.Char.escaped in
        (Int.Map.set arg_map ~key:id ~data:name, targ_index + 1, name)
      in
      match t with
      | Unit -> only "()"
      | Int -> only "Int"
      | Float -> only "Float"
      | String -> only "String"
      | Bool -> only "Bool"
      | UserType {name; args} ->
          let arg_str =
            List.map ~f:(inner arg_map targ_index) args
            |> List.map ~f:(fun (_, _, v) -> v)
          in
          only
            ( ["<unchecked user type>"; name] @ arg_str
            |> String.concat ~sep:" " )
      | Alias {name; args} ->
          let arg_str =
            List.map ~f:(inner arg_map targ_index) args
            |> List.map ~f:(fun (_, _, v) -> v)
          in
          only (name :: arg_str |> String.concat ~sep:" ")
      | TypeArg {TypeArg.name; instance; id} -> (
        match (!instance, name) with
        | Some inst, _ -> inner arg_map targ_index inst
        | None, None ->
            if ugly_args = None then
              match Int.Map.mem arg_map id with
              | true -> only (Map.find arg_map id |> Option.value ~default:"_")
              | false -> add_arg id
            else only @@ "Arg: " ^ string_of_int id
        | None, Some name ->
            only @@ name
            ^
            if ugly_args = None then "" else "[Arg: " ^ string_of_int id ^ "]"
        )
      | Arrow (arg, ret) ->
          let arg_map, targ_index, arg_str = inner arg_map targ_index arg in
          let arg_map, targ_index, ret_str = inner arg_map targ_index ret in
          (* TODO we should have a 'populated type' function to work around type args *)
          let arg_str =
            match arg with
            | Arrow _ -> add_parens arg_str
            | TypeArg a -> (
              match !(a.instance) with
              | Some (Arrow _) -> add_parens arg_str
              | _ -> arg_str )
            | _ -> arg_str
          in
          (arg_map, targ_index, arg_str ^ " -> " ^ ret_str)
      | List t ->
          let _, _, t_str = inner arg_map targ_index t in
          only (sprintf "List %s" t_str)
      | Tuple types ->
          (* todo - need to fold this, not map, so we can preserve the type arg env *)
          types
          |> List.fold_left
               ~f:(fun (arg_map, targ_index, acc) x ->
                 let arg_map, targ_index, s = inner arg_map targ_index x in
                 (arg_map, targ_index, acc @ [s]) )
               ~init:(arg_map, targ_index, [])
          |> (fun (_, _, s) -> s)
          |> String.concat ~sep:", " |> sprintf "(%s)" |> only
      | Adt {name; args; variants} ->
          let arg_list =
            args
            |> List.map ~f:(inner arg_map targ_index)
            |> List.map ~f:(fun (_, _, t) -> t)
            |> List.filter ~f:(( <> ) "")
            |> List.map ~f: add_parens
            |> String.concat ~sep:" "
          in
          (*
          let variant_list =
            List.map
              ~f: (fun {Variant.name; args} ->
                let variant_args =
                  args
                  |> List.map ~f: (fun x ->
                         let _, _, s = inner arg_map targ_index x in
                         s )
                  |> String.concat ~sep: " "
                in
                name ^ (if (String.length variant_args) > 0 then (" " ^ variant_args) else "")) 
              variants
            |> String.concat ~sep: " | " 
          in *)
          (* TODO - perhaps if we had a 'verbose' flag we would show all the variants *)
          only
          @@ ( [name; arg_list]
             |> List.filter ~f:(( <> ) "")
             |> String.concat ~sep:" " )
      | SelfRef {name} -> only name
      | Record {fields} ->
          (* let end_ = if poly then ", .. }" else " }" in *)
          let fieldstr =
            fields
            |> List.map ~f:(fun (name, t) ->
                   let _, _, type_ = inner arg_map targ_index t in
                   sprintf "%s : %s" name type_ )
            |> String.concat ~sep:", "
          in
          only @@ "{ " ^ fieldstr ^ " }"
      | PolyRecord pr ->
          let fields = (PolyRecord.get_base pr).fields in
          let fieldstr =
            fields
            |> List.fold_left
                 ~f:(fun (arg_map, targ_index, acc) (name, t) ->
                   let arg_map, targ_index, type_ =
                     inner arg_map targ_index t
                   in
                   let str = sprintf "%s : %s" name type_ in
                   (arg_map, targ_index, acc @ [str]) )
                 ~init:(arg_map, targ_index, [])
            |> (fun (_, _, args) -> args)
            |> String.concat ~sep:", "
          in
          only @@ "{ " ^ fieldstr ^ ", .. }"
    in
    let _, _, str = inner Int.Map.empty 0 t in
    str

  let copy t non_free =
    let rec get_instance_type t =
      match t with
      | Type.TypeArg {TypeArg.instance} -> (
        match !instance with Some inst -> get_instance_type inst | _ -> t )
      | _ -> t
    in
    let non_free_fresh = List.map ~f:get_instance_type non_free in
    let type_map = Int.Map.empty in
    let rec inner type_ map =
      match get_instance_type type_ with
      | TypeArg {TypeArg.name; instance; id} -> (
        match
          (* already in symbol map? *)
          ( Int.Map.mem map id
          , List.exists
              ~f:(function Type.TypeArg t -> id = t.id | _ -> false)
              non_free_fresh )
        with
        | true, _ -> (Int.Map.find_exn map id, map)
        | _, true -> (type_, map)
        | false, false ->
          match !instance with
          | Some t ->
              failwith "Non-generic instance found where it shouldn't be"
              (* this shouldn't happen, ever *)
          | None ->
              let t = TypeArg (TypeArg.make ()) in
              (t, Map.set map ~key:id ~data:t)
          (* ignore name for now... *) )
      | Arrow (expr, arg) ->
          let expr2, map2 = inner expr map in
          let arg2, map3 = inner arg map2 in
          (Arrow (expr2, arg2), map3)
      | Tuple args ->
          let args, map =
            List.fold_left
              ~f:(fun (acc, map) el ->
                let t, map = inner el map in
                (t :: acc, map) )
              ~init:([], map) args
          in
          (Tuple (List.rev args), map)
      | List t ->
          let arg, map = inner t map in
          (List arg, map)
      | Record r ->
          let args, map =
            List.fold_left
              ~f:(fun (acc, map) (field, t) ->
                let t, map = inner t map in
                ((field, t) :: acc, map) )
              ~init:([], map) r.fields
          in
          (Record {fields= args}, map)
      | PolyRecord r ->
          if
            List.exists
              ~f:(function
                  | Type.PolyRecord pr -> pr = PolyRecord.get_base r
                  | _ -> false)
              non_free_fresh
          then (Type.PolyRecord r, map)
          else
            let args, map =
              List.fold_left
                ~f:(fun (acc, map) (field, t) ->
                  let t, map = inner t map in
                  ((field, t) :: acc, map) )
                ~init:([], map) (PolyRecord.get_base r).fields
            in
            (PolyRecord {fields= args; poly= ref None}, map)
      | SelfRef {name; args} ->
          let args, map =
            List.fold_left
              ~f:(fun (acc, map) el ->
                let t, map = inner el map in
                (t :: acc, map) )
              ~init:([], map) args
          in
          (SelfRef {name; args}, map)
      | Adt t ->
          let args, map =
            List.fold_left
              ~f:(fun (acc, map) el ->
                let t, map = inner el map in
                (t :: acc, map) )
              ~init:([], map) t.Adt.args
          in
          let variants, map =
            List.fold_left
              ~f:(fun (acc, map) el ->
                let args, map =
                  List.fold_left
                    ~f:(fun (acc, map) el ->
                      let t, map = inner el map in
                      (t :: acc, map) )
                    ~init:([], map) el.Variant.args
                in
                ({el with args= List.rev args} :: acc, map) )
              ~init:([], map) t.Adt.variants
          in
          ( Adt
              {t with Adt.args= List.rev args; Adt.variants= List.rev variants}
          , map )
      | default -> (default, map)
    in
    let t, map = inner t type_map in
    t
end

module rec Lambda : sig
  type t = {arg: string; body: Node.t} [@@deriving sexp]
end = struct
  type t = {arg: string; body: Node.t} [@@deriving sexp]
end

and Apply : sig
  type t = {expr: Node.t; arg: Node.t} [@@deriving sexp]
end = struct
  type t = {expr: Node.t; arg: Node.t} [@@deriving sexp]
end

and Call : sig
  type t = {module_: string; fun_: string; args: Node.t list} [@@deriving sexp]
end = struct
  type t = {module_: string; fun_: string; args: Node.t list} [@@deriving sexp]
end

and Let : sig
  type t = {name: string; bound_expr: Node.t; expr: Node.t} [@@deriving sexp]
end = struct
  type t = {name: string; bound_expr: Node.t; expr: Node.t} [@@deriving sexp]
end

and AdtConstructor : sig
  type t = {type_: string; name: string; args: Node.t list} [@@deriving sexp]
end = struct
  type t = {type_: string; name: string; args: Node.t list} [@@deriving sexp]
end

and Literal : sig
  type t =
    | Int of int
    | Float of float
    | String of string
    | Boolean of bool
    | Tuple of Node.t list
    | Record of (string * Node.t) list
    | ConsCell of (Node.t * Node.t)
    | EmptyList
  [@@deriving sexp]
end = struct
  type t =
    | Int of int
    | Float of float
    | String of string
    | Boolean of bool
    | Tuple of Node.t list
    | Record of (string * Node.t) list
    | ConsCell of (Node.t * Node.t)
    | EmptyList
  [@@deriving sexp]
end

and Match : sig
  type t = {expr: Node.t; clauses: MatchClause.t list} [@@deriving sexp]
end = struct
  type t = {expr: Node.t; clauses: MatchClause.t list} [@@deriving sexp]
end

and MatchClause : sig
  type t = {pattern: MatchPattern.t; result: Node.t} [@@deriving sexp]
end = struct
  type t = {pattern: MatchPattern.t; result: Node.t} [@@deriving sexp]
end

and MatchPattern : sig
  type t =
    | Int of int
    | Float of float
    | String of string
    | Boolean of bool
    | Tuple of t list
    | Cons of (t * t)
    | EmptyList
    | Binding of string
    | Record of (string * t) list
    | Constructor of (string * t list)
  [@@deriving sexp]

  val cons_to_list : t -> t list
end = struct
  type t =
    | Int of int
    | Float of float
    | String of string
    | Boolean of bool
    | Tuple of t list
    | Cons of (t * t)
    | EmptyList
    | Binding of string
    | Record of (string * t) list
    | Constructor of (string * t list)
  [@@deriving sexp]

  let rec cons_to_list x =
    match x with
    | EmptyList -> [EmptyList]
    | Binding b -> [x]
    | Cons (pat, tail) -> pat :: cons_to_list tail
    | _ -> failwith "Bad match pattern in tail position"
end

and RecordAccess : sig
  type t = Node.t * string [@@deriving sexp]
end = struct
  type t = Node.t * string [@@deriving sexp]
end

and RecordUpdate : sig
  type t = Node.t * (string * Node.t) list [@@deriving sexp]
end = struct
  type t = Node.t * (string * Node.t) list [@@deriving sexp]
end

and Expr : sig
  type t =
    | Identifier of Identifier.t
    | Lambda of Lambda.t
    | Literal of Literal.t
    | Apply of Apply.t
    | Call of Call.t
    | Let of Let.t
    | Match of Match.t
    | RecordAccess of RecordAccess.t
    | RecordUpdate of RecordUpdate.t
    | AdtConstructor of AdtConstructor.t
    | Unit
  [@@deriving sexp]

  val format : Node.t -> string
end = struct
  type t =
    | Identifier of Identifier.t
    | Lambda of Lambda.t
    | Literal of Literal.t
    | Apply of Apply.t
    | Call of Call.t
    | Let of Let.t
    | Match of Match.t
    | RecordAccess of RecordAccess.t
    | RecordUpdate of RecordUpdate.t
    | AdtConstructor of AdtConstructor.t
    | Unit
  [@@deriving sexp]

  let format e =
    let rec formati indent e =
      match e.Node.expr with
      | Identifier {name; type_} -> name
      | Lambda {Lambda.arg; body} ->
          "(\\" ^ arg ^ " -> " ^ formati indent body ^ ")"
      | Apply {Apply.expr; arg} ->
          "(" ^ formati indent expr ^ " " ^ formati indent arg ^ ")"
      | Literal (Literal.Boolean b) -> string_of_bool b
      | Literal (Literal.Int i) -> string_of_int i
      | Literal (Literal.Float f) -> string_of_float f
      | Literal (Literal.String s) -> "<<" ^ s ^ ">>"
      | Literal (Literal.Tuple items) ->
          let parts = List.map ~f:(formati indent) items in
          sprintf "(%s)" (String.concat parts ~sep:", ")
      | Call {Call.module_; fun_; args} -> sprintf "%s:%s" module_ fun_
      | Let {Let.name; bound_expr; expr} ->
          sprintf "let %s = %s in %s" name
            (formati indent bound_expr)
            (formati indent expr)
      | Match {Match.expr; clauses} ->
          sprintf "match %s with ???" (formati indent expr)
      | AdtConstructor {type_; name; args} ->
          let arg_vals =
            List.map ~f:(formati indent) args |> String.concat ~sep:", "
          in
          sprintf "%s.%s %s" type_ name arg_vals
      | _ -> "unknown"
    in
    formati 0 e
end

and Node : sig
  type t = {expr: Expr.t; source: Source.t} [@@deriving sexp]

  val make : Expr.t -> Source.t -> t
end = struct
  type t = {expr: Expr.t; source: Source.t} [@@deriving sexp]

  let make expr source = {expr; source}
end

module Error = struct
  type error_type =
    | Syntax of string
    | TypeMismatch of (Type.t * Type.t)
    | TypeSignatureMismatch of (Type.t * Type.t)
    | IdentifierNotFound of string
    | TypeNotFound of string
    | Inexhaustive of string
    | TypeParamMissing of string
    | CyclicalType of string
    | FieldMismatch of string

  type t = {source: Source.t; error: error_type}

  let format_src (src: Source.t) : string =
    match src with
    | None -> "(no pos)"
    | Pos pos -> sprintf "%d:%d" pos.line pos.char

  let format_error err =
    let src = format_src err.source in
    match err.error with
    | Syntax e -> src ^ ": Syntax error: " ^ e
    | IdentifierNotFound t -> src ^ ":Unknown identifier: '" ^ t ^ "'"
    | TypeMismatch (t1, t2) ->
        sprintf "%s: Type mismatch: expected '%s' but found '%s'" src
          (Type.format t1) (Type.format t2)
    | Inexhaustive s -> sprintf "%s: Inexhaustive pattern match %s" src s
    | TypeParamMissing s ->
        sprintf "%s: Type parameter '%s' used in definition but not listed" src
          s
    | CyclicalType s -> sprintf "%s: Cyclical type error - %s" src s
    | FieldMismatch s -> sprintf "%s: Field mismatch error - %s" src s
    | TypeNotFound f -> sprintf "%s: Type not found - %s" src f
    | TypeSignatureMismatch (t1, t2) ->
        sprintf
          "%s: Type signature mismatch: expected '%s' but type signature \
           given of '%s'"
          src (Type.format t1) (Type.format t2)
end

exception CompileError of Error.t

module TypedNode = struct
  type t = {node: Node.t; type_: Type.t} [@@deriving sexp]

  let make node type_ = {node; type_}
end

module Binding = struct
  type t = {name: string; expr: TypedNode.t} [@@deriving sexp]

  let make name expr = {name; expr}
end

module NamedType = struct
  type t = {name: string; type_: Type.t} [@@deriving sexp]
end

module Import = struct
  type t = {module_: string; qualified: string list} [@@deriving sexp]
end

module Module = struct
  type t =
    { name: string
    ; imports: Import.t list
    ; bindings: Binding.t list
    ; verbatim: string
    ; types: NamedType.t list }
  [@@deriving sexp]

  let empty =
    {name= "__empty__"; imports= []; bindings= []; types= []; verbatim= ""}

  let rec find_constructor types variant_name : (Adt.t * Variant.t) option =
    match types with
    | [] -> None
    | {NamedType.type_} :: rest ->
      match type_ with
      | Type.Adt adt -> (
        try
          let variant =
            List.find_exn
              ~f:(fun {Variant.name} -> name = variant_name)
              adt.variants
          in
          Some (adt, variant)
        with Not_found -> find_constructor rest variant_name )
      | _ -> find_constructor rest variant_name
end

module Statement = struct
  type t =
    | Let of {name: string; expr: Node.t}
    | TypeSignature of {name: string; type_: Type.t}
    | Module of {name: string}
    | Import of Import.t
    | Type of {name: string; type_: Type.t}
    | Verbatim of {name: string; code: string}
  [@@deriving sexp]

  let format = function
    | Let {name; expr} -> sprintf "let %s = %s" name (Expr.format expr)
    | TypeSignature {name; type_} ->
        sprintf "val %s : %s" name (Type.format type_)
    | Module {name} -> sprintf "module %s" name
    | Type {name; type_} -> sprintf "type %s" name
    | Import {module_} -> sprintf "import %s" module_
    | Verbatim {name} -> sprintf "[%% %s verbatim code block %%]" name

  let rec make_variant argc name type_ v =
    match v with
    | [] ->
        let rec make_args i =
          match i with
          | 0 -> []
          | x -> sprintf "arg%d" (x - 1) :: make_args (x - 1)
        in
        let args =
          make_args argc |> List.rev
          |> List.map ~f:(fun a ->
                 Expr.Identifier
                   { Identifier.name= a
                   ; type_= ref IdentType.Var
                   ; module_= ref None } )
          |> List.map ~f:(fun expr -> Node.make expr Source.None)
        in
        Node.make (Expr.AdtConstructor {type_; name; args}) Source.None
    | arg :: rest ->
        let arg = sprintf "arg%d" argc in
        let body = make_variant (argc + 1) name type_ rest in
        let expr = Expr.Lambda {arg; body} in
        Node.make expr Source.None

  let post_process (statement: t) =
    match statement with
    | Type {name; type_} -> (
        let type_name = name in
        match type_ with
        | Type.Adt {variants} ->
            let adt_funs =
              variants
              |> List.map ~f:(fun {Variant.args; name} ->
                     let expr = make_variant 0 name type_name args in
                     Let {name; expr} )
            in
            statement :: adt_funs
        | _ -> [statement] )
    | _ -> [statement]
end

module TestExpressions = struct
  let test_node expr = Node.make expr Source.None

  let lambda arg body = test_node @@ Expr.Lambda {Lambda.arg; body}

  let var f =
    test_node @@ Expr.Identifier (Identifier.make f (ref IdentType.Var))

  let lit_int i = test_node @@ Expr.Literal (Literal.Int i)

  let lit_bool b = test_node @@ Expr.Literal (Literal.Boolean b)

  let apply expr arg = test_node @@ Expr.Apply {Apply.expr; arg}

  let call module_ fun_ args = test_node @@ Expr.Call {Call.module_; fun_; args}
end
