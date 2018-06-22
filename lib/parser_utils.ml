open Core_kernel
open Ast


(* the failure cases for both these funs should normally be impossible *)
let rec curry_function args expr source =
  match args with
  | [arg] -> Node.make (Expr.Lambda {arg; body= expr}) source
  | arg :: rest ->
      Node.make
        (Expr.Lambda {arg; body= curry_function rest expr source})
        source
  | [] -> raise (failwith "Attempted to curry a zero-arg fun")

let rec make_apply expr args src =
  match args with
  | [arg] -> Node.make (Expr.Apply {expr; arg}) src
  | arg :: rest ->
      Node.make (Expr.Apply {expr= make_apply expr rest src; arg}) src
  | [] -> raise (failwith "Attempted to apply with zero args")

let make_call callref args position =
  match callref |> String.split_on_chars ~on: ['.'] |> List.rev with
  | fun_ :: module_parts ->
      let module_ = String.concat ~sep: "." module_parts in
      Node.make (Expr.Call {module_; fun_; args}) position
  | _ -> failwith "Bad call expr"

let make_fun_type args =
  let module TArgMap = Map.Make (String) in
  let check_arg arg targ_map =
    match arg with
    | Type.TypeArg {TypeArg.name} -> (
        let name = match name with Some v -> v | None -> "" in
        match TArgMap.mem targ_map name with
        | true -> (TArgMap.find_exn targ_map name, targ_map)
        | false -> (arg, TArgMap.set targ_map name arg) )
    | _ -> (arg, targ_map)
  in
  let rec inner args targ_map =
    (* normalise all type args *)
    match args with
    | [arg] ->
        let arg, _ = check_arg arg targ_map in
        arg
    | arg :: rest ->
        let arg, targ_map = check_arg arg targ_map in
        Type.Arrow (arg, inner rest targ_map)
    | [] -> raise (failwith "Attempted to create funtype with 0 arity")
  in
  inner args TArgMap.empty

let rec make_cons_literal items position =
  match items with
  | [] -> Node.make (Expr.Literal Literal.EmptyList) position
  | item :: rest ->
      Node.make
        (Expr.Literal
           (Literal.ConsCell (item, make_cons_literal rest position)))
        position

let rec make_match_cons_literal items =
  match items with
  | [] -> MatchPattern.EmptyList
  | item :: rest -> MatchPattern.Cons (item, make_match_cons_literal rest)

(* Switch out any typeargs defined for the type *)
let rec match_up_typeargs (args: Type.t list) (type_: Type.t) =
  match type_ with
  | Adt adt ->
      (* First let's match against any typeargs in the list *)
      let adt_args = List.map ~f: (match_up_typeargs args) adt.args in
      (* Now let's match up against all variants *)
      let variants =
        List.map
          ~f: (fun variant ->
            let v_args =
              List.map ~f: (match_up_typeargs args) variant.Variant.args
            in
            {variant with args= v_args} )
          adt.variants
      in
      Type.Adt {adt with args= adt_args; variants}
  | SelfRef {name; args} ->
      let args = List.map ~f: (match_up_typeargs args) args in
      Type.SelfRef {name; args}
  | Tuple parts -> Type.Tuple (List.map ~f: (match_up_typeargs args) parts)
  | UserType user_type ->
      let user_type_args = List.map ~f: (match_up_typeargs args) user_type.args in
      UserType {user_type with args= user_type_args}
  | Record record ->
      Type.Record
        { fields=
            List.map
              ~f: (fun (name, f) -> (name, match_up_typeargs args f))
              record.fields }
  | PolyRecord record ->
      Type.PolyRecord
        { fields=
            List.map
              ~f: (fun (name, f) -> (name, match_up_typeargs args f))
              record.fields
        ; poly= record.poly }
  | List t -> Type.List (match_up_typeargs args t)
  | Type.TypeArg arg -> (
    try
      List.find_exn
        ~f: (function
            | Type.TypeArg targ -> targ.TypeArg.name = arg.TypeArg.name
            | _ -> failwith "Bad typearg type")
        args
    with Not_found ->
      raise
        (CompileError
           { Ast.Error.source= None
           ; error=
               Ast.Error.TypeParamMissing
                 (match arg.name with Some n -> n | _ -> "?") }) )
  | Type.Arrow (arg, expr) ->
      let arg = match_up_typeargs args arg in
      let expr = match_up_typeargs args expr in
      Type.Arrow (arg, expr)
  | other -> other

let gen_ident pos =
  let alphanum =  "abcdefghijklmnopqrstuvwxyz" in
  let len = String.length alphanum in
  let name = List.range 0 12 
             |> List.map ~f: (fun _ -> String.get alphanum (Random.int len)) 
             |> String.of_char_list in
  (name, Node.make (Expr.Identifier (Identifier.make name (ref IdentType.Unknown))) pos)

let pos p =
  Ast.Source.Pos
    { Ast.Pos.file= p.Lexing.pos_fname
    ; char= p.pos_cnum - p.pos_bol
    ; line= p.pos_lnum }
