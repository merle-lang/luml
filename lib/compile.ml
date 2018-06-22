open Ast
open Printf

let ( <| ) f x = f x

let sym_count = ref 1

let gen_sym () =
  sym_count := !sym_count + 1 ;
  sprintf "sym_%d" !sym_count

let map_infix_char c =
  match c with
  | '(' -> "LP"
  | ')' -> "RP"
  | '>' -> "GT"
  | '<' -> "LT"
  | '|' -> "BR"
  | '+' -> "PL"
  | '-' -> "MI"
  | '=' -> "EQ"
  | ':' -> "CO"
  | '/' -> "FS"
  | '*' -> "AX"
  | '.' -> "DT"
  | '&' -> "AM"
  | a -> failwith ("Non-infixable char" ^ string_of_int (int_of_char c))

let sub_infix infix =
  match String.sub infix 0 1 with
  | "(" ->
      let rec to_list c =
        match c with
        | "" -> []
        | c -> c.[0] :: to_list (String.sub c 1 (String.length c - 1))
      in
      infix |> to_list |> List.map map_infix_char |> String.concat "_"
  | _ -> infix

let rec make_literal = function
  | Literal.Int i -> string_of_int i
  | Literal.Float f -> string_of_float f
  | Literal.Boolean true -> "true"
  | Literal.Boolean false -> "false"
  | Literal.String s -> "\"" ^ s ^ "\"" (* TODO - quote this *)
  | Literal.Tuple t -> tuple t
  | Literal.ConsCell (this, rest) ->
      let this_val = make_expr this in
      let rest_val =
        match rest.expr with
        | Expr.Literal Literal.EmptyList -> "{}"
        | _ -> make_expr rest
      in
      sprintf "{ value = %s, next = %s }" this_val rest_val
  | Literal.EmptyList -> "{}"
  | Literal.Record fields ->
      let fstring =
        List.map
          (fun (name, expr) -> sprintf "%s = %s" name (make_expr expr))
          fields
        |> String.concat ", "
      in
      "{" ^ fstring ^ ", _type='record'}"

and make_lambda {Lambda.arg; body} =
  sprintf "function (%s) return (%s) end\n" arg (make_expr body)

and tuple t =
  let parts = List.map make_expr t in
  "{" ^ String.concat ", " parts ^ ", _type='tuple'}"

and make_binop op args =
  let exprs = List.map make_expr args in
  match exprs with
  | [] -> failwith "Cannot call a binop with less than two args!"
  | [_one] -> failwith "Cannot call a binop with less than two args!"
  | initial :: rest ->
      sprintf "((%s) %s (%s))" initial op (String.concat "," rest)

and make_call {Call.module_; fun_; args} =
  match (module_, fun_) with
  | "lua", "+" -> make_binop "+" args
  | "lua", "-" -> make_binop "-" args
  | "lua", "*" -> make_binop "*" args
  | "lua", "/" -> make_binop "/" args
  | "lua", "+." -> make_binop "+" args
  | "lua", "==" -> make_binop "==" args
  | "lua", "~=" -> make_binop "~=" args
  | "lua", ">" -> make_binop ">" args
  | "lua", "<" -> make_binop "<" args
  | "lua", "%" -> make_binop "%" args
  | "lua", "and" -> make_binop "and" args
  | "lua", "or" -> make_binop "or" args
  | "lua", _ ->
      List.map make_expr args |> String.concat "," |> sprintf "%s(%s)" fun_
  | _ ->
      List.map make_expr args |> String.concat ","
      |> sprintf "%s.%s(%s)" module_ fun_

and make_apply {Apply.expr; arg} =
  sprintf "(%s)(%s)" (make_expr expr) (make_expr arg)

and make_let {Let.name; bound_expr; expr} =
  let bexpr_var = make_expr bound_expr in
  let expr_var = make_expr expr in
  sprintf
    "((function () local %s = (%s) local __result__ = (%s) return __result__ \
     end)())"
    (sub_infix name) bexpr_var expr_var

and make_match {Match.expr; clauses} =
  let expr_str = make_expr expr in
  let name = gen_sym () in
  let patterns = List.map (make_clauses name) clauses in
  (* wrap in a function so we can short circuit with `return` *)
  sprintf "(function (%s) \n\t%s \n\tend\n)(%s)" name
    (String.concat "\n\t" patterns)
    expr_str

and make_clauses name {MatchClause.pattern; MatchClause.result} =
  let skip_ident = gen_sym () in
  let rec make_checks name pat =
    match pat with
    | MatchPattern.Boolean b ->
        ([], [sprintf "(%s == %s)" name (string_of_bool b)])
    | MatchPattern.Int v -> ([], [sprintf "(%s == %s)" name (string_of_int v)])
    | MatchPattern.Float f ->
        ([], [sprintf "(%s == %s)" name (string_of_float f)])
    | MatchPattern.String s ->
        (* TODO - need to escape strings *)
        ([], [sprintf "(%s == \"%s\")" name s])
    | MatchPattern.Tuple t_items ->
        List.mapi
          (fun index pat ->
            let new_name = gen_sym () in
            (* Lua indexes from 1, not 0... *)
            let binding =
              sprintf "local %s = %s[%s]" new_name name
                (string_of_int (index + 1))
            in
            let bindings, checks = make_checks new_name pat in
            (bindings @ [binding], checks) )
          t_items
        |> List.fold_left (fun (a, b) (x, y) -> (x @ a, b @ y)) ([], [])
    | MatchPattern.Binding b ->
        let binding = sprintf "local %s = %s" b name in
        ([binding], ["true"])
    | MatchPattern.Cons (value, next) ->
        let val_name = gen_sym () in
        let val_binding =
          sprintf "if %s.value == nil then goto %s end\n\tlocal %s = %s.value"
            name skip_ident val_name name
        in
        let next_name = gen_sym () in
        let next_binding = sprintf "local %s = %s.next" next_name name in
        let bindings1, checks1 = make_checks val_name value in
        let bindings2, checks2 = make_checks next_name next in
        (bindings1 @ bindings2 @ [next_binding; val_binding], checks1 @ checks2)
    | MatchPattern.EmptyList -> ([], [sprintf "%s.value == nil" name])
    | MatchPattern.Record items ->
        List.map
          (fun (field, pat) ->
            let new_name = gen_sym () in
            let binding = sprintf "local %s = (%s).%s" new_name name field in
            let bindings, checks = make_checks new_name pat in
            (bindings @ [binding], checks) )
          items
        |> List.fold_left (fun (a, b) (x, y) -> (x @ a, b @ y)) ([], [])
    | MatchPattern.Constructor (cname, args) ->
        let args = MatchPattern.String cname :: args in
        (* These are very similar to tuples, but with an initial string argument 
         with the name of the ADT *)
        List.mapi
          (fun index pat ->
            let new_name = gen_sym () in
            (* Lua indexes from 1, not 0... *)
            let binding =
              sprintf "local %s = %s[%s]" new_name name
                (string_of_int (index + 1))
            in
            let bindings, checks = make_checks new_name pat in
            (bindings @ [binding], checks) )
          args
        |> List.fold_left (fun (a, b) (x, y) -> (x @ a, b @ y)) ([], [])
  in
  let bindings, checks = make_checks name pattern in
  let binding_str = String.concat " " (List.rev bindings) in
  let check_str = String.concat " and " checks in
  let res = make_expr result in
  sprintf "do %s\n\tif %s then return %s end end ::%s::" binding_str check_str
    res skip_ident

and make_adtcons {AdtConstructor.type_; name; args} =
  (* TODO - we should definitely be qualifying it like this *)
  (* let type_string = sprintf "\"%s.%s\"" type_ name in *)
  let type_string = sprintf "\"%s\"" name in
  let arg_vals = List.map make_expr args in
  let inner =
    "_type=\"adt\"" :: type_string :: arg_vals
    |> List.filter (fun x -> x <> "")
    |> String.concat ", "
  in
  "{" ^ inner ^ "}"

and make_recordaccess (expr, field) =
  let expr_string = make_expr expr in
  sprintf "(%s).%s" expr_string field

and make_recordupdate (expr, fields) =
  let expr_string = make_expr expr in
  (* TODO - we shouldn't bother copying the old fields *)
  let copy_string =
    sprintf
      "\n  \
       local new_table = {}\n  \
       for k, v in pairs(%s) do\n    \
       new_table[k] = v\n  \
       end\n  \
       "
      expr_string
  in
  let new_fields =
    List.map
      (fun (f, e) ->
        let new_field_expr = make_expr e in
        sprintf "new_table.%s = %s" f new_field_expr )
      fields
    |> String.concat "\n"
  in
  sprintf "((function() %s %s return new_table end)())" copy_string new_fields

and make_expr (node: Node.t) =
  match node.expr with
  | Literal l -> make_literal l
  | Lambda l -> make_lambda l
  | Call c -> make_call c
  | Identifier {module_= {contents= Some m}; name} -> m ^ "." ^ sub_infix name
  | Identifier {name} -> sub_infix name
  | Apply a -> make_apply a
  | Let l -> make_let l
  | Match m -> make_match m
  | AdtConstructor c -> make_adtcons c
  | RecordAccess a -> make_recordaccess a
  | RecordUpdate u -> make_recordupdate u
  | Unit -> "nil"

let real_type t =
  match t with
  | Ast.Type.TypeArg arg -> (
    match !(arg.instance) with Some something -> something | _ -> t )
  | _ -> t

let make_fun_version name (version: TypedNode.t) =
  let name = sub_infix name in
  sprintf "local %s %s = %s\n" name name (make_expr version.node)

let get_binding_arity t = match real_type t with Type.Arrow _ -> 1 | _ -> 0

let make_export (binding: Binding.t) =
  sprintf "%s = %s" (sub_infix binding.name) (sub_infix binding.name)

let make_binding (binding: Binding.t) =
  make_fun_version binding.name binding.expr

let make_module (module_: Module.t) =
  let bindings = List.map make_binding module_.bindings |> List.rev in
  let exports = List.map make_export module_.bindings in
  let binding_list = String.concat "\n  " bindings in
  let export_list = String.concat "," exports in
  sprintf "local %s = (function() \n %s \n %s\n  return {%s} end)()"
    module_.name module_.verbatim binding_list export_list
