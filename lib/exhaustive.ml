open Ast.MatchPattern
open Ast

type node = {value: string; child: tree; sibling: tree; span: int; args: int}

and tree = Empty | Node of node

type path_element = Child of string | Sibling of string

let rec debug_path path =
  match path with
  | h :: t -> (
    match h with
    | Child s -> "Child " ^ s ^ " -> " ^ debug_path t
    | Sibling s -> "Sibling " ^ s ^ " -> " ^ debug_path t )
  | [] -> "ø"

type path = path_element list

let debug_pat pat : string =
  let rec inner pat level =
    match pat with
    | Empty -> "ø"
    | Node {value; child; sibling; span; args} ->
        value ^ " -> "
        ^ inner sibling (level + String.length value + 4)
        ^ "\n" ^ String.make level ' ' ^ "|- "
        ^ inner child (level + 3)
  in
  inner pat 0

let andThen f node = match node with Node n -> f n | Empty -> Empty

let rec get_in_tree path tree =
  match path with
  | [] -> tree
  | el :: rest ->
    match el with
    | Child v ->
        tree
        |> andThen (fun n ->
               n.child
               |> andThen (fun child ->
                      if v = n.value || n.value = "Binding" then
                        get_in_tree rest (Node child)
                      else Empty ) )
    | Sibling v ->
        tree
        |> andThen (fun n ->
               n.sibling
               |> andThen (fun sibling ->
                      if v = n.value || n.value = "Binding" then
                        get_in_tree rest (Node sibling)
                      else Empty ) )

let rec isExhaustive pat pats path noSpan =
  match pat with
  | Empty -> false
  | Node {value; child; sibling; span; args} ->
      let childrenExhaustive =
        match args > 0 with
        | true -> isExhaustive child pats (path @ [Child value]) false
        | false -> true
      in
      let siblingExhaustive =
        match sibling with
        | Empty -> true
        | Node n -> isExhaustive sibling pats (path @ [Sibling value]) false
      in
      let spanExhaustive =
        match (span > 0, noSpan) with
        (* TODO - if any of the 'spans' are a capture, we've captured everything *)
        (* TODO - check this isn't short cutting sibling checks - shouldn't be tho *)
        | true, false ->
            (*  Get all variants that match the same path *)
            let variantsAll = List.map (get_in_tree path) pats in
            let variants =
              variantsAll
              |> List.filter (fun x -> isExhaustive x pats path true)
            in
            let values =
              List.map
                (fun x -> match x with Empty -> "" | Node n -> n.value)
                variants
              |> List.filter (( != ) "")
              |> List.sort_uniq String.compare
            in
            let any_bindings = List.exists (( = ) "Binding") values in
            (*
                        print_endline (String.concat ", " values);
                        print_endline ("Any bindings?" ^ (string_of_bool any_bindings));
                        print_endline (debug_path path); *)
            List.length values = span || any_bindings
        | _ -> true
      in
      spanExhaustive && siblingExhaustive && childrenExhaustive

let rec make_tuple_pat types els =
  match els with
  | [] -> Empty
  | h :: t -> Node {(build_tree types h) with sibling= make_tuple_pat types t}

and make_record_pat types els =
  match els with
  | [] -> Empty
  | (name, h) :: t ->
      (* TODO - I'm pretty sure this strategy is bogus *)
      let child = build_tree types h in
      let child =
        { child with
          sibling= make_record_pat types t; value= name ^ "=" ^ child.value }
      in
      Node child

and build_tree types pat =
  match pat with
  | Int i ->
      { value= "Int " ^ string_of_int i
      ; args= 0
      ; span= max_int
      ; child= Empty
      ; sibling= Empty }
  | Float f ->
      { value= "Float " ^ string_of_float f
      ; args= 0
      ; span= max_int
      ; child= Empty
      ; sibling= Empty }
  | String s ->
      { value= "String " ^ s
      ; args= 0
      ; span= max_int
      ; child= Empty
      ; sibling= Empty }
  | Binding _ ->
      {value= "Binding"; args= 0; span= 0; child= Empty; sibling= Empty}
  | Boolean b ->
      { value= "Bool " ^ string_of_bool b
      ; args= 0
      ; span= 2
      ; child= Empty
      ; sibling= Empty }
  | Tuple t ->
      { value= "Tuple"
      ; args= List.length t
      ; span= 0
      ; sibling= Empty
      ; child= make_tuple_pat types t }
  | EmptyList ->
      {value= "EmptyList"; args= 0; span= 2; child= Empty; sibling= Empty}
  | Record items ->
      { value= "Record"
      ; span= 0
      ; args= List.length items
      ; sibling= Empty
      ; child= make_record_pat types items }
  | Cons (h, t) ->
      let value = build_tree types h in
      { value= "Cons"
      ; args= 2
      ; span= 2
      ; sibling= Empty
      ; child= Node {value with sibling= Node (build_tree types t)} }
  | Constructor (name, args) ->
      (* We need to find both the type and the particular variant:
       - the type so we know how many variants (SPAN);
       - the variant so we know how many args it takes (ARGS) *)
      let adt, variant =
        match Module.find_constructor types name with
        | Some res -> res
        | None -> failwith "Bad constructor during exhaustive check"
      in
      let argc, span =
        (List.length adt.variants, List.length variant.Variant.args)
      in
      { value= "Constructor " ^ name
      ; span
      ; args= argc
      ; sibling= Empty
      ; child= make_tuple_pat types args }

let check_match types m =
  let tree = List.map (fun x -> Node (build_tree types x)) m in
  (* let _ = tree |> List.iter (fun t -> debug_pat t |> print_endline; print_newline ();) in *)
  let head = List.hd tree in
  isExhaustive head tree [] false

let rec find_match {Ast.Node.expr; source} (types: NamedType.t list) =
  let option_or x y = match x with Some thing -> Some thing | None -> y in
  match expr with
  | Ast.Expr.Let {bound_expr; expr} ->
      find_match bound_expr types |> option_or (find_match expr types)
  | Ast.Expr.Match {clauses} ->
      if
        check_match types
          (List.map (function {Ast.MatchClause.pattern} -> pattern) clauses)
        = false
      then
        Some
          {Ast.Error.error= Ast.Error.Inexhaustive "Missing patterns"; source}
      else None
  | Ast.Expr.Lambda {body} -> find_match body types
  | _ -> None

let rec check_bindings (bindings: Ast.Binding.t list)
    (types: Ast.NamedType.t list) =
  match bindings with
  | [] -> None
  | h :: t ->
    match find_match h.expr.node types with Some err -> Some err | _ -> None

let check_module (mod_: Ast.Module.t) =
  match check_bindings mod_.bindings mod_.types with
  | Some err -> Error err
  | None -> Ok mod_
