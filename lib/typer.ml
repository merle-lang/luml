open Core_kernel
open Ast

let rec get_instance_type t =
  match t with
  | Type.TypeArg {TypeArg.instance} -> (
    match !instance with Some inst -> get_instance_type inst | _ -> t )
  | _ -> t

module Env = struct
  type t =
    { symbols: (int * Type.t) String.Map.t
    ; signatures: (string * Type.t) list
    ; modules: (string * Module.t) list
    ; qualified_imports: (string * string) list
    ; types: Ast.NamedType.t list
    ; non_free: Type.t list
    ; type_refs: string list
    ; argi: int }

  (* construct and return a new empty type arg *)
  let new_arg env name level =
    let arg = Type.TypeArg (TypeArg.make ()) in
    let symbols = String.Map.set env.symbols name (level, arg) in
    ({env with argi= env.argi + 1; symbols}, arg)

  (* add a symbol directly into the env *)
  let add name binding_type env =
    {env with symbols= String.Map.set env.symbols name binding_type}

  let add_non_free t env =
    {env with non_free= get_instance_type t :: env.non_free}

  let get_mod_type module_ name node env =
    try
      let _, mod_ = List.find_exn ~f:(fun (n, _) -> n = module_) env.modules in
      let (binding : Ast.Binding.t) =
        List.find_exn
          ~f:(fun (b: Ast.Binding.t) -> b.name = name)
          mod_.bindings
      in
      (0, Type.copy binding.expr.type_ env.non_free)
    with Not_found ->
      raise
        (CompileError
           { Error.error= IdentifierNotFound (module_ ^ "." ^ name)
           ; source= node.Node.source })

  (* TODO - get_type and get_user_type are really confusing *)
  let get_user_type (n: string) (env: t) =
    try List.find_exn ~f:(fun {Ast.NamedType.name} -> n = name) env.types
    with Not_found ->
      (* Qualified import? *)
      try
        let mod_name, type_name =
          List.find_exn ~f:(fun (_, name) -> n = name) env.qualified_imports
        in
        let _, mod_ =
          List.find_exn ~f:(fun (n, _) -> n = mod_name) env.modules
        in
        (* Now we know that this symbol really belongs to an external module,
      mutation the module reference on this identifier *)
        List.find_exn
          ~f:(fun {Ast.NamedType.name} -> name = type_name)
          mod_.Module.types
      with Not_found ->
        raise
          (CompileError {Error.error= TypeNotFound n; Error.source= Source.None})

  let get_type ident node env =
    try
      let level, type_ = String.Map.find_exn env.symbols ident in
      (level, Type.copy type_ env.non_free)
    with Not_found ->
      raise
        (CompileError
           { Error.error= IdentifierNotFound ident
           ; Error.source= node.Node.source })

  let empty =
    { argi= 0
    ; signatures= []
    ; symbols= String.Map.empty
    ; non_free= []
    ; types= []
    ; modules= []
    ; qualified_imports= []
    ; type_refs= [] }
end

let rec occurs_in (ta: TypeArg.t) (t: Type.t) =
  let t = get_instance_type t in
  match t with
  | TypeArg t -> t = ta
  | Tuple args -> List.exists ~f:(occurs_in ta) args
  | List t -> occurs_in ta t
  | _ -> false

let rec unify ?source:((source: Source.t) = None) t1 t2 =
  let it1 = get_instance_type t1 in
  let it2 = get_instance_type t2 in
  match (it1, it2) with
  | Type.TypeArg a1, a2 ->
      if it1 != it2 && occurs_in a1 a2 then
        raise
          (CompileError
             {Error.error= Error.CyclicalType "recursive type detected"; source}) ;
      ( match it2 == it1 with
      | false -> a1.TypeArg.instance := Some it2
      | true -> () ) ;
      it2
  | _, Type.TypeArg _ -> unify t2 t1 ~source
  | Type.Tuple items1, Type.Tuple items2 ->
      Type.Tuple
        ( List.zip_exn items1 items2
        |> List.map ~f:(fun (i1, i2) -> unify i1 i2 ~source) )
  | Type.Arrow (a1a, a1r), Type.Arrow (a2a, a2r) ->
      let a3a = unify a1a a2a ~source in
      let a3r = unify a1r a2r ~source in
      let result = Type.Arrow (a3a, a3r) in
      result
  | Type.List l1, Type.List l2 ->
      let l3 = unify l1 l2 ~source in
      Type.List l3
  | Type.Adt a1, Type.Adt a2 ->
      (* Check name *)
      if a1.name <> a2.name then
        raise (CompileError {error= Error.TypeMismatch (t1, t2); source}) ;
      (* Check type args *)
      if List.length a1.args != List.length a2.args then
        raise (CompileError {error= Error.TypeMismatch (t1, t2); source}) ;
      let args =
        List.zip_exn a1.args a2.args
        |> List.map ~f:(fun (i1, i2) -> unify i1 i2 ~source)
      in
      Type.Adt {name= a1.name; args; variants= a1.variants}
  | Type.SelfRef ref, Type.Adt adt ->
      (* Very naive check! We possibly need a way of checking that the types really do match,
       * without falling down the infinite recursion hole *)
      (* We do, at least, check any supplied arguments *)
      let args =
        List.zip_exn ref.args adt.args
        |> List.map ~f:(fun (i1, i2) -> unify i1 i2 ~source)
      in
      if ref.name <> adt.name then
        raise (CompileError {error= Error.TypeMismatch (t1, t2); source})
      else Type.Adt {adt with args}
  | Type.Adt adt, Type.SelfRef ref -> unify t2 t1 ~source
  | Type.PolyRecord r1, Type.PolyRecord r2 ->
      (* Unify two poly records - this means finding the base of each and creating a new base linked to it *)
      (* TODO - check types and ensure only one of each type! *)
      let b1, b2 = (PolyRecord.get_base r1, PolyRecord.get_base r2) in
      (* Unify all of r1 on r2 *)
      (* Unify all of r2 on r1 *)
      let new_base_fields =
        List.fold_left
          ~f:(fun acc (name, t1) ->
            ( match List.find ~f:(fun (n, _) -> n = name) b2.fields with
            | Some (_, t2) -> (name, unify t1 t2)
            | None -> (name, t1) )
            :: acc )
          ~init:[] b1.fields
      in
      let new_base_fields =
        List.fold_left
          ~f:(fun acc (name, t1) ->
            if List.exists ~f:(fun (n, _) -> n = name) acc then acc
            else
              ( match List.find ~f:(fun (n, _) -> n = name) b1.fields with
              | Some (_, t2) -> (name, unify t1 t2)
              | None -> (name, t1) )
              :: acc )
          ~init:new_base_fields b2.fields
      in
      let new_record = {PolyRecord.fields= new_base_fields; poly= ref None} in
      b1.poly := Some new_record ;
      b2.poly := Some new_record ;
      Type.PolyRecord b1
  | Type.PolyRecord r1, Type.Record r2 ->
      (* Try to unify to a poly record - that means we only require the
       * subset of r1 in r2, not a full matching set *)
      List.iter
        ~f:(fun (name, t1) ->
          try
            let _, t2 = List.find_exn ~f:(fun (n, _) -> n = name) r2.fields in
            let _ = unify t1 t2 in
            ()
          with Not_found_s _ ->
            raise
              (CompileError
                 { error= Error.FieldMismatch ("unknown field '" ^ name ^ "'")
                 ; source }) )
        (PolyRecord.get_base r1).fields ;
      Type.Record r2
  | Type.Record _, Type.PolyRecord _ -> unify ~source t2 t1
  | Type.Record r1, Type.Record r2 ->
      (* Non-poly records require everything to match *)
      let r1set =
        List.map ~f:(fun (n, _) -> n) r1.fields
        |> List.sort String.compare |> String.concat ~sep:","
      in
      let r2set =
        List.map ~f:(fun (n, _) -> n) r2.fields
        |> List.sort String.compare |> String.concat ~sep:","
      in
      (* TODO - proper field mismatch error *)
      if r1set <> r2set then
        raise
          (CompileError
             {error= Error.FieldMismatch (r1set ^ " versus " ^ r2set); source}) ;
      List.iter
        ~f:(fun (name, t1) ->
          let _, t2 = List.find_exn ~f:(fun (n, _) -> n = name) r2.fields in
          let _ = unify t1 t2 in
          () )
        r1.fields ;
      Type.Record r2
  | Type.Alias a1, Type.Alias a2 -> unify a1.type_ a2.type_
  | Type.Alias a1, t2 -> unify a1.type_ t2
  | t1, Type.Alias a2 -> unify t1 a2.type_
  | _ ->
    match it1 == it2 with
    | true -> it2
    | false ->
        raise (CompileError {error= Error.TypeMismatch (t1, t2); source})

let rec pattern_type env pattern =
  let open MatchPattern in
  match pattern with
  | Int _ -> (env, Type.Int)
  | Boolean _ -> (env, Type.Bool)
  | String _ -> (env, Type.String)
  | Float _ -> (env, Type.Float)
  | Tuple items ->
      let env, types =
        List.fold_left
          ~f:(fun (env, types) pat ->
            let env, t = pattern_type env pat in
            (env, t :: types) )
          ~init:(env, []) items
      in
      (env, Type.Tuple (List.rev types))
  | Record items ->
      let env, types =
        List.fold_left
          ~f:(fun (env, types) (name, pat) ->
            let env, t = pattern_type env pat in
            (env, (name, t) :: types) )
          ~init:(env, []) items
      in
      (env, Type.PolyRecord {fields= types; poly= ref None})
  | Cons _ -> (
      (* Flatten AST cons to a list, ignoring a binding in the tail position *)
      (* FIXME - the tail position should explicitly be a list, may need to be non_free (?) *)
      let cells = MatchPattern.cons_to_list pattern in
      match List.rev cells with
      | tail :: heads ->
          (* unify the tail immediately with a list type *)
          let env, t = pattern_type env tail in
          let t = unify t @@ Type.List (Type.TypeArg (TypeArg.make ())) in
          let env, cell_type =
            List.fold_left
              ~f:(fun (env, acc_t) pat ->
                let env, t = pattern_type env pat in
                let t2 = unify acc_t (Type.List t) in
                (env, t2) )
              ~init:(env, t) heads
          in
          (env, cell_type)
      | _ -> failwith "Bad cons cell" )
  | EmptyList -> (env, Type.List (Type.TypeArg (TypeArg.make ())))
  | Constructor (name, args) -> (
      Ast.Module.find_constructor env.Env.types name
      |> function
        | None ->
            raise
              (CompileError
                 { Error.error= IdentifierNotFound name
                 ; Error.source= Source.None })
        | Some (adt, t) ->
            (* Bomb out if there's an arg count mismatch *)
            (* TODO this should be a proper compile error *)
            if List.length t.Variant.args <> List.length args then
              failwith "Bad constructor length" ;
            let env, arg_types =
              List.fold_left
                ~f:(fun (env, types) pat ->
                  let env, t = pattern_type env pat in
                  (env, t :: types) )
                ~init:(env, []) args
            in
            let arg_types = List.rev arg_types in
            let _ =
              List.zip_exn t.Variant.args arg_types
              |> List.map ~f:(fun (x, y) -> unify x y)
            in
            (env, Type.Adt adt) )
  | Binding b ->
      let ta = Type.TypeArg (TypeArg.make ()) in
      let env = Env.add b (1, ta) env in
      let env = Env.add_non_free ta env in
      (Env.add b (1, ta) env, ta)

let rec analyse program env =
  match program.Node.expr with
  | Expr.Unit ->
      (env, Type.Unit)
  | Expr.Lambda {Lambda.arg; body} ->
      (* we know nothing about the type arg at this point in time *)
      let local_env, arg_type = Env.new_arg env arg 1 in
      let local_env = Env.add_non_free arg_type local_env in
      (* within the env, the type arg will also need to be within the non-generic list for the lambda *)
      let env2, return_type = analyse body local_env in
      (env, Type.Arrow (arg_type, return_type))
  | Expr.Identifier {module_= {contents= Some module_}; name; type_} ->
      (* Symbol relates to another module*)
      let _, type_ = Env.get_mod_type module_ name program env in
      (env, type_)
  | Expr.Identifier {name; module_; type_} -> (
    try
      (* update ident type now we know about its environment... *)
      let level, typeof_ = Env.get_type name program env in
      match (level, typeof_) with
      (* Outervalue ?? *)
      | 0, Type.Arrow _ ->
          type_ := IdentType.OuterFun ;
          (env, typeof_)
      | 0, _ ->
          type_ := IdentType.OuterVal ;
          (env, typeof_)
      | _ ->
          type_ := IdentType.Var ;
          (env, typeof_)
    with err ->
      try
        let mod_, _ =
          List.find_exn ~f:(fun (_, n) -> n = name) env.qualified_imports
        in
        (* Now we know that this symbol really belongs to an external module,
             mutation the module reference on this identifier *)
        module_ := Some mod_ ;
        let _, t = Env.get_mod_type mod_ name program env in
        (env, t)
      with Not_found -> raise err )
  | Expr.Literal (Literal.Int _) -> (env, Type.Int)
  | Expr.Literal (Literal.Float _) -> (env, Type.Float)
  | Expr.Literal (Literal.Boolean _) -> (env, Type.Bool)
  | Expr.Literal (Literal.String _) -> (env, Type.String)
  | Expr.Literal (Literal.Tuple items) ->
      let folder (env, items) x =
        let env, t = analyse x env in
        (env, items @ [t])
      in
      let env, items = List.fold_left ~f:folder ~init:(env, []) items in
      (env, Type.Tuple items)
  | Expr.Literal (Literal.Record items) ->
      let mapper (name, x) =
        let _, t = analyse x env in
        (name, t)
      in
      let record_type = Type.Record {fields= List.map ~f:mapper items} in
      (env, record_type)
  | Expr.Apply {Apply.expr; arg} -> (
      let env1, ft = analyse expr env in
      let env2, at = analyse arg env1 in
      let _, rt = Env.new_arg env2 "_" 1 in
      let rt2 = unify ft (Type.Arrow (at, rt)) ~source:program.Node.source in
      match rt2 with
      | Type.Arrow (_, rt3) -> (env, rt3)
      | Type.TypeArg a -> (
        match !(a.instance) with Some x -> (env, rt2) | None -> (env, rt2) )
      | _ -> raise (failwith "bad arrow unification") )
  | Expr.Call {Call.module_; fun_; args} ->
      (* analyse args *)
      let _args = List.map ~f:(fun a -> analyse a env) args in
      let _, rt = Env.new_arg env "_" 1 in
      (env, rt)
  | Expr.Let {Let.name; bound_expr; expr} ->
      (* TODO - ensure bound funs have copied envs to avoid
       issues with generalisation *)
      let env, bt = analyse bound_expr env in
      let env = Env.add name (1, bt) env in
      analyse expr env
  | Expr.Match {Match.expr; Match.clauses} ->
      let env, input_type = analyse expr env in
      let env = Env.add_non_free input_type env in
      (* now match the input type with each clause *)
      let _, result_type =
        List.fold_left
          ~f:(fun (acc_input_type, acc_res_type) cl ->
            (* TODO we need to inject things into the env from the pattern!!! *)
            let env, pat_type = pattern_type env cl.MatchClause.pattern in
            let input_type =
              unify pat_type acc_input_type ~source:program.Node.source
            in
            let _, res_type = analyse cl.MatchClause.result env in
            let res_type =
              unify res_type acc_res_type ~source:program.Node.source
            in
            (input_type, res_type) )
          ~init:(input_type, Type.TypeArg (TypeArg.make ()))
          clauses
      in
      (env, result_type)
  | Expr.Literal (Literal.ConsCell (c, rest)) ->
      (* Fold the cons cell down to the bottom and check the types unify *)
      (* TODO -> there's some sort of issue with the unification here *)
      let env, this_type = analyse c env in
      let rest_type =
        match rest.expr with
        | Expr.Literal Literal.EmptyList -> Type.List this_type
        | _ ->
            let _, rt = analyse rest env in
            rt
      in
      let ret_type =
        unify rest_type (Type.List this_type) ~source:program.Node.source
      in
      (env, ret_type)
  | Expr.Literal Literal.EmptyList ->
      (env, Type.List (Type.TypeArg (TypeArg.make ())))
  | Expr.AdtConstructor {type_; name; args} -> (
      (* See if the ADT is defined in the environment *)
      (* NEED TO COPY THE BASE ADT TYPE / VARIANTS (?) *)
      let {Ast.NamedType.type_} = Env.get_user_type type_ env in
      match type_ with
      | Type.Adt adt ->
          let env, arg_types =
            List.fold_left
              ~f:(fun (env, args) a ->
                let env, rt = analyse a env in
                (env, rt :: args) )
              ~init:(env, []) args
          in
          let variant =
            List.find_exn ~f:(fun v -> v.Variant.name = name) adt.Adt.variants
          in
          let env =
            List.fold_left
              ~f:(fun env a -> Env.add_non_free a env)
              ~init:env variant.Variant.args
          in
          let _ =
            List.map2
              ~f:(unify ~source:program.Node.source)
              variant.Variant.args (List.rev arg_types)
          in
          ( env
          , Type.Adt
              {name= adt.Adt.name; args= adt.args; variants= adt.Adt.variants}
          )
      | _ -> raise (failwith "Unknown variant for ADT") )
  | Expr.RecordAccess (expr, field) ->
      let env, expr_type = analyse expr env in
      let env, field_type = Env.new_arg env "_" 1 in
      let access_type =
        Type.PolyRecord {fields= [(field, field_type)]; poly= ref None}
      in
      let unified = unify ~source:expr.Node.source expr_type access_type in
      (env, field_type)
  | Expr.RecordUpdate (expr, update_fields) ->
      let env, original_t = analyse expr env in
      (* TODO - we should unify traditionally so we can cope with typeargs,
         presumably constructing a poly record and unifying with it should do the trick (?) *)
      let (env, new_fields) =
        List.fold_left
          ~f:(fun (env, fields) (field, new_expr) ->
            let env, new_t = analyse new_expr env in
            let clean_fields =
              fields |> List.filter ~f:(fun (f, _) -> f <> field)
            in
            (env, (field, new_t) :: clean_fields ))
          ~init:(env, []) update_fields
      in
      let new_t = Type.PolyRecord {fields= new_fields; poly= ref None} in
      (* Do we really want to unify? We only really want to check that fields exist! *)
      (* let _ = unify ~source:expr.Node.source new_t original_t in *)
      (env, new_t)

(* TODO - proper error for this *)

let get_type_sig name env =
  let rec find = function
    | [] -> None
    | (n, t) :: rest ->
      match n = name with true -> Some t | false -> find rest
  in
  find env.Env.signatures

let rec unify_targs type_ targ_map env =
  let arg_folder (types, targ_map) i =
    let type_, targ_map = unify_targs i targ_map env in
    (types @ [type_], targ_map)
  in
  match type_ with
  | Type.Arrow (arg, expr) ->
      let arg, targ_map = unify_targs arg targ_map env in
      let expr, targ_map = unify_targs expr targ_map env in
      (Type.Arrow (arg, expr), targ_map)
  | Type.TypeArg {name= Some name; instance; id} -> (
    match String.Map.mem targ_map name with
    | true -> (String.Map.find_exn targ_map name, targ_map)
    | false -> (type_, String.Map.set targ_map name type_) )
  | Type.List listT ->
      let arg, targ_map = unify_targs listT targ_map env in
      (Type.List arg, targ_map)
  | Type.Record record ->
      let fields, targ_map =
        List.fold_left
          ~f:(fun (fields, targ_arg) (f, t) ->
            let t, targ_map = unify_targs t targ_map env in
            let field = (f, t) in
            (field :: fields, targ_map) )
          ~init:([], targ_map) record.fields
      in
      (Type.Record {fields}, targ_map)
  | Type.PolyRecord record ->
      let fields, targ_map =
        List.fold_left
          ~f:(fun (fields, targ_arg) (f, t) ->
            let t, targ_map = unify_targs t targ_map env in
            let field = (f, t) in
            (field :: fields, targ_map) )
          ~init:([], targ_map) (PolyRecord.get_base record).fields
      in
      (Type.PolyRecord {fields; poly= ref None}, targ_map)
  | Type.UserType {name; args} ->
      if List.exists ~f:(( = ) name) env.Env.type_refs then
        let args, targ_map =
          List.fold_left ~f:arg_folder ~init:([], targ_map) args
        in
        (Type.SelfRef {name; args}, targ_map)
      else
        let type_ = Env.get_user_type name env in
        (* Try and unify the type args *)
        let args, targ_map =
          List.fold_left ~f:arg_folder ~init:([], targ_map) args
        in
        (* TODO - bomb out if wrong number of constructor args... *)
        let type_ =
          match type_.type_ with
          | Type.Adt adt -> Type.Adt {adt with args}
          | Type.Alias alias ->
              (* TODO: Probably need to make sure this is copied ? *)
              let args_ = List.map2 ~f:unify alias.args args in
              (* TODO - may need to put targ_map out of this scope! *)
              let t, targ_map = unify_targs alias.type_ targ_map env in
              Type.Alias {alias with args; type_= t}
          | Type.TypeArg arg -> Type.TypeArg arg
          | t -> failwith ("Can't match user type to type" ^ Type.format t)
        in
        (type_, targ_map)
  | Type.Tuple els ->
      let types, targ_map =
        List.fold_left ~f:arg_folder ~init:([], targ_map) els
      in
      (Type.Tuple types, targ_map)
  | Type.Adt adt ->
      let variants, targ_map =
        List.fold_left
          ~f:(fun (vars, targ_map) {Variant.name; args} ->
            let args, targ_map =
              List.fold_left ~f:arg_folder ~init:([], targ_map) args
            in
            (vars @ [{Variant.name; args}], targ_map) )
          ~init:([], targ_map) adt.Adt.variants
      in
      (Type.Adt {adt with variants}, targ_map)
  | _ -> (type_, targ_map)

let unify_signature (env, mod_) sig_type =
  (* type args need to be unified by name *)
  let map = String.Map.empty in
  unify_targs sig_type map env

type module_resolver = string -> Module.t

let from_statement (resolve_module: module_resolver) (env, mod_) statement =
  let ( >>= ) = Option.( >>= ) in
  match statement with
  | Statement.Let {name; expr} ->
      let rec_type = Type.TypeArg (TypeArg.make ()) in
      let env_with_rec =
        Env.add name (1, rec_type) env |> Env.add_non_free rec_type
      in
      let new_env, binding_type = analyse expr env_with_rec in
      let _ = unify rec_type binding_type in
      let maybe_sig = get_type_sig name new_env in
      let final_type =
        maybe_sig
        >>= (fun t ->
              try
                let _ = unify t binding_type in
                Some t
              with CompileError _ ->
                raise
                  (CompileError
                     { Error.source= expr.source
                     ; error= Error.TypeSignatureMismatch (binding_type, t) })
              )
        |> Option.value ~default:binding_type
      in
      (* We discard the env we've built up and simply add the final type of the binding to the name *)
      let ret_env = Env.add name (0, final_type) env in
      let typed_binding = TypedNode.make expr (get_instance_type final_type) in
      let binding = Ast.Binding.make name typed_binding in
      (ret_env, {mod_ with Module.bindings= binding :: mod_.Module.bindings})
  | Statement.TypeSignature {name; type_} ->
      let type_, _ = unify_signature (env, mod_) type_ in
      ({env with Env.signatures= (name, type_) :: env.signatures}, mod_)
  | Statement.Type {name; type_} ->
      let map = String.Map.empty in
      let ref_env = {env with type_refs= name :: env.type_refs} in
      let type_, _ = unify_targs type_ map ref_env in
      let new_type = {Ast.NamedType.name; type_} in
      let mod_ = {mod_ with types= new_type :: mod_.types} in
      let env = {env with Env.types= new_type :: env.types} in
      (env, mod_)
  | Statement.Module ms -> (env, {mod_ with Ast.Module.name= ms.name})
  | Statement.Import import ->
      let resolved_module = resolve_module import.module_ in
      (* fold env importing qualified imports *)
      let qualified_imports =
        env.qualified_imports
        @ List.map ~f:(fun n -> (import.module_, n)) import.qualified
      in
      ( { env with
          modules= (import.module_, resolved_module) :: env.modules
        ; qualified_imports }
      , {mod_ with imports= import :: mod_.imports} )
  | Statement.Verbatim {name= "lua"; code} ->
      (env, {mod_ with verbatim= mod_.verbatim ^ code})
  | Statement.Verbatim {name} ->
      failwith ("Unknown verbatim block type: " ^ name)

let from_statements ?(module_resolver: module_resolver option) statements =
  let module_resolver =
    module_resolver
    |> function
      | Some v -> v
      | _ ->
          fun _ -> failwith "Cannot import module: no module resolver provided"
  in
  let env, mod_ =
    List.fold_left
      ~f:(from_statement module_resolver)
      ~init:(Env.empty, Ast.Module.empty)
      statements
  in
  match mod_.name with
  | "__empty__" -> (env, {mod_ with name= "Main"})
  | _ -> (env, mod_)

let type_statements ?module_resolver statements =
  let module_resolver =
    module_resolver
    |> function
      | Some v -> v
      | _ ->
          fun _ -> failwith "Cannot import module: no module resolver provided"
  in
  try Ok (from_statements ~module_resolver statements)
  with CompileError err -> Error err
