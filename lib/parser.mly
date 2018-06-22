%{ open Ast %}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT
%token <string> DOT_IDENT
%token <string> UPPER_IDENT
%token <string> UPPER_IDENT_DOT
%token <string> CALLREF
%token LINE_START
%token INDENT0
%token WS
%token EQUALS
%token LET
%token MATCH
%token MODULE
%token IMPORT
%token WITH
%token IN
%token INT_TYPE
%token FLOAT_TYPE
%token BOOLEAN_TYPE
%token TRUE
%token FALSE
%token LEFT_PARENS
%token RIGHT_PARENS
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token POLY
%token COLON
%token CONS_OP
%token COMMA
%token LAMBDA
%token PIPE
%token ARROW
%token ENDER
%token EOF
%token TYPE
%token UNIT
%token TYPEDEF
%token <string> INFIX
%token <string> INFIXR
%token <string * string> VERBATIM
%token <string> ACCESSOR
%token <string> SETTER
%token <string> UPDATER

%nonassoc IN
%right CONS_OP
%left INFIX
%right INFIXR

%{ open Ast %}
%{ open Parser_utils %}

%start <Ast.Statement.t option> prog

%%

prog:
  | EOF       { None }
  | s = statement { Some s }
  | WS { failwith "Ignored token found" }
  | LINE_START { failwith "Ignored token found" }
  ;


statement:
  | MODULE; name = module_name; end_statement
      { Statement.Module { name = name; } }
  | IMPORT; name = module_name; qualified = qualified_imports; end_statement
      { Statement.Import { Import.module_ = name; qualified } }
  | name = ident; EQUALS; expression = expr; end_statement
      { Statement.Let { name = name; expr = expression } }
  | name = ident; args = arglist; EQUALS; expression = expr; end_statement
      { Statement.Let { name = name; expr = curry_function args expression (pos $startpos) } }
  | name = ident; COLON; type_ = type_expr; end_statement
      { Statement.TypeSignature { name; type_; } }
  | TYPE; name = UPPER_IDENT; args = type_arg_list; EQUALS; variants = variant_list; end_statement
      { Statement.Type { name = name; type_ = Type.Adt ({ Adt.name; args; variants }) |> match_up_typeargs args } }
  | TYPEDEF; name = UPPER_IDENT; args = type_arg_list; EQUALS; type_ = type_expr; end_statement
      { 
        let type_ = match_up_typeargs args type_ in
        Statement.Type { name = name; type_ = Type.Alias { name = name; type_; args } } }
  | v = VERBATIM; end_statement
      { let name, code = v in
        Statement.Verbatim { name; code } }

qualified_imports:
  | { [] }
  | LEFT_PARENS; idents = separated_nonempty_list(COMMA, ident); RIGHT_PARENS { idents }

module_name:
  | n = UPPER_IDENT { n }
  | n = UPPER_IDENT_DOT { n }

end_statement:
  | INDENT0 { true }
  | EOF { true }

ident:
  | s=IDENT { s }
  | LEFT_PARENS s=INFIX RIGHT_PARENS { "(" ^ s ^ ")" }
  | LEFT_PARENS s=INFIXR RIGHT_PARENS { "(" ^ s ^ ")" }
  | LEFT_PARENS CONS_OP RIGHT_PARENS { "(::)" }
  | u=UPPER_IDENT { u }
  | u=UPPER_IDENT_DOT { u }

expr:
  | m = most_expr %prec IN { m }
  | LET; name = IDENT; args = arglist; EQUALS; expression = expr; IN; result = expr; end_statement
      { Node.make (Expr.Let { name = name; bound_expr = curry_function args expression (pos $startpos); expr = result }) (pos $startpos) }

most_expr:
  | e = simple_expr { e }
  | m = match_ { m }
  | head=most_expr; CONS_OP; tail=most_expr;
    { Node.make (Expr.Literal (Literal.ConsCell (head, tail))) (pos $startpos) }
  | LAMBDA; args = arglist; ARROW; expression = expr
    { curry_function args expression (pos $startpos)}
  | callref = CALLREF; args = expr_list { make_call callref args (pos $startpos) }
    (* TODO - we write this out as a 'match' node but we should mark it as a let somehow for error reporting *)
  | LET; pattern = match_pattern; EQUALS; input=most_expr; IN; expr=expr;
    { Node.make (Expr.Match { Match.expr = input; clauses = [{ MatchClause.pattern = pattern; result = expr}]}) (pos $startpos)}

simple_expr:
  | e = very_simple_expr; { e }
  | e1 = most_expr; i=INFIX; arg = most_expr;
    { let infix = Node.make (Expr.Identifier
                            (Identifier.make ("(" ^ i ^ ")") (ref IdentType.Unknown)))
                            (pos $startpos) in
      make_apply infix ([arg; e1]) (pos $startpos)}
  | e1 = most_expr; i=INFIXR; arg = most_expr;
    { let infix = Node.make (Expr.Identifier
                            (Identifier.make ("(" ^ i ^ ")") (ref IdentType.Unknown)))
                            (pos $startpos) in
      make_apply infix ([arg; e1]) (pos $startpos)}

   | callable = very_simple_expr; args = expr_list;
    { make_apply callable (List.rev args) (pos $startpos) }

very_simple_expr:
  | UNIT { Node.make Expr.Unit (pos $startpos) }
  | LEFT_PARENS; e = expr; RIGHT_PARENS { e }
  | i = INT { Node.make (Expr.Literal (Literal.Int i)) (pos $startpos) }
  | f = FLOAT { Node.make (Expr.Literal (Literal.Float f)) (pos $startpos) }
  | TRUE    { Node.make (Expr.Literal (Literal.Boolean true)) (pos $startpos) }
  | FALSE   { Node.make (Expr.Literal (Literal.Boolean false)) (pos $startpos)  }
  | s = STRING { Node.make (Expr.Literal (Literal.String s)) (pos $startpos)  }
  | symbol = ident { Node.make (Expr.Identifier (Identifier.make symbol (ref IdentType.Unknown))) (pos $startpos) }
  | l = list_expr { l }
  | r = record { r }
  | expr = very_simple_expr; i = DOT_IDENT; { Node.make (Expr.RecordAccess (expr, i)) (pos $startpos) }
  | field = ACCESSOR {
       let ident_name, ident = gen_ident (pos $startpos) in
       let accessor = Node.make (Expr.RecordAccess (ident, field)) (pos $startpos) in
       Node.make (Expr.Lambda { arg = ident_name; body = accessor }) (pos $startpos)
    }
  | field = SETTER {
       let record_name, record = gen_ident (pos $startpos) in
       let value_name, value = gen_ident (pos $startpos) in
       let setter = Node.make (Expr.RecordUpdate (record, [(field, value)])) (pos $startpos) in
       let setNode = Node.make (Expr.Lambda { arg = record_name; body = setter }) (pos $startpos) in
       Node.make (Expr.Lambda { arg = value_name ; body = setNode }) (pos $startpos)
    }
  | field = UPDATER {
       let record_name, record = gen_ident (pos $startpos) in
       let fun_name, fun_ident = gen_ident (pos $startpos) in
       let accessor = Node.make (Expr.RecordAccess (record, field)) (pos $startpos) in
       let updated_value = Node.make (Expr.Apply { arg = accessor; expr = fun_ident }) (pos $startpos) in
       let setter = Node.make (Expr.RecordUpdate (record, [(field, updated_value)])) (pos $startpos) in
       let setNode = Node.make (Expr.Lambda { arg = record_name; body = setter }) (pos $startpos) in
       Node.make (Expr.Lambda { arg = fun_name; body = setNode }) (pos $startpos)
    }

  | r = record_update { r }
  | t = tuple { t }

match_:
    MATCH; input = most_expr; WITH; clauses = match_clauses; ENDER
      { Node.make (Expr.Match { Match.expr = input; clauses = clauses } ) (pos $startpos) }

match_clauses:
    items = separated_nonempty_list(PIPE, match_clause) { items }

match_clause:
    pat = match_pattern; ARROW; result = most_expr
      { { MatchClause.pattern = pat; result; } }

match_pattern:
  | p = simple_pattern { p }
  | t = tuple_pat { t }
  | c = cons_pat { c }
  | l = list_pat { l }
  | r = record_pat { r }
  | constructor = constructor_pat { constructor }


simple_pattern:
  | i = INT { MatchPattern.Int i }
  | f = FLOAT { MatchPattern.Float f }
  | TRUE { MatchPattern.Boolean true }
  | FALSE { MatchPattern.Boolean false }
  | symbol = IDENT { MatchPattern.Binding symbol }
  | c = simple_constructor_pat { c }
  | LEFT_PARENS; pat = match_pattern; RIGHT_PARENS; { pat }
  | s = STRING { MatchPattern.String s }

tuple_pat:
  | items = tuple_pat_items; { MatchPattern.Tuple items }

tuple_pat_items:
  | p1 = simple_pattern; COMMA; p2 = simple_pattern; {[p1; p2]}
  | p1 = simple_pattern; COMMA; items = tuple_pat_items { p1 :: items }

constructor_pat:
  | name = UPPER_IDENT; items = nonempty_list(simple_pattern);
    { MatchPattern.Constructor (name, items) }

simple_constructor_pat:
  | name = UPPER_IDENT;
    { MatchPattern.Constructor (name, []) }

cons_pat:
  | h = simple_pattern; CONS_OP; t = match_pattern
    { MatchPattern.Cons (h, t) }

list_pat: LEFT_BRACK; items = separated_list(COMMA, simple_pattern); RIGHT_BRACK;
    { make_match_cons_literal (List.rev items) }

record_pat:
  | LEFT_BRACE; items=separated_list(COMMA, record_match_field); RIGHT_BRACE { MatchPattern.Record items }

record_match_field:
  | name = IDENT; EQUALS; pat = simple_pattern
    { ( name, pat ) }


tuple:
  | LEFT_PARENS; items = tuple_items; RIGHT_PARENS
    { Node.make (Expr.Literal (Literal.Tuple items)) (pos $startpos) }

(* Tuples must be a least n2 *)
tuple_items:
  | e = simple_expr; COMMA; e2 = simple_expr; { [e; e2]}
  | e = simple_expr; COMMA; items = tuple_items { e :: items }

record:
  | LEFT_BRACE; items = separated_list(COMMA, record_item); RIGHT_BRACE;
    { Node.make (Expr.Literal (Literal.Record items)) (pos $startpos)}

record_item:
  | name = IDENT; EQUALS; e = simple_expr { (name, e) }

record_update:
  | LEFT_BRACE; e = simple_expr; PIPE; items = separated_nonempty_list(COMMA, record_item); RIGHT_BRACE;
     { Node.make (Expr.RecordUpdate (e, items)) (pos $startpos)}

list_expr: LEFT_BRACK; items = separated_list(COMMA, simple_expr); RIGHT_BRACK;
    { make_cons_literal items (pos $startpos) }


expr_list:
  | e = very_simple_expr; exprs = expr_list
    { e :: exprs }
  | e = very_simple_expr
    { [e] }

arglist:
  | arg = IDENT; args = arglist
    { arg :: args }
  | arg = IDENT
    { [arg] }

type_expr:
  | t = separated_nonempty_list(ARROW, arrow_top_type)
    { make_fun_type t }
  | t = tuple_type { t }

arrow_top_type:
  | t = simple_type { t }
  | name = UPPER_IDENT; args = nonempty_list(simple_type)
    { match name with
      (* TODO - should error if more than one type arg passed to hd! *)
      | "List" -> Type.List (List.hd args)
      | _ -> Type.UserType { name; args; type_ = None } }


simple_type:
  | INT_TYPE { Type.Int }
  | FLOAT_TYPE { Type.Float }
  | BOOLEAN_TYPE { Type.Bool }
  | name = IDENT
     { let t = (TypeArg.make ()) in
       Type.TypeArg { t with TypeArg.name = Some name } }
  | LEFT_PARENS; t = type_expr; RIGHT_PARENS { t }
  | name = UPPER_IDENT; {
      match name with
      | "String" -> Type.String
      | _ -> Type.UserType { name; args = []; type_ = None } }
  | r = record_type { r }

record_type:
 | LEFT_BRACE; fields = separated_nonempty_list(COMMA, record_field_type); PIPE; POLY; RIGHT_BRACE;
      { Type.PolyRecord { fields; poly = ref None } }
 | LEFT_BRACE; fields = separated_nonempty_list(COMMA, record_field_type); RIGHT_BRACE;
      { Type.Record { fields } }

record_field_type:
  name = IDENT; COLON; t = simple_type { ( name, t ) }

tuple_type:
  | parts = tuple_type_parts; { Type.Tuple parts }

tuple_type_parts:
  | t1 = simple_type; COMMA; t2 = simple_type { [t1; t2 ] }
  | t = simple_type; COMMA; rest = tuple_type_parts { t :: rest }

type_arg_list:
  types = list(IDENT)
    { types |> List.map (fun name ->
      let t = (TypeArg.make () ) in
      Type.TypeArg { t with TypeArg.name = Some name }) }

variant_list:
  types = separated_list(PIPE, variant_def) { types }

variant_def:
 | name = UPPER_IDENT; args = variant_def_list
  { { Variant.name = name; args } }

variant_def_list:
  | /* empty */ { [] }
  | e = simple_type; rest = variant_def_list { e :: rest }
