{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let drop_initial str =
  String.sub str 1 ((String.length str) - 1)

let make_field_magic drop str =
  let drop_len = (String.length drop) + 1 in
  let base = String.sub str drop_len ((String.length str) - drop_len) in
  let first = String.sub base 0 1 in
  let rest = String.sub base 1 ((String.length base) - 1) in
  (first |> String.lowercase_ascii) ^ rest


let split_verbatim str =
  let open Str in
  let open String in
  let splitter = split (regexp "[ \n\r\t]") in
  match splitter str with
  | hd :: tail -> 
    let code = tail |> concat " " in
    let code = sub code 0 ((length code) - 2) in
    let type_ = sub hd 2 ((length hd) - 2) in
    VERBATIM (type_, code)
  | _ -> failwith "Bad verbatim definition"

}

let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = "." digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let whitespace_only_line = white* newline

let ident = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let dot_ident = '.' ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let up_ident = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let up_ident_dot = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '.']*
let accessor = '#' "get" up_ident
let setter = '#' "set" up_ident
let updater = '#' "update" up_ident


let callref = ['@'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '.' '+' '-' '=' '*' '/' '%' '>' '<' '~']*


let verbatim = '[' '%' ['a'-'z']* "\\S"* _* '%' ']'

let begin_statement = "^\\S"

let comment = "--" ([^ '\r' '\n'])*

rule read =
  parse
  | newline  { next_line lexbuf; LINE_START }
  | comment  { read lexbuf }
  | whitespace_only_line { next_line lexbuf; LINE_START }
  | white    { WS }
  | verbatim { (split_verbatim (Lexing.lexeme lexbuf)) }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "+."     { INFIX "+." }
  | "*."     { INFIX "*." }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | ".."     { POLY }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "match"  { MATCH }
  | "with"   { WITH }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | "()"     { UNIT }
  | '('      { LEFT_PARENS }
  | ')'      { RIGHT_PARENS }
  | '{'      { LEFT_BRACE }
  | '}'      { RIGHT_BRACE }
  | '['      { LEFT_BRACK }
  | ']'      { RIGHT_BRACK }
  | ':'      { COLON }
  | ','      { COMMA }
  | '|'      { PIPE }
  | "module" { MODULE }
  | "import" { IMPORT }
  | "let"    { LET }
  | "match"  { MATCH }
  | "with"   { WITH }
  | "in"     { IN }
  | "Int"    { INT_TYPE }
  | "Float"  { FLOAT_TYPE }
  | "Bool"   { BOOLEAN_TYPE }
  | "type"   { TYPE }
  | "typedef" { TYPEDEF }
  | "end"    { ENDER }
  | callref  { CALLREF (drop_initial (Lexing.lexeme lexbuf)) }
  | "=="     { INFIX "==" }
  | "/="     { INFIX "/=" }
  | "++"     { INFIX "++" }
  | "&&"     { INFIX "&&" }
  | "||"     { INFIX "||" }
  | "<|"     { INFIXR "<|" }
  | ">>"     { INFIX ">>" }
  | '='      { EQUALS }
  | "->"     { ARROW }
  | '\\'     { LAMBDA }
  | "::"     { CONS_OP }
  | dot_ident{ DOT_IDENT (drop_initial (Lexing.lexeme lexbuf)) }
  | accessor { ACCESSOR (make_field_magic "get" (Lexing.lexeme lexbuf))}
  | setter   { SETTER (make_field_magic "set" (Lexing.lexeme lexbuf))}
  | updater  { UPDATER (make_field_magic "update" (Lexing.lexeme lexbuf))}
  | ident    { IDENT (Lexing.lexeme lexbuf)}
  | "|>"     { INFIX "|>" }
  | "<|"     { INFIX "<|" }
  | up_ident { UPPER_IDENT (Lexing.lexeme lexbuf)}
  | up_ident_dot { UPPER_IDENT_DOT (Lexing.lexeme lexbuf) } (* TODO - check valid *)
  | '+'      { INFIX "+" }
  | '-'      { INFIX "-" }
  | '*'      { INFIX "*" }
  | '/'      { INFIX "/" }
  | '>'      { INFIX ">" }
  | '<'      { INFIX "<" }
  | eof      { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}


and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
