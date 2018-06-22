open Ast
open Lexing
open Lexer

let rec read_until_non_ws q lexbuf =
  match Lexer.read lexbuf with
  | WS -> Lexer.read lexbuf
  | LINE_START -> read_until_non_ws q lexbuf
  | other -> Queue.push other q ; Parser.INDENT0

let reader () =
  let q = Queue.create () in
  let rec read lexbuf =
    match Queue.length q > 0 with
    | true -> Queue.pop q
    | false ->
      match Lexer.read lexbuf with
      | WS -> read lexbuf
      | LINE_START -> read_until_non_ws q lexbuf
      | other -> other
  in
  read

let parse_with_error reader lexbuf =
  try Ok (Parser.prog reader lexbuf) with
  | SyntaxError msg ->
      let pos = lexbuf.lex_curr_p in
      Error
        { Error.source=
            Source.Pos
              { file= "__no_file__"
              ; line= pos.pos_lnum
              ; char= pos.pos_cnum - pos.pos_bol + 1 }
        ; error= Error.Syntax msg }
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Error
        { Error.source=
            Source.Pos
              { file= "__no_file__"
              ; line= pos.pos_lnum
              ; char= pos.pos_cnum - pos.pos_bol + 1 }
        ; error=
            Error.Syntax ("Unexpected token '" ^ Lexing.lexeme lexbuf ^ "'") }
  | CompileError err -> Error err

let parse_single_with_error lexbuf = parse_with_error (reader ()) lexbuf

let parse_all_with_error lexbuf =
  let r = reader () in
  let rec inner statements =
    match parse_with_error r lexbuf with
    | Ok None -> Ok (List.rev statements)
    | Ok (Some statement) -> inner (statement :: statements)
    | Error err -> Error err
  in
  inner []
