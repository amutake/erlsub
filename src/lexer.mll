{
open Lexing
open Parser
open Base

exception SyntaxError of string
}

let int = ['1'-'9'] ['0'-'9']* | '0'
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t' '\n' '\r']+
let comment = '#' [^ '\n']* '\n'
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let variant = '`' ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let atom = '\'' ['a'-'z' 'A'-'Z' '0'-'9' '_']* '\''

rule read =
  parse
  | white    { read lexbuf }
  | comment  { read lexbuf }
  | "->"     { ARROW }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | "{"      { LBRACE }
  | "}"      { RBRACE }
  | ","      { COMMA }
  | "|"      { VBAR }
  | "rec"    { REC }
  | "="      { EQUALS }
  | "fun"    { FUN }
  | "let"    { LET }
  | "in"     { IN }
  | "def"    { DEF }
  | "match"  { MATCH }
  | "with"   { WITH }
  | "end"    { END }

  | "=="     { OP_EQ }
  | "<"      { OP_LT }
  | ">"      { OP_GT }
  | "<="     { OP_LE }
  | ">="     { OP_GE }
  | "+"      { OP_ADD }
  | "-"      { OP_SUB }
  | "*"      { OP_MUL }
  | "/"      { OP_DIV }

  | variant  { VARIANT (String.drop_prefix (Lexing.lexeme lexbuf) 1) }
  | atom     { ATOM (String.drop_prefix (String.drop_suffix (Lexing.lexeme lexbuf) 1) 1) }
  | id       { IDENT (Lexing.lexeme lexbuf) }
  | float    { FLOAT (Float.of_string (Lexing.lexeme lexbuf)) }
  | int      { INT (Int.of_string (Lexing.lexeme lexbuf)) }
  | eof      { EOF }
