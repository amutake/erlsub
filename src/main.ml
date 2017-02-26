open Lexing
open Lexer
open Base

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
                 pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
     Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
     []
  | Parser.Error ->
     Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
     exit (-1)

let rec parse_and_print lexbuf =
  let prog = parse_with_error lexbuf in
  String.concat ~sep:"\n" (List.map prog ~f:(fun (a, b, c) -> Expr.to_string c))

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx

let main () =
  if Array.length Sys.argv = 1 then
    let filename = Sys.argv.(1) in
    loop filename ()
  else
    print_endline "Usage: erlsubc [filename]"
