open Lexing
open Lexer

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
  BatString.concat "\n" (BatList.map (fun (a, b, c) -> Expr.to_string c) prog)

let loop filename () =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  close_in inx

let main () =
  let twice = Alpha.Abs (["f"], Alpha.Abs (["x"], Alpha.App (Alpha.AbsVar "f", [Alpha.App (Alpha.AbsVar "f", [Alpha.AbsVar "x"])]))) in
  print_endline (Biunification.Scheme.to_string (Biunification.p (BatMap.empty) twice));

  let xx = Alpha.Abs (["x"], Alpha.App (Alpha.AbsVar "x", [Alpha.AbsVar "x"])) in
  print_endline (Biunification.Scheme.to_string (Biunification.p (BatMap.empty) xx));

  let xxx = Alpha.Abs (["x"], Alpha.App (Alpha.App (Alpha.AbsVar "x", [Alpha.AbsVar "x"]), [Alpha.AbsVar "x"])) in
  print_endline (Biunification.Scheme.to_string (Biunification.p (BatMap.empty) xxx));

  if Array.length Sys.argv = 2 then
    let filename = Sys.argv.(1) in
    loop filename ()
  else
    print_endline "Usage: erlsubc [filename]"
