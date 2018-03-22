
{
  open Parse   (* ./parse.mly *)
  open Lexing  (* librairie standard *)

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let varname = ['a'-'z'  'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let integer = ['0'-'9']['0'-'9']* | ("0x" | "0X")['0'-'9' 'A'-'F' 'a'-'f']*


rule token = parse

  | ":="               { ASSIGN }
  | "skip"             { SKIP }
  | ";"                { SEQ }
  | "if"               { IF }
  | "then"             { THEN }
  | "else"             { ELSE }
  | "while"            { WHILE }
  | "do"               { DO }
  | "begin"            { BEGIN }
  | "end"              { END }

  | varname as v       { VAR v }
  | integer as i       { INT (int_of_string i) }
  | '"'                { string (Buffer.create 17) lexbuf }
  | "(*"               { comlex 0 lexbuf }

  | "("                { LPAR }
  | ")"                { RPAR }

  | "+"                { PLUS }
  | "-"                { MINUS }
  | "*"                { BIN_MULT "*" }
  | "/"                { BIN_MULT "/" }
  | "="                { BIN_CMP "=" }
  | "<"                { BIN_CMP "<" }
  | ">"                { BIN_CMP "<" }


  | "and"              { AND }
  | "or"               { OR }
  | "not"              { NOT }

  | ","                { COMMA }

  | [' ' '\t' '\r']+   { token lexbuf }
  | "\n"               { next_line lexbuf; token lexbuf }
  | eof                { EOF }
  | _                  { failwith "Syntax error in lexer" }


and string buf = parse
  | '"'           { STRING (Scanf.unescaped(Buffer.contents buf)) }
 | [^ '"' '\\']+ | "\\\\" | "\\\""
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      string buf lexbuf
    }
  | _ { failwith "Illegal String charactere" }


and comlex n = parse
  | "(*"          { comlex (n + 1) lexbuf }
  | "*)"          { if n == 0 then token lexbuf else comlex (n - 1) lexbuf }
  | _             { comlex n lexbuf }
