
{
  open Parse   (* ./parse.mly *)
  open Lexing  (* librairie standard *)
}

let varname = ['a'-'z'  'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let integer = ['0'-'9']['0'-'9']* | ("0x" | "0X")['0'-'9' 'A'-'F' 'a'-'f']*

(*
{
let keyword_table = Hashtabl.create 53
let _ =
  List.iter (fun (kwd, tok) -> hashtabl.add kwd tok)
            [ "KWD1", KWD1;
              "KWD2", KWD2
            ]
}
*)
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
  | '"' (([^'"' '\\'] | "\"")* as s) '"' { STRING(Scanf.unescaped s) }
  | "(*"               { comlex 0 lexbuf }

  | "("                { LPAR }
  | ")"                { RPAR }

  | "+"                { BIN_PLUS "+" }
  | "-"                { BIN_PLUS "-" }
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
  | ['\n']+                 { token lexbuf }

  | eof                { EOF }

and comlex n = parse
  | "(*"          { comlex (n + 1) lexbuf }
  | "*)"          { if n == 0 then token lexbuf else comlex (n - 1) lexbuf }
  | _             { comlex n lexbuf }
