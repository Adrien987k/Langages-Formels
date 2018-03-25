%{
  open Expr
%}

%token EOF

%token <string> VAR
%token <string> STRING
%token <int> INT

%token <string> BIN_MULT
%token PLUS
%token MINUS
%token <string> BIN_CMP
%token NOT
%token AND
%token OR

%token LPAR RPAR

%token IF THEN ELSE
%token WHILE DO
%token ASSIGN
%token SKIP
%token SEQ
%token BEGIN END

%token COMMA

%left SEQ
%nonassoc DO
%nonassoc ELSE

%left OR
%left AND
%left NOT
%left BIN_CMP

%left PLUS MINUS
%left BIN_MULT
%left UMINUS


/* Les non-terminaux par lesquels l'analyse peut commencer,
 * et la donnée de leurs types. */

%start terminated_expr
%type <Expr.t> terminated_expr

%%

terminated_expr:
  | cmd EOF { $1 }
  | expr EOF { $1 }

expr:
  | INT                            { Int $1 }
  | VAR                            { Var $1 }
  | STRING                         { String $1 }
  | LPAR expr RPAR                 { $2 }
  | expr PLUS expr                 { App ("+",[$1;$3]) }
  | expr MINUS expr                { App ("-",[$1;$3]) }
  | MINUS expr   %prec UMINUS      { App ("-",[$2]) }
  | expr BIN_MULT expr             { App ($2,[$1;$3]) }
  | expr BIN_CMP expr              { App ($2,[$1;$3]) }
  | expr AND expr                  { App ("and",[$1;$3]) }
  | expr OR expr                   { App ("or",[$1;$3]) }
  | NOT expr                       { App ("not",[$2]) }
  | VAR LPAR expr_seq RPAR         { App ($1, $3) }
  | VAR LPAR RPAR                  { App ($1, []) }

expr_seq:
  | expr COMMA expr_seq            { $1 :: $3 }
  | expr                           { [$1] }

cmd:
  | VAR ASSIGN expr           { Assign($1,$3) }
  | SKIP                      { Skip }
  | cmd SEQ cmd               { Seq($1,$3) }
  | IF expr THEN cmd ELSE cmd { Ite($2,$4,$6) }
  | WHILE expr DO cmd         { While($2,$4) }
  | BEGIN cmd END             { $2 }
