(* identifiers *)
%token <Base.String.t> IDENT

(* values *)
%token <Base.Int.t> INT
%token <Base.Float.t> FLOAT
%token <Base.String.t> ATOM
%token <Base.String.t> VARIANT

(* structures *)
%token DEF
%token EQUALS
%token FUN
%token ARROW
%token MATCH
%token WITH
%token END
%token LET
%token REC
%token IN

(* chars *)
%token EOF
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token COMMA
%token VBAR

(* binary operators *)
%token OP_EQ
%token OP_LT
%token OP_GT
%token OP_LE
%token OP_GE
%token OP_ADD
%token OP_SUB
%token OP_MUL
%token OP_DIV
%nonassoc OP_EQ
%nonassoc OP_LT
%nonassoc OP_GT
%nonassoc OP_LE
%nonassoc OP_GE
%left OP_ADD
%left OP_SUB
%left OP_MUL
%left OP_DIV

(* entry point *)
%start <(Base.String.t * Base.String.t Base.List.t * Expr.t) Base.List.t> prog

%%

prog:
  | ds = defs
    { ds }

defs:
  | EOF
    { [] }
  | d = def; ds = defs
    { d :: ds }

def:
  | DEF; id = IDENT; params = params; EQUALS; e = expr
    { (id, params, e) }

expr:
  | FUN; params = params; ARROW; e = expr
    { Expr.Abs (params, e) }
  | LET; REC; id = IDENT; EQUALS; e1 = expr; IN e2 = expr
    { Expr.Letrec (id, e1, e2) }
  | LET; id = IDENT; EQUALS; e1 = expr; IN; e2 = expr
    { Expr.Let (id, e1, e2) }
  | MATCH; e = expr; WITH; bs = nonempty_list(branch); END
    { Expr.Match (e, bs) }
  | LBRACE; es = separated_list(COMMA, expr); RBRACE
    { Expr.Tuple es }
  | e = app
    { e }
  | v = value
    { Expr.Val v }
  | id = IDENT
    { Expr.Var id }

app:
  | LPAREN; e = expr; RPAREN; args = args
    { Expr.App (e, args) }
  | id = IDENT; args = args
    { Expr.App (Expr.Var id, args) }
  | e1 = expr; op = binop; e2 = expr
    { Expr.App (Expr.Var op, [e1; e2]) }

branch:
  | VBAR; vt = VARIANT; ARROW; e = expr
    { (vt, [], e) }
  | VBAR; vt = VARIANT; params = params; ARROW; e = expr
    { (vt, params, e) }

params:
  | LPAREN; params = separated_list(COMMA, IDENT); RPAREN
    { params }

args:
  | LPAREN; args = separated_list(COMMA, expr); RPAREN
    { args }

value:
  | n = INT
    { Value.Int n }
  | f = FLOAT
    { Value.Float f }
  | a = ATOM
    { Value.Atom a }

%inline binop:
| OP_EQ { "(==)" }
| OP_LT   { "(<)" }
| OP_GT   { "(>)" }
| OP_LE  { "(<=)" }
| OP_GE  { "(>=)" }
| OP_ADD   { "(+)" }
| OP_SUB   { "(-)" }
