%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI DOT
%token PRINT
%token EOF

%token MINUS EXCLA PLUS STAR SLASH PERCENT EQUAL LESS MORE AND VBAR

%token TYP_INT
%token TYP_BOOL
%token TYP_VOID

%token VAR
%token ATTRIBUTE
%token CLASS
%token METHOD

%token TRUE
%token FALSE
%token THIS
%token NEW

%token IF
%token ELSE
%token WHILE
%token RETURN

// %token VAR
// %token ATTRIBUTE

%start program
%type <Kawa.program> program

%%

program:
| MAIN BEGIN main=list(instruction) END EOF
    { {classes=[]; globals=[]; main} }
;

class_def:
| CLASS id=IDENT BEGIN attr=list(attr_decl) meth=list(meth_def) END
    { {class_name=id; attributes=attr; methods=meth} }
;

typed_ident:
| t=typ id=IDENT {id, t}
;

var_decl:
| VAR tid=typed_ident SEMI {tid}
;

attr_decl:
| ATTRIBUTE tid=typed_ident SEMI {tid}
;

typ:
| TYP_INT { TInt }
| TYP_BOOL { TBool }
| id=IDENT { TClass(id) }
| TYP_VOID { TVoid }
;

meth_def:
| METHOD t=typ id=IDENT LPAR args=list(typed_ident) RPAR BEGIN var=list(var_decl) instr=list(instruction) END
    { {method_name=id; code=instr; params=args; locals=var; return=t} }
;

expression:
| n=INT { Int(n) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| THIS { This }
| m=mem { Get(m) }
| op=uop e=expression { Unop(op, e) }
| e1=expression op=bop e2=expression { Binop(op, e1, e2) }
| LPAR e=expression RPAR { e }
| NEW id=IDENT { New(id) }
| NEW id=IDENT LPAR args=list(expression) RPAR { NewCstr(id, args) }
| e=expression DOT id=IDENT LPAR expr=list(expression) RPAR { MethCall(e, id, expr) }
;

mem:
| id=IDENT { Var(id) }
| e=expression DOT id=IDENT { Field(e, id) }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| m=mem EQUAL e=expression SEMI { Set(m, e) }
| IF LPAR e1=expression RPAR BEGIN i1=list(instruction) END ELSE BEGIN i2=list(instruction) END
    { If(e1, i1, i2) }
| WHILE LPAR e=expression RPAR BEGIN i=list(instruction) END
    { While(e, i) }
| RETURN e=expression SEMI { Return(e) }
| e=expression SEMI { Expr(e) }
;

uop:
| MINUS { Opp } 
| EXCLA { Not }
;

bop:
| PLUS { Add }
| MINUS { Sub }
| STAR { Mul }
| SLASH { Div }
| PERCENT { Rem }
| LESS { Lt }
| LESS EQUAL { Le }
| MORE { Gt }
| MORE EQUAL { Ge }
| EQUAL EQUAL { Eq }
| EXCLA EQUAL { Neq }
| AND AND { And }
| VBAR VBAR { Or }
;
