%{

  open Lexing
  open Kawa

%}

// Reserved keywords
%token MAIN VAR ATTRIBUTE METHOD CLASS NEW THIS IF ELSE WHILE RETURN PRINT EXTENDS FINAL INSTANCEOF PRINTLN

// Arithmetic operators
%token SUB ADD MUL DIV MOD

// Boolean arithmetic operators
%token NOT EQ NEQ LT LE GT GE AND OR SEQ SNEQ

// Symbols
%token LPAR RPAR BEGIN END SEMI DOT COMMA

// Types
%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token TINT TBOOL TVOID TSTRING

// Miscallaneous
%token ASSIGN
%token <string> IDENT
%token EOF
%token UOP

// Tokens' priority
%left OR
%left AND
%nonassoc LT LE GT GE EQ NEQ SEQ SNEQ
%left ADD SUB
%left MUL DIV MOD INSTANCEOF
%nonassoc UOP
%left DOT

%start program
%type <Kawa.program> program

%%

program:
| globals=vars_decl classes=list(class_def) MAIN BEGIN main=list(instruction) END EOF
    { {globals=globals; classes=classes; main} }
;

class_def:
| CLASS id=IDENT parent=opt_extends BEGIN attr=attrs_decl meth=list(meth_def) END
    { {class_name=id; attributes=attr; methods=meth; parent=parent} }
;

opt_extends:
| /* empty */ { None }
| EXTENDS parent=IDENT { Some parent }
;

var_decl:
| id=IDENT v=opt_var_value { {variable_name=id; variable_type=TVoid; variable_value=v; variable_final=false} }
;

vars_decl:
| final=opt_final VAR t=typ vars=separated_list(COMMA, var_decl) SEMI vars2=vars_decl {
        (List.map (fun v -> {variable_name=v.variable_name; variable_type=t; variable_value=v.variable_value; variable_final=final}) vars) @ vars2
    }
| { [] }
;

attrs_decl:
| final=opt_final ATTRIBUTE t=typ vars=separated_list(COMMA, var_decl) SEMI attrs=attrs_decl {
        (List.map (fun v -> {variable_name=v.variable_name; variable_type=t; variable_value=v.variable_value; variable_final=final}) vars) @ attrs
    }
| { [] }
;

arg_decl:
| final=opt_final t=typ id=IDENT { {variable_name=id; variable_type=t; variable_value=None; variable_final=final} }
;

typ:
| TINT { TInt }
| TBOOL { TBool }
| TSTRING { TString }
| id=IDENT { TClass(id) }
| TVOID { TVoid }
;

opt_final:
| /* empty */ { false }
| FINAL { true }

opt_var_value:
| /* empty */ { None }
| ASSIGN e=expression { Some e}

meth_def:
| METHOD t=typ id=IDENT LPAR args=separated_list(COMMA, arg_decl) RPAR BEGIN var=vars_decl instr=list(instruction) END
    { {method_name=id; code=instr; params=args; locals=var; return=t} }
;

expression:
| n=INT { Int(n) }
| b=BOOL { Bool(b) }
| s=STRING { String(s) }
| THIS { This }
| m=mem { Get(m) }
| op=uop e=expression %prec UOP { Unop(op, e) }
| e1=expression op=bop e2=expression { Binop(op, e1, e2) }
| e=expression INSTANCEOF t=typ { Instanceof(e, t) }
| LPAR e=expression RPAR { e }
| NEW id=IDENT { New(id) }
| NEW id=IDENT LPAR args=separated_list(COMMA, expression) RPAR { NewCstr(id, args) }
| e=expression DOT id=IDENT LPAR expr=separated_list(COMMA, expression) RPAR { MethCall(e, id, expr) }
;

mem:
| id=IDENT { Var(id) }
| e=expression DOT id=IDENT { Field(e, id) }
;

instruction:
| PRINT LPAR args=separated_list(COMMA, expression) RPAR SEMI { Print(args, false) }
| PRINTLN LPAR args=separated_list(COMMA, expression) RPAR SEMI { Print(args, true) }
| m=mem ASSIGN e=expression SEMI { Set(m, e) }
| IF LPAR e1=expression RPAR BEGIN i1=list(instruction) END ELSE BEGIN i2=list(instruction) END
    { If(e1, i1, i2) }
| WHILE LPAR e=expression RPAR BEGIN i=list(instruction) END
    { While(e, i) }
| RETURN e=expression SEMI { Return(e) }
| e=expression SEMI { Expr(e) }
;

uop:
| SUB { Opp } 
| NOT { Not }
;

%inline bop:
| ADD { Add }
| SUB { Sub }
| MUL { Mul }
| DIV { Div }
| MOD { Rem }
| EQ { Eq }
| NEQ { Neq }
| LT { Lt }
| LE { Le }
| GT { Gt }
| GE { Ge }
| AND { And }
| OR { Or }

| SEQ { Seq }
| SNEQ { Sneq }
;