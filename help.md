# PARSER

%ATTRIBUTE, VAR (donner les tokens)
%nonassoc  | - prioritaire
%left      V + prioritaire

attribute_decl:
ATTRIBUTE tid=typed_ident SEMI {tid}

variable_decl:
    typed_ident:
    | t=typ id=INDENT {id, t}

    extension:
    ....

# Type Checker

- vérifier si 1 classe est une sous-classe d'une autre.

- check e typ tenv
  vérifier si le type de e est un sous-type de typ.

- Binop((Add|Null), e1, e2)
  check e1 TInt tenv
  check e2 TInt tenv