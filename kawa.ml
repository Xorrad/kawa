(**
   Kawa : un petit langage à objets inspiré de Java
 *)

(* Types déclarés pour les attributs, pour les variables, et pour les 
   paramètres et résultats des méthodes. *)
type typ =
  | TVoid
  | TInt
  | TBool
  | TString
  | TClass of string

let typ_to_string = function
  | TVoid -> "void"
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TClass c -> c

type unop =
  | Opp
  | Not

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Neq
  | And
  | Or
  | Seq
  | Sneq

(* Expressions *)
type expr =
  (* Base arithmétique *)
  | Int    of int
  | Bool   of bool
  | String of string
  | Unop   of unop * expr
  | Binop  of binop * expr * expr
  (* Accès à une variable ou un attribut *)
  | Get      of mem_access
  (* Objet courant *)
  | This
  (* Création d'un nouvel objet *)
  | New      of string
  | NewCstr  of string * expr list
  (* Appel de méthode *)
  | MethCall of expr * string * expr list
  (* Test de type dynamique *)
  | Instanceof  of expr * typ

(* Accès mémoire : variable ou attribut d'un objet *)
and mem_access =
  | Var   of string
  | Field of expr (* objet *) * string (* nom d'un attribut *)

(* Instructions *)
type instr =
  (* Affichage de plusieurs expressions avec nouvelle ligne ou non*)
  | Print  of expr list * bool
  (* Écriture dans une variable ou un attribut *)
  | Set    of mem_access * expr
  (* Structures de contrôle usuelles *)
  | If     of expr * seq * seq
  | While  of expr * seq
  (* Fin d'une fonction *)
  | Return of expr
  (* Expression utilisée comme instruction *)
  | Expr   of expr

and seq = instr list

(* Définition d'une variable/attribut 

   Syntaxe : [final] var <nom>;
        ou : [final] attribute <nom>;
*)
type variable_def = {
  variable_name: string;
  variable_type: typ;
  variable_value: expr option;
  variable_final: bool;
}

(* Définition de méthode 

   Syntaxe : method <type de retour> <nom> (<params>) { ... }

   Le corps de la méthode est similaire au corps d'une fonction. *)
type method_def = {
  method_name: string;
  code: seq;
  params: variable_def list;
  locals: variable_def list;
  return: typ;
}
        
(* Définition de classe 

   Syntaxe : class <nom de la classe> { ... }
        ou : class <nom de la classe> extends <nom de la classe mère> { ... }

   On considère que toute classe C contient une définition de méthode de nom
   "constructor" et de type de retour void, qui initialise les champs du 
   paramètre implicite this. *)
type class_def = {
  class_name: string;
  attributes: variable_def list;
  methods: method_def list;
  parent: string option;
}

(* Programme complet : variables globales, classes, et une séquence 
   d'instructions *)
type program = {
  globals: variable_def list;
  classes: class_def list;
  main: seq;
}
