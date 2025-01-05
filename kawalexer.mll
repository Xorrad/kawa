{

  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
    let h = Hashtbl.create 18 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [
        "main", MAIN;
        "true", BOOL(true);
        "false", BOOL(false);
        "var", VAR;
        "attribute", ATTRIBUTE;
        "method", METHOD;
        "class", CLASS;
        "new",  NEW;
        "this", THIS;
        "if", IF;
        "else", ELSE;
        "while", WHILE;
        "return", RETURN;
        "print", PRINT;
        "int", TINT;
        "bool", TBOOL;
        "void", TVOID;
        "extends", EXTENDS;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
}

let digit = ['0'-'9']
let number = ['-']? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf; token lexbuf }

  | "=" { ASSIGN }
  | "-" { SUB }
  | "!" { NOT }
  | "+" { ADD }
  | "*" { MUL }
  | "/" { DIV }
  | "%" { MOD }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<" { LT }
  | "<=" { LE }
  | ">" { GT }
  | ">=" { GE }
  | "&&" { AND }
  | "||" { OR }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | ";"  { SEMI }
  | "."  { DOT }
  | ","  { COMMA }

  | number as n  { INT(int_of_string n) }
  | ident as id  { keyword_or_ident id }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
