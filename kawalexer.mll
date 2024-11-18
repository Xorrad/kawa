{

  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "print",    PRINT;
      "main",     MAIN;
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

  | "-" { MINUS }
  | "!" { EXCLA }
  | "+" { PLUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "%" { PERCENT }
  | "=" { EQUAL }
  | "<" { LESS }
  | ">" { MORE }
  | "&" { AND }
  | "|" { VBAR }

  | "int" { TYP_INT }
  | "bool" { TYP_BOOL }
  | "void" { TYP_VOID }

  | "var" { VAR }
  | "attribute" { ATTRIBUTE }
  | "class" { CLASS }
  | "method" { METHOD }

  | "true" { TRUE }
  | "false" { FALSE }

  | "this" { FALSE }
  | "new" { FALSE }

  | "print" { PRINT }
  | "if" { PRINT }
  | "else" { PRINT }
  | "while" { WHILE }
  | "return" { RETURN }

  | number as n  { INT(int_of_string n) }
  | ident as id  { keyword_or_ident id }

  | ";"  { SEMI }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | "."  { DOT }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
