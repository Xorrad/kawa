open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in

  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ

  and type_expr e tenv = match e with
    | Int _  -> TInt
    | Bool _  -> TBool
    
    | Unop(Opp, e1) ->
      check e1 TInt tenv;
      TInt
    | Unop(Not, e1) ->
      check e1 TBool tenv;
      TBool

    | Binop((Add | Sub | Mul | Div | Rem), e1, e2) ->
      check e1 TInt tenv;
      check e2 TInt tenv;
      TInt
    | Binop((Lt | Le | Gt | Ge), e1, e2) ->
      check e1 TInt tenv;
      check e2 TInt tenv;
      TBool
    | Binop((Eq | Neq), e1, e2) ->
      check e2 (type_expr e tenv) tenv;
      TBool
    | Binop((And | Or), e1, e2) ->
      check e1 TBool tenv;
      check e2 TBool tenv;
      TBool
    
    | Get mem -> type_mem_access mem tenv
    | This -> type_mem_access (Var "this") tenv

    | New s ->
      let exists = List.exists (fun c -> c.class_name = s) p.classes in
      if not exists then error (Printf.sprintf "undefined class %s" s);
      TClass s
    | NewCstr(s, args) ->
      begin 
        match List.find_opt (fun c -> c.class_name = s) p.classes with
          | Some c ->
            begin
              match List.find_opt (fun m -> m.method_name = s) c.methods with
                | Some m -> List.iter2 (fun (s, t) e -> check e t tenv) m.params args
                | None -> error (Printf.sprintf "undefined constructor for class %s" s);
            end
          | None -> error (Printf.sprintf "undefined class %s" s);
      end;
      TClass s
    (* | MethCall(e, s, args) ->
      begin *)
        


    | _ -> failwith "case not implemented in type_expr"

  and type_mem_access m tenv = match m with
    | _ -> failwith "case not implemented in type_mem_access"
  in

  let rec check_instr i ret tenv = match i with
    | Print e -> check e TInt tenv
    | _ -> failwith "case not implemented in check_instr"
  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in

  check_seq p.main TVoid tenv
