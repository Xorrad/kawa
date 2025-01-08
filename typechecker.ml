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

  let rec check_subclass class_name parent_name =
    if class_name <> parent_name then
      let c = List.find (fun c -> c.class_name = class_name) p.classes in
      match c.parent with
        | Some p -> check_subclass class_name p
        | None -> error (Printf.sprintf "%s is not a subclass of %s" class_name parent_name)
  in

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
    | Var s ->
      begin
        try Hashtbl.find tenv s
        with Not_found -> error (Printf.sprintf "undefined variable %s" s)
      end
    | Field(e, s) ->
      (* 1. Check if the class is defined *)
      (* 2. Check if class has attribute *)
      (* 3. If not check recursively for parent classes *)
      let rec type_attribute class_name s =
        match List.find_opt (fun c -> c.class_name = class_name) p.classes with
        | Some c ->
          begin
            match List.find_opt (fun (a,t) -> a = s) c.attributes with
            | Some (s, t) -> t
            | None ->
              begin
                match c.parent with
                | Some parent_name -> type_attribute parent_name s
                | None -> error (Printf.sprintf "undefined attribute %s in class %s" s class_name)
              end
          end
        | None -> error (Printf.sprintf "undefined class %s" s)
      in
      match type_expr e tenv with
      | TClass class_name -> type_attribute class_name s
      | t -> error (Printf.sprintf "%s don't have attributes" (typ_to_string t))
      
    | _ -> failwith "case not implemented in type_mem_access"
  in

  let rec check_instr i ret tenv = match i with
    | Print e -> check e TInt tenv
    | Set(mem, e) ->
      begin
        let t1 = type_mem_access mem tenv in
        let t2 = type_expr e tenv in
        match t1, t2 with
        | TClass s1, TClass s2 -> check_subclass s1 s2
        | t1, t2 -> if t1 <> t2 then type_error t1 t2
      end

    | Expr e -> check e TVoid tenv

    | _ -> failwith "case not implemented in check_instr"
  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in

  check_seq p.main TVoid tenv
