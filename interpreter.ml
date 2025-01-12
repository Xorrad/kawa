open Kawa

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | Null
and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value

let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;
  
  let rec eval_call f this args =
    let rec eval_method class_name =
      match List.find_opt (fun cls -> cls.class_name = class_name) p.classes with
      | Some c -> begin
        match List.find_opt (fun m -> m.method_name = f) c.methods with
        | Some m ->
          begin
            let local_env = Hashtbl.copy env in
            Hashtbl.add local_env "this" (VObj this);
            List.iter2 (fun (s, t) v -> Hashtbl.add local_env s v) m.params args;
            try
              exec_seq m.code local_env;
              Null
            with Return e -> e
          end
        | None ->
          begin
            match c.parent with
            | Some x -> eval_method x
            | None -> failwith (Printf.sprintf "undefined method %s in class %s" f class_name)
          end
        end
      | None -> failwith (Printf.sprintf "undefined class %s" class_name) in
    eval_method this.cls

  and exec_seq s lenv =
    let rec evali e = match eval e with
      | VInt n -> n
      | _ -> assert false
    and evalb e = match eval e with
      | VBool b -> b
      | _ -> assert false
    and evalo e = match eval e with
      | VObj o -> o
      | _ -> assert false
        
    and eval (e: expr): value = match e with
      | Int n  -> VInt n
      | Bool b  -> VBool b

      | Unop(Opp, e1) -> VInt (- evali e1)
      | Unop(Not, e1) -> VBool (not (evalb e1))

      | Binop(Add, e1, e2) -> VInt (evali e1 + evali e2)
      | Binop(Sub, e1, e2) -> VInt (evali e1 - evali e2)
      | Binop(Mul, e1, e2) -> VInt (evali e1 * evali e2)
      | Binop(Div, e1, e2) -> VInt (evali e1 / evali e2)
      | Binop(Rem, e1, e2) -> VInt (evali e1 mod evali e2)
      | Binop(Lt, e1, e2) -> VBool (evali e1 < evali e2)
      | Binop(Le, e1, e2) -> VBool (evali e1 <= evali e2)
      | Binop(Gt, e1, e2) -> VBool (evali e1 > evali e2)
      | Binop(Ge, e1, e2) -> VBool (evali e1 >= evali e2)
      | Binop(Eq, e1, e2) -> VBool (evali e1 == evali e2)
      | Binop(Neq, e1, e2) -> VBool (evali e1 != evali e2)
      | Binop(And, e1, e2) -> VBool (evalb e1 && evalb e2)
      | Binop(Or, e1, e2) -> VBool (evalb e1 || evalb e2)

      | Get mem ->
        begin
          match mem with
          | Var s -> (Hashtbl.find lenv s)
          | Field(e, s) ->
            let obj = evalo e in
            (Hashtbl.find obj.fields s)
        end
      | This -> (Hashtbl.find lenv "this")

      | New s ->
        begin
          match List.find_opt (fun c -> c.class_name = s) p.classes with
          | Some c ->
            let class_env =
              match c.parent with
              | Some x -> (evalo (New x)).fields
              | None -> Hashtbl.create (List.length c.attributes)
            in
            List.iter (fun (a, v) -> Hashtbl.add class_env a Null) c.attributes;
            VObj { cls = s; fields = class_env }
          | None -> failwith (Printf.sprintf "undefined class %s" s);
        end
      | NewCstr(s, args) ->
        let obj = evalo (New s) in
        let args = List.rev (List.fold_left (fun r e -> eval e :: r) [] args) in
        let _ = eval_call s obj args in
        VObj obj
      | MethCall(e, s, args) ->
        let args = List.rev (List.fold_left (fun r e -> eval e :: r) [] args) in
        eval_call s (evalo e) args

      | _ -> failwith "case not implemented in eval"
    in
  
    let rec exec (i: instr): unit = match i with
      | Print e -> Printf.printf "%d\n" (evali e)
      | Set(mem, e) ->
        begin
          let v = eval e in
          match mem with
            | Var s -> (Hashtbl.add lenv s v)
            | Field(e, s) ->
              let obj = evalo e in
              (Hashtbl.add obj.fields s v)
        end
      | If(pred, seq1, seq2) ->
        if evalb pred then exec_seq seq1
        else exec_seq seq2
      | While(pred, seq1) ->
        let rec exec_while pred seq1 =
          if evalb pred then begin
            exec_seq seq1;
            exec_while pred seq1
          end
        in exec_while pred seq1
      | Return e ->
        raise (Return (eval e))
      | Expr e ->
        let _ = eval e in ()
        
      | _ -> failwith "case not implemented in exec"

    and exec_seq s = 
      List.iter exec s
    in

    exec_seq s
  in
  
  exec_seq p.main (Hashtbl.create 1)
