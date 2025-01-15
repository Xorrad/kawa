open Kawa

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | Null
and obj = {
  cls:    string;
  fields: (string, var) Hashtbl.t;
}
and var = {
  mutable value: value;
  final: bool;
}

exception Error of string
exception Return of value

let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in
  List.iter (fun v -> Hashtbl.add env v.variable_name {value=Null; final=v.variable_final}) p.globals;

  let find_var env (s: string) (message: string) =
    match Hashtbl.find_opt env s with
    | Some v -> v.value
    | None -> failwith message
  in

  let rec eval_call f this args =
    let rec eval_method class_name =
      match List.find_opt (fun cls -> cls.class_name = class_name) p.classes with
      | Some c ->
        begin
          match List.find_opt (fun m -> m.method_name = f) c.methods with
          | Some m ->
            begin
              let local_env = Hashtbl.copy env in
              Hashtbl.add local_env "this" {value=(VObj this); final=true};
              List.iter2 (fun v1 v2 -> Hashtbl.add local_env v1.variable_name {value=v2; final=v1.variable_final}) m.params args;
              try
                exec_seq m.code local_env;
                Null
              with Return e -> e
            end
          | None ->
            begin
              match c.parent with
              | Some p -> eval_method p
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
          | Var s -> find_var lenv s (Printf.sprintf "undefined variable %s" s)
          | Field(e, s) ->
            let obj = evalo e in
            find_var obj.fields s (Printf.sprintf "undefined field %s" s)
        end
      | This -> find_var lenv "this" "no local environment available"

      | New s ->
        begin
          match List.find_opt (fun c -> c.class_name = s) p.classes with
          | Some c ->
            let class_env =
              match c.parent with
              | Some p -> (evalo (New p)).fields
              | None -> Hashtbl.create (List.length c.attributes)
            in
            List.iter (fun a -> Hashtbl.add class_env a.variable_name {value=Null; final=a.variable_final}) c.attributes;
            VObj { cls = s; fields = class_env }
          | None -> failwith (Printf.sprintf "undefined class %s" s);
        end
      | NewCstr(s, args) ->
        let obj = evalo (New s) in
        let args = List.rev (List.fold_left (fun r e -> eval e :: r) [] args) in
        let _ = eval_call "constructor" obj args in
        VObj obj
      | MethCall(e, s, args) ->
        let args = List.rev (List.fold_left (fun r e -> eval e :: r) [] args) in
        eval_call s (evalo e) args

      | _ -> failwith "case not implemented in eval" [@@ocaml.warning "-11"]
    in
  
    let rec exec (i: instr): unit = match i with
      | Print e -> Printf.printf "%d\n" (evali e)
      | Set(mem, e) ->
        let v = eval e in
        begin
          match mem with
          | Var s ->
            begin
              match Hashtbl.find_opt lenv s with
              | Some var ->
                if var.final && var.value != Null then failwith (Printf.sprintf "final variable %s cannot be modified" s)
                else var.value <- v
              | None -> Hashtbl.add lenv s {value=v; final=false}
            end
          | Field(e, s) ->
            begin
              let obj = evalo e in
              match Hashtbl.find_opt obj.fields s with
              | Some var ->
                if var.final && var.value != Null then failwith (Printf.sprintf "final field %s in class %s cannot be modified" s obj.cls)
                else var.value <- v
              | None -> Hashtbl.add obj.fields s {value=v; final=false}
            end
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
        
      | _ -> failwith "case not implemented in exec" [@@ocaml.warning "-11"]

    and exec_seq s = 
      List.iter exec s
    in

    exec_seq s
  in
  
  exec_seq p.main env
