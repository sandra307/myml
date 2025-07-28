open Ast

type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * expr * env

and env = (string * value) list

exception RuntimeError of string

(* Built-in environment with "+" "-" "*" as curried OCaml functions *)
let builtins : env = [
  ("+", VClosure ("x", Fun ("y", App (App (Var "+", Var "x"), Var "y")), []));
  ("-", VClosure ("x", Fun ("y", App (App (Var "-", Var "x"), Var "y")), []));
  ("*", VClosure ("x", Fun ("y", App (App (Var "*", Var "x"), Var "y")), []));
  ("true", VBool true);
  ("false", VBool false)
]

let rec eval (e : expr) (env : env) : value =
  match e with
  | Int n -> VInt n
  | Bool b -> VBool b

  | Var x -> (
      match List.assoc_opt x env with
      | Some v -> v
      | None -> (
          match List.assoc_opt x builtins with
          | Some v -> v
          | None -> raise (RuntimeError ("Unbound variable: " ^ x))
        )
    )

  | Fun (param, body) -> VClosure (param, body, env)

  | App (App (Var "+", e1), e2) ->
      (match eval e1 env, eval e2 env with
       | VInt v1, VInt v2 -> VInt (v1 + v2)
       | _ -> raise (RuntimeError "Expected integers for +"))

  | App (App (Var "-", e1), e2) ->
      (match eval e1 env, eval e2 env with
       | VInt v1, VInt v2 -> VInt (v1 - v2)
       | _ -> raise (RuntimeError "Expected integers for -"))

  | App (App (Var "*", e1), e2) ->
      (match eval e1 env, eval e2 env with
       | VInt v1, VInt v2 -> VInt (v1 * v2)
       | _ -> raise (RuntimeError "Expected integers for *"))
  | App (App (Var "=", e1a), e1b) ->
    let v1 = eval e1a env in
    let v2 = eval e1b env in
    begin match v1, v2 with
    | VInt a, VInt b -> VBool (a = b)
    | _ -> raise (RuntimeError "Type error: = expects integers")
    end


  | App (e1, e2) ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      (match v1 with
       | VClosure (param, body, closure_env) ->
           eval body ((param, v2) :: closure_env)
       | _ -> raise (RuntimeError "Expected a function"))

  | Let (x, e1, e2) ->
      let v1 = eval e1 env in
      eval e2 ((x, v1) :: env)

  | LetRec (f, Fun (param, body), e2) ->
      let rec_env = ref [] in
      let closure = VClosure (param, body, (f, VClosure (param, body, !rec_env)) :: env) in
      rec_env := (f, closure) :: env;
      eval e2 !rec_env

  | LetRec (_, _, _) ->
      raise (RuntimeError "LetRec must bind to a function")

  | If (e1, e2, e3) ->
      match eval e1 env with
      | VBool true -> eval e2 env
      | VBool false -> eval e3 env
      | _ -> raise (RuntimeError "Expected a boolean condition")

let string_of_value = function
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VClosure _ -> "<fun>"
