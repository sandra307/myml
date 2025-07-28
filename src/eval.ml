open Ast

type value=
VInt of int|
VBool of bool|
VClosure of string*expr* env 
and env=(string*value)list

exception RuntimeError of string
(*this is built-in environment with basic feature like +,-,*...and boolean value*)
let builtins:env=[
  ("+",VClosure("x",Fun("y",App(App(Var"+",Var "x"),Var "y")),[]));
  ("*",VClosure("x",Fun("y",App(App(Var"*",Var "x"),Var"y")),[]));
  ("-",VClosure("x",Fun("y",App(App(Var"-",Var "x"),Var"y")),[]));
  ("true",VBool true);
  ("false",VBool false);
]
let rec eval(e:expr)(env:env):value=
match e with 
Int n->VInt n|
Bool b->VBool b|
Var x->(match List.assoc_opt x env with 
Some v->v|
None->(match List.assoc_opt x builtins with 
Some v->v|
None->raise(RuntimeError(" Unbound Variable: "^x))
  ))|
  Fun(p,b)->VClosure(p,b,env)|
  App(App(Var"+",e1),e2)->(
    match eval e1 env,eval e2 env with
    VInt v1,VInt v2->VInt(v1+v2)|
    _->raise(RuntimeError" Expected integer for +")
  )|
App(App(Var"-",e1),e2)->(
  match eval e1 env ,eval e2 env with
  VInt v1,VInt v2->VInt(v1-v2)|
  _->raise(RuntimeError" Expected integer for -")
)|
App(App(Var"*",e1),e2)->(
  match eval e1 env ,eval e2 env with
  VInt v1,VInt v2->VInt(v1*v2)|
  _->raise(RuntimeError" Expected integer for *")
)|
App(App(Var"=",e1),e2)->
 (match eval e1 env ,eval e2 env with 
 VInt v1,VInt v2->VBool(v1=v2)|
 VBool b1,VBool b2->VBool(b1=b2)|
 _->raise(RuntimeError"Type error: Expected integer or boolean"))|
If (e1,e2,e3) ->
  (match eval e1 env with 
  VBool true->eval e2 env|
  VBool false->eval e3 env|
  _->raise(RuntimeError"Expected  boolean condition")
)|
Let(x,e1,e2)->
  let v1=eval e1 env in 
  eval e2 ((x,v1)::env)|
App(e1,e2)->
  let v1=eval e1 env in
  let v2=eval e2 env in
  (match v1 with
  VClosure(p,b,e)->eval b ((p,v2)::e)|
  _->raise(RuntimeError"Expected a function"))


let string_of_value=function
VInt n->string_of_int n|
VBool b->string_of_bool b|
VClosure _->"<fun>"







