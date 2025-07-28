open Ast
open Types
open Eval
open Parser
open Lexer
open Infer

let parse_string (i:string):expr=
try parse(lex i) with
ParseError msg->failwith("ParseError: "^ msg)

let infer_and_eval(env:Env.t)(i:string):string=
let expr=parse_string i in
let ty=try infer_type env expr with 
TypeError msg->failwith("Type error: "^msg)in
let v=eval expr [] in
Printf.sprintf "Type: %s\nResult: %s\n"(stringOf_ty ty)(string_of_value v)







