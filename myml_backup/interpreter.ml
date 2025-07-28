open Ast
open Types
open Eval
open Typechecker
(*open Parser
open Lexer*)

let parse_string (input : string) : expr =
  try Parser.parse (Lexer.lex input)
  with
  | Parser.ParseError msg -> failwith ("Parser error: " ^ msg)

let infer_and_eval (env : Env.t) (input : string) : string =
  let expr = parse_string input in
  let ty =
    try infer_expr env expr
    with TypeError msg -> failwith ("Type error: " ^ msg)
  in
  let value = eval expr [] in
  Printf.sprintf "Type: %s\nResult: %s\n"
    (string_of_ty ty)
    (string_of_value value)



