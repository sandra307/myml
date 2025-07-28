open Lexer
open Ast

exception ParseError of string

type token_stream = token list

let peek = function
  | [] -> EOF
  | t :: _ -> t

let consume = function
  | [] -> raise (ParseError "Unexpected end of input")
  | _ :: rest -> rest

let expect expected_token tokens =
  match tokens with
  | t :: rest when t = expected_token -> rest
  | _ -> raise (ParseError ("Expected " ^ string_of_token expected_token))

let rec parse_expr tokens =
  parse_let tokens

and parse_let tokens =
  match peek tokens with
  | LET ->
      let tokens = consume tokens in
      let is_rec, tokens =
        match tokens with
        | REC :: rest -> (true, rest)
        | _ -> (false, tokens)
      in
      begin match tokens with
      | IDENT x :: EQ :: rest ->
          let e1, rest1 = parse_expr rest in
          begin match rest1 with
          | IN :: rest2 ->
              let e2, rest3 = parse_expr rest2 in
              let let_expr = if is_rec then LetRec (x, e1, e2) else Let (x, e1, e2) in
              (let_expr, rest3)
          | _ -> raise (ParseError "Expected 'in'")
          end
      | _ -> raise (ParseError "Malformed let expression")
      end
  | IF ->
      let tokens = consume tokens in
      let cond, tokens = parse_expr tokens in
      let tokens = expect THEN tokens in
      let e_then, tokens = parse_expr tokens in
      let tokens = expect ELSE tokens in
      let e_else, tokens = parse_expr tokens in
      (If (cond, e_then, e_else), tokens)
  | FUN ->
      let tokens = consume tokens in
      begin match tokens with
      | IDENT x :: ARROW :: rest ->
          let body, rest1 = parse_expr rest in
          (Fun (x, body), rest1)
      | _ -> raise (ParseError "Malformed function")
      end
  | _ -> parse_cmp tokens

(* === comparison === *)
and parse_cmp tokens =
  let lhs, tokens = parse_add tokens in
  match peek tokens with
  | EQ ->
      let tokens = consume tokens in
      let rhs, tokens = parse_add tokens in
      (App (App (Var "=", lhs), rhs), tokens)
  | _ -> (lhs, tokens)

(* === addition/subtraction === *)
and parse_add tokens =
  let lhs, tokens = parse_mul tokens in
  parse_add_tail lhs tokens

and parse_add_tail lhs tokens =
  match peek tokens with
  | PLUS ->
      let tokens = consume tokens in
      let rhs, tokens = parse_mul tokens in
      parse_add_tail (App (App (Var "+", lhs), rhs)) tokens
  | MINUS ->
      let tokens = consume tokens in
      let rhs, tokens = parse_mul tokens in
      parse_add_tail (App (App (Var "-", lhs), rhs)) tokens
  | _ -> (lhs, tokens)

(* === multiplication === *)
and parse_mul tokens =
  let lhs, tokens = parse_app tokens in
  parse_mul_tail lhs tokens

and parse_mul_tail lhs tokens =
  match peek tokens with
  | TIMES ->
      let tokens = consume tokens in
      let rhs, tokens = parse_app tokens in
      parse_mul_tail (App (App (Var "*", lhs), rhs)) tokens
  | _ -> (lhs, tokens)

(* === function application === *)
and parse_app tokens =
  let lhs, tokens = parse_atom tokens in
  parse_app_tail lhs tokens

and parse_app_tail lhs tokens =
  match peek tokens with
  | INT _ | BOOL _ | IDENT _ | LPAREN ->
      let rhs, tokens = parse_atom tokens in
      parse_app_tail (App (lhs, rhs)) tokens
  | _ -> (lhs, tokens)

(* === atoms: literals, variables, parenthesized === *)
and parse_atom tokens =
  match peek tokens with
  | INT n -> (Int n, consume tokens)
  | BOOL b -> (Bool b, consume tokens)
  | IDENT x -> (Var x, consume tokens)
  | LPAREN ->
      let tokens = consume tokens in
      let e, tokens = parse_expr tokens in
      let tokens = expect RPAREN tokens in
      (e, tokens)
  | _ -> raise (ParseError "Unexpected token in expression")

let parse (tokens : token list) : expr =
  let e, rest = parse_expr tokens in
  match rest with
  | [] | [EOF] -> e
  | _ -> raise (ParseError "Extra tokens after valid expression")


