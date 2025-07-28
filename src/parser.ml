open Lexer
open Ast

exception ParseError of string


let peek=function
[]->EOF|
t::_->t

let remain=function
[]->raise(ParseError" Unexpected end of input token list")|
_::t->t

let choosen_one expected tokens=match tokens with
h::t when h=expected->t|
_->raise(ParseError("Expected "^string_of_token expected))

let rec parse_e tokens=
parse_first tokens and 
parse_first tokens=match peek tokens with
LET->let tok=remain tokens in begin match  tok with 
IDENT x::EQ::rest->
  let e1,rest1=parse_e rest in begin match rest1 with
  IN::rest2->let e2,rest3=parse_e rest2 in(Let(x,e1,e2),rest3)
  |_->raise(ParseError" Expected in")
end
|_->raise(ParseError"Let expression not right")
end|
 IF ->
   let token1 = remain tokens in
   let cond, tokens2 = parse_e token1 in
   let tokens3 = choosen_one THEN tokens2 in
   let e_then, tokens4 = parse_e tokens3 in 
   let tokens5 = choosen_one ELSE tokens4 in
   let e_else, tokens6 = parse_e tokens5 in
   (If(cond, e_then, e_else), tokens6)|
   FUN ->
    let tokens=remain tokens in begin match tokens with
    IDENT x::ARROW::rest->let body,rest1=parse_e rest in (Fun(x,body),rest1)|
    _->raise(ParseError "Function not structed in the right way")
   end
   |_->parse_cmp tokens
   (*we compare in this function*)
   and parse_cmp tokens=
   let lhs,tokens=parse_add tokens in
   match peek tokens with 
   EQ->let tokens=remain tokens in 
    let rhs,tokens=parse_add tokens in
    (App(App(Var "=",lhs),rhs),tokens)|
    _->(lhs,tokens)


(*Now...this function down here is for parsing addition*)
and parse_add tokens=
let lhs,tokens=parse_mul tokens in
let rec loop lhs tokens=match peek tokens with
PLUS->let tokens=remain tokens in
let rhs,tokens=parse_mul tokens in
loop(App(App(Var "+",lhs),rhs))tokens |
MINUS->let tokens=remain tokens in 
let rhs,tokens=parse_mul tokens in
loop(App(App(Var "-",lhs),rhs)) tokens |
_->(lhs,tokens) in
loop lhs tokens



  (*now...we check for multiplication of  nos*)
and parse_mul tokens=
let lhs,tokens=parse_app tokens in 
let rec loop lhs tokens =
  match peek tokens with 
  TIMES->
    let tokens=remain tokens in
  let rhs,tokens=parse_app tokens in
    loop(App(App(Var "*",lhs),rhs)) tokens |
    _->(lhs,tokens) in
    loop lhs tokens
    




(*Here we apply functions to variables...like((f x)y)...after parsing it becomes(App(App (f x)y)*)
and parse_app tokens=
let lhs,tokens=parse_base tokens in
let rec loop lhs tokens=
match peek tokens with
INT _|BOOL _|IDENT _|LPAREN->
let rhs,tokens=parse_base tokens in 
loop(App(lhs,rhs))tokens|
_->(lhs,tokens) in
loop lhs tokens





(* here...the basic elements of the expression...which cant be broken again are parsed*)
and parse_base tokens=match peek tokens with 
INT n->(Int n,remain tokens)|
BOOL b->(Bool b,remain tokens)|
IDENT x->(Var x,remain tokens)|
LPAREN->let tokens=remain tokens in
let e1,tokens=parse_e tokens in
let tokens=choosen_one RPAREN tokens in
(e1,tokens)
|_->raise (ParseError "Unexpected tokens in the expression")



let parse (tokens : token list) : expr =
  let e, rest = parse_e tokens in
  match rest with
  | [] | [EOF] -> e
  | _ -> raise (ParseError "Extra tokens after valid expression")


