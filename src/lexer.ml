(*basicallt wwhat lexer does is it convert our instruction and commands to Tokens making it simpler o pass around and use it*)

open Printf
type token =
  | INT of int
  | BOOL of bool
  | IDENT of string
  | LET
  | IN
  | FUN
  | ARROW (* -> *)
  | IF
  | THEN
  | ELSE
  | EQ (* = *)
  | PLUS(* + *)
  | MINUS(* - *)
  | TIMES(* * *)
  | LPAREN(* ( *)
  | RPAREN(* ) *)
  |GT(*>*)
  |LT(*<*)
  |GTE(*>=*)
  |LTE(*<=*)
  |EQEQ(*==,physical equality..like care if 2 variables point to the same memory location*)
  |NEQ(*<>,not equal to*)
  | EOF(*End of file...there will be no more data stream*)
  let is_no x='0'<=x&&x<='9'
  let is_char x=('a'<=x&&x<='z')||('A'<=x&&x<='Z')||x='_'
  let is_whitespace x=x=' '||x='\t'||x='\n'||x='\r'

let lex(ip:string):token list=
let l=String.length ip in
let rec skip i=if i<l&&is_whitespace ip.[i] then skip(i+1) else i in
let rec check_int i acc=if i<l && is_no ip.[i] then check_int(i+1)(acc*10+(Char.code ip.[i]-Char.code '0')) else(INT acc,i) in
let rec check_ident i buff=if i<l && (is_char ip.[i]||is_no ip.[i]) then check_ident(i+1)(Buffer.add_char buff ip.[i];buff)
else (Buffer.contents buff,i)in
let rec loop i acc=let i=skip i in
if i>=l then List.rev(EOF::acc)else
  match ip.[i]with|
  '('->loop(i+1)(LPAREN::acc)|
  ')'->loop(i+1)(RPAREN::acc)|
  '+'->loop(i+1)(PLUS::acc)|
  '-'->if i+1<l&&ip.[i+1]='>' then loop(i+2)(ARROW::acc)else loop(i+1)(MINUS::acc)|
  '*'->loop(i+1)(TIMES::acc)|
 '>'->if (i+1)<l&&ip.[i+1]='=' then loop(i+2)(GTE::acc)else loop(i+1)(GT::acc)|
 '<'->if(i+1)<l&&ip.[i+1]='>' then loop(i+2)(NEQ::acc) else 
  if (i+1)<l&&ip.[i+1]='=' then  loop(i+2)(LTE::acc) else
    loop(i+1)(LT::acc)|
    '='->if(i+1)<l &&ip.[i+1]='=' then loop(i+2)(EQEQ::acc) else
      loop(i+1)(EQ::acc)|
  x when is_no x->
    let t,j=check_int i 0 in loop(j)(t::acc)|
    x when is_char x ->
      let buff=Buffer.create 8 in Buffer.add_char buff x ;
      let id,j=check_ident(i+1)(buff) in
      let tok=match id with 
      "let"->LET|
      "in"->IN|
      "fun"->FUN|
      "true"->BOOL true|
      "false"->BOOL false|
      "if"->IF|
      "else"->ELSE|
      "then"->THEN|
      _->IDENT id in
      loop j (tok::acc)|
      x->failwith(sprintf "UNexpected character in the lexer: %c" x) in loop 0 []
let string_of_token=function|
INT x->"INT(" ^string_of_int x^")"|
BOOL b->"BOOL("^string_of_bool b^")"|
IDENT x->"IDENT("^x^")"|
LET->"LET"|
IN->"IN"|
IF->"IF"|
THEN->"THEN"|
ELSE->"ELSE"|
FUN->"FUN"|
ARROW->"ARROW"|
EQ->"EQ"|
PLUS->"PLUS"|
MINUS->"MINUS"|
TIMES->"TIMES"|
LPAREN->"LPAREN"|
RPAREN->"RPAREN"|
GT->"GREATER THAN"|
LT->"LESS THAN"|
GTE->"GREATER THAN OR EQUAL"|
LTE->"LESS THAN OR EQUAL"|
EQEQ->"PHYSICALLY EQUAL"|
NEQ->"NOT EQUAL"|
EOF->"EOF"


