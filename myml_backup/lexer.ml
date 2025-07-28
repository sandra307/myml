(** src/lexer.ml *)

open Printf

(* ------------------------------------------------------------------ *)
(* Token definition — adjust to match the one your parser expects      *)
(* ------------------------------------------------------------------ *)
type token =
  | INT    of int
  | BOOL   of bool
  | IDENT  of string
  | LET
  | REC
  | IN
  | FUN
  | ARROW          (* -> *)
  | IF
  | THEN
  | ELSE
  | EQ             (* = *)
  | PLUS           (* + *)
  | MINUS          (* - *)
  | TIMES          (* * *)
  | LPAREN         (* ( *)
  | RPAREN         (* ) *)
  | EOF
  
(* ------------------------------------------------------------------ *)
(* Utility predicates                                                  *)
(* ------------------------------------------------------------------ *)
let is_digit c = '0' <= c && c <= '9'
let is_letter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'
let is_whitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

(* ------------------------------------------------------------------ *)
(* Lexer: string  -> token list                                        *)
(* ------------------------------------------------------------------ *)
let lex (input : string) : token list =
  let len = String.length input in
  let rec skip i =
    if i < len && is_whitespace input.[i] then skip (i + 1) else i
  in
  let rec read_int i acc =
    if i < len && is_digit input.[i] then
      read_int (i + 1) (acc * 10 + (Char.code input.[i] - Char.code '0'))
    else
      (INT acc, i)
  in
  let rec read_ident i buf =
    if i < len && (is_letter input.[i] || is_digit input.[i]) then
      read_ident (i + 1) (Buffer.add_char buf input.[i]; buf)
    else
      (Buffer.contents buf, i)
  in
  let rec loop i acc =
    let i = skip i in
    if i >= len then List.rev (EOF :: acc)
    else
      match input.[i] with
      | '(' -> loop (i + 1) (LPAREN :: acc)
      | ')' -> loop (i + 1) (RPAREN :: acc)
      | '+' -> loop (i + 1) (PLUS  :: acc)
      | '-' ->
          if i + 1 < len && input.[i + 1] = '>' then
            loop (i + 2) (ARROW :: acc)
          else
            loop (i + 1) (MINUS :: acc)
      | '*' -> loop (i + 1) (TIMES :: acc)
      | '=' -> loop (i + 1) (EQ :: acc)
      | c when is_digit c ->
          let tok, j = read_int i 0 in
          loop j (tok :: acc)
      | c when is_letter c ->
          let buf = Buffer.create 8 in
          Buffer.add_char buf c;
          let ident, j = read_ident (i + 1) buf in
          let tok =
            match ident with
            | "let"  -> LET
            | "rec"  -> REC
            | "in"   -> IN
            | "fun"  -> FUN
            | "true" -> BOOL true
            | "false"-> BOOL false
            | "if"   -> IF
            | "then" -> THEN
            | "else" -> ELSE
            | _      -> IDENT ident
          in
          loop j (tok :: acc)
      | c ->
          failwith (sprintf "Unexpected character in lexer: '%c'" c)
  in
  loop 0 []
let string_of_token = function
  | INT n -> "INT(" ^ string_of_int n ^ ")"
  | BOOL b -> "BOOL(" ^ string_of_bool b ^ ")"
  | IDENT s -> "IDENT(" ^ s ^ ")"
  | LET -> "LET"
  | REC -> "REC"
  | IN -> "IN"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | FUN -> "FUN"
  | ARROW -> "ARROW"
  | EQ -> "EQ"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | EOF -> "EOF"

