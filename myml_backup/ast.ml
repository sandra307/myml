type expr =
  | Var of string                      (* variables *)
  | Int of int                         (* integers *)
  | Bool of bool                       (* booleans *)
  | Fun of string * expr              (* function abstraction: fun x -> e *)
  | App of expr * expr                (* function application: e1 e2 *)
  | Let of string * expr * expr       (* let-binding: let x = e1 in e2 *)
  | If of expr * expr * expr          (* conditionals: if e1 then e2 else e3 *)
  | LetRec of string * expr * expr    (* recursive let-binding *)
