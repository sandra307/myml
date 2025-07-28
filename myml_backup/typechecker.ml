open Ast
open Types
open Infer
(*open Env*)
exception TypeError of string

(* Returns the type of an expression *)
let infer_expr  (env : Env.t) (e : expr) : ty =
  let _, t = infer env e in
  t
