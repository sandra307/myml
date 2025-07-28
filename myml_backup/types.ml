type ty =
  | TInt                          (* int *)
  | TBool                         (* bool *)
  | TVar of string                (* type variable: 'a, 'b, etc. *)
  | TFun of ty * ty               (* function type: t1 -> t2 *)

type scheme =
  | Simple of ty
  | Forall of string list * ty    (* ∀ variables. type *)

(*finding free variables in type*)
let rec ftv_ty(t:ty):string list=match t with 
TInt|TBool->[]|
TVar x->[x]|
TFun (t1,t2)->(ftv_ty t1)@(ftv_ty t2);;

(*finding free type variable in scheme*)
let ftv_scheme =function
Simple t->ftv_ty t|
Forall (var,t)->List.filter(fun x->not(List.mem x var))(ftv_ty t)
let rec string_of_ty (t : ty) : string =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TVar x -> x
  | TFun (t1, t2) ->
      let s1 = match t1 with
        | TFun _ -> "(" ^ string_of_ty t1 ^ ")"
        | _ -> string_of_ty t1
      in
      s1 ^ " -> " ^ string_of_ty t2
