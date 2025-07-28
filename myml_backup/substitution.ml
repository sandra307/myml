module StringMap = Map.Make(String)
open Types
type substitution=(string * ty )list;;
(*applying substitutioon to various variables*)
let rec apply (subs:substitution)(t:ty):ty=
match t with 
TInt|TBool->t|
TVar x->(match List.assoc_opt x subs with
Some t'->apply subs t'|
None->TVar x)|
TFun (t1,t2)->TFun(apply subs t1,apply subs t2);;
(*composinng a new substitutioon and updating the older one and then appending the newer one
s1 which is the newer substitution overrides the older updated s2 substitution,
hence in case of duplicatin gets us the new subs*)
let compose (s1:substitution)(s2:substitution)=
let s2_sub=List.map(fun(x,t)->(x,apply s1 t))s2 in
s2_sub@s1;;
(*gives us freevariables*)
let rec ftv_ty(t:ty):string list=match t with
TInt|TBool->[]|
TVar x->[x]|
TFun(t1,t2)->ftv_ty t1@ftv_ty t2;;

(*getting free type variables from substitution*)
let free_tyvars(s:substitution):string list=
s
|>List.map snd
|>List.map ftv_ty
|>List.flatten
|>List.sort_uniq String.compare

let apply_scheme (subs : substitution) (sch : scheme) : scheme =
  match sch with
  | Simple t -> Simple (apply subs t)
  | Forall (vars, t) ->
      let filtered_subs =
        List.filter (fun (x, _) -> not (List.mem x vars)) subs
      in
      Forall (vars, apply filtered_subs t)