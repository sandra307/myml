open Ast
open Types
open Substitution
open Env

(* Counter to generate fresh type variables: 'a, 'b, 'c, ... *)
let counter=ref 0

  let fresh_tyvar ():ty=
let n= !counter in
incr counter;
let letter=Char.chr(int_of_char 'a'+(n mod 26)) in
if n<26 then TVar("'"^String.make 1 letter) else
  TVar("'"^String.make 1 letter ^string_of_int(n/26))

(* Unification algorithm *)
exception TypeError of string
let rec unify(t1:ty)(t2:ty):substitution=
match (t1,t2) with 
TInt,TInt|TBool,TBool->[]|
TVar x,ty|ty,TVar x->if ty=TVar x then [] else if 
  List.mem x(freevariable_ty ty) then raise(TypeError ("Occur check failed: "^x ^"in "^stringOf_ty ty)) else
    [(x,ty)]|
    TFun(i1,o1),TFun(i2,o2)->
      let s1=unify i1 i2 in
      let s2=unify (apply s1 o1)(apply s1 o2) in
      compose s2 s1|
      _->raise(TypeError("Cannot unify "^stringOf_ty t1 ^" and "^stringOf_ty t2))

(* Generalization: convert ty to Forall if needed *)

  let generalise(e:Env.t)(t:ty):scheme=
  let fv_e=freevariable_env e in
  let fv_ty=freevariable_ty t in
  let v=List.filter(fun x->not(List.mem x fv_e))fv_ty in
  if v=[] then Simple t else Forall(v,t)

(* Instantiation: here basically from schemes we get the types...
like if forall schemes we have an identity function..
  where first we use it with int then bool then we actually create new fresh variables 
for both and we add it to substitution and apply them*)
let instantiate(s:scheme):ty=
match s with 
Simple t->t|
Forall(v,t)->apply (List.map(fun x->(x,fresh_tyvar()))v )t



(* The main inference function *)
let rec infer (e:Env.t)(exp:expr):substitution*ty=
match exp with 
Int _->([],TInt)|
Bool _->([],TBool)|
Var x->(match Env.search e x with
Some sc->
  let t=instantiate sc in ([],t)|
  None->raise(TypeError("  Unbound variable: "^x)))|
Fun(i,o)->
  let fv=fresh_tyvar() in
  let new_env=Env.extend e i (Simple fv) in
  let s1,t1=infer new_env o in
  (s1,TFun(apply s1 fv ,t1))|
  App(e1,e2)->
    let s1,t1=infer e e1 in
    let s2,t2=infer(apply_subst s1 e) e2 in
    let fr_ty=fresh_tyvar() in
    let s3=unify (apply s2 t1)(TFun(t2,fr_ty)) in
    let s=compose s3( compose s2 s1) in
    (s,apply s3 fr_ty)|
    Let (x,e1,e2)->
      let s1,t1=infer e e1 in
      let new_env=apply_subst s1 e in
      let sc=generalise new_env t1 in
      let s2,t2=infer (Env.extend new_env x sc) e2 in
      (compose s2 s1 ,t2)|
      If(e1,e2,e3)->
        let s1,t1=infer e e1 in
        let s2=unify t1 TBool in
        let s3,t2=infer (apply_subst s2(apply_subst s1 e))e2 in
        let s4,t3=infer (apply_subst s3(apply_subst s2(apply_subst s1 e)))e3 in
        let s5=unify t2 t3 in
        let s=compose s5(compose s4(compose s3(compose s2 s1))) in 
        (s,apply s5 t2)
       
      
let infer_type  (env : Env.t) (e : expr) : ty =
  let _, t = infer env e in
  t
        




