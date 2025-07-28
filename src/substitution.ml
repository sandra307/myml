open Types
module StringMap=Map.Make(String)
type substitution=(string*ty)list

let rec apply(s:substitution)(t:ty):ty=(*ok...so this apply function it applies substitution on unknown variables
 like if we have in s ->[x,TInt] and then we have a variable x...
  we will look at s first and match x with Int..so we get x as TInt *)
match t with 
TInt|TBool->t|
TVar x->(match List.assoc_opt x s with 
Some t'->apply s t'|
None->TVar x)|
TFun(x1,x2)->TFun(apply s x1,apply s x2)

let compose (s1:substitution) (s2:substitution)=(*In compose...if we have 2 substitution 
  which is just mapping of variables with their types..then compose merges the substitution...
but only after updating the older substitution with newer ones*)
let s2_new=List.map(fun(x,t)->(x,apply s1 t))s2 in
s2_new@s1

let rec ftvariable(t:ty):string list=(*This function helps us get those free varaible
s in a type like those whose types are still not bonded in the substitution map...we gat a list of tthem*)
match t with 
TInt|TBool->[]|
TVar x->[x]|
TFun(x1,x2)->ftvariable x1@ftvariable x2

let freety_sub(s:substitution):string list=(*This helps us get freetype variables in the substitution...
from what we saw...sometimes there are non substituted variables in substitution itself*)
s|>List.map snd|>List.map ftvariable|>List.flatten|>List.sort_uniq String.compare

let apply_scheme(s:substitution)(sc:scheme):scheme=(*here we apply substoitutions to schemes instead of types*)
match sc with 
Simple x->Simple(apply s x)|
Forall(v,t)->let filt_sub=List.filter(fun(x,_)->not(List.mem x v))s in Forall(v,apply filt_sub t)

