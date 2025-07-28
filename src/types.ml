type ty=(*Here I define the types of expression that my interpreter evaluate*)
TInt|
TBool|
TVar of string|
TFun of ty*ty

type scheme=(*This is used for polymorphism liek in the forall type it binds the variable for any type*)
Simple of ty|
Forall of string list*ty

let rec freevariable_ty(t:ty):string list=(*This function helps us in finding 
freevariables in a type so that we could use them in generalising*)
match t with 
TInt|TBool->[]|
TVar x->[x]|
TFun(t1,t2)->freevariable_ty(t1)@freevariable_ty(t2);;

let freevariables_scheme=(*This one again helps us in finding freevariables for binding and generalising but in a scheme*)
function 
Simple t->freevariable_ty t|
Forall (v,t)->List.filter(fun x->not(List.mem x v))(freevariable_ty t)

let rec stringOf_ty(t:ty):string=(*This function is used to convert our 
types to string for purpose liek repl output...it makes it easier to read*)
match t with
TInt->"int"|
TBool->"bool"|
TVar x->x|
TFun(t1,t2)->let s1=match t1 with 
TFun _->"("^stringOf_ty t1^")"|
_->stringOf_ty t1 in
s1^" -> "^stringOf_ty t2

let string_of_scheme(s:scheme):string=
match s with 
Simple x->stringOf_ty x|
Forall (v,t)->"forall"^String.concat " "v^"."^stringOf_ty t







