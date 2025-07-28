type expr=
Var of string|(*type for variables*)
Int of int|(*Integer*)
Bool of bool|(*Boolean*)
Fun of string*expr|(*function like ->fun x=e*)
App of expr*expr|(*function application e1 e2 *)
Let of string*expr*expr|(*let expression like let x=e in f*)
If of expr*expr*expr(*if condition expression*)





