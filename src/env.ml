(*Environment ,this file is basically like a dictionary
 where we store a variable and their type like the variable for eg x=5..it stores it as int *)
open Types 
open Substitution
module StringMap=Map.Make(String)
type t=scheme StringMap.t
let empty:t=StringMap.empty

let extend (e:t)(x:string)(s:scheme):t=
StringMap.add x s e

let search (e:t)(x:string):scheme option=
StringMap.find_opt x e

let to_map(l:(string*scheme)list):t=
List.fold_left(fun acc (x,s)->extend acc x s )empty l

let freevariable_env(e:t):string list=
StringMap.bindings e|>List.map snd|>List.map freevariables_scheme|>List.flatten|>List.sort_uniq String.compare

let apply_subst (s : substitution) (e : t) : t =
  StringMap.map (apply_scheme s) e
