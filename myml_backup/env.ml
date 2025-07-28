open Types  (* we’ll define this in dune soon *)
open Substitution
module StringMap = Map.Make(String)

type t = scheme StringMap.t

let empty : t = StringMap.empty

let extend (env : t) (x : string) (s : scheme) : t =
  StringMap.add x s env

let lookup (env : t) (x : string) : scheme option =
  StringMap.find_opt x env

let of_list (lst : (string * scheme) list) : t =
  List.fold_left (fun acc (x, s) -> extend acc x s) empty lst
let ftv_env (env : t) : string list =
  StringMap.bindings env
  |> List.map snd
  |> List.map ftv_scheme
  |> List.flatten
  |> List.sort_uniq String.compare
let apply_subst (s : substitution) (env : t) : t =
  StringMap.map (apply_scheme s) env