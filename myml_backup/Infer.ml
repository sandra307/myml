open Ast
open Types
open Substitution
open Env

(* Counter to generate fresh type variables: 'a, 'b, 'c, ... *)
let counter = ref 0

let fresh_tyvar () : ty =
  let n = !counter in
  incr counter;
  let letter = Char.chr (int_of_char 'a' + (n mod 26)) in
  if n < 26 then TVar ("'" ^ String.make 1 letter)
  else TVar ("'" ^ String.make 1 letter ^ string_of_int (n / 26))

(* Unification algorithm *)
exception TypeError of string

let rec unify (t1 : ty) (t2 : ty) : substitution =
  match (t1, t2) with
  | TInt, TInt | TBool, TBool -> []
  | TVar x, ty | ty, TVar x ->
      if ty = TVar x then []
      else if List.mem x (ftv_ty ty) then
        raise (TypeError ("Occurs check failed: " ^ x ^ " in " ^ string_of_ty ty))
      else [(x, ty)]
  | TFun (l1, r1), TFun (l2, r2) ->
      let s1 = unify l1 l2 in
      let s2 = unify (apply s1 r1) (apply s1 r2) in
      compose s2 s1
  | _ ->
      raise (TypeError ("Cannot unify " ^ string_of_ty t1 ^ " and " ^ string_of_ty t2))

(* Generalization: convert ty to Forall if needed *)
let generalize (env : Env.t) (t : ty) : scheme =
  let env_fvs = ftv_env env in
  let t_fvs = ftv_ty t in
  let vars = List.filter (fun x -> not (List.mem x env_fvs)) t_fvs in
  if vars = [] then Simple t else Forall (vars, t)

(* Instantiation: Forall to ty with fresh vars *)
let instantiate (sch : scheme) : ty =
  match sch with
  | Simple t -> t
  | Forall (vars, t) ->
      let fresh_map = List.map (fun x -> (x, fresh_tyvar ())) vars in
      let subs = List.map (fun (x, t) -> (x, t)) fresh_map in
      apply subs t

(* The main inference function *)
let rec infer (env : Env.t) (e : expr) : substitution * ty =
  match e with
  | Int _ -> ([], TInt)
  | Bool _ -> ([], TBool)
  | Var x ->
      (match Env.lookup env x with
       | Some sch ->
           let t = instantiate sch in
           ([], t)
       | None -> raise (TypeError ("Unbound variable: " ^ x)))
  | Fun (x, e1) ->
      let tv = fresh_tyvar () in
      let new_env = Env.extend env x (Simple tv) in
      let s1, t1 = infer new_env e1 in
      (s1, TFun (apply s1 tv, t1))
  | App (e1, e2) ->
      let s1, t1 = infer env e1 in
      let s2, t2 = infer (apply_subst s1 env) e2 in
      let tv = fresh_tyvar () in
      let s3 = unify (apply s2 t1) (TFun (t2, tv)) in
      let s = compose s3 (compose s2 s1) in
      (s, apply s3 tv)
  | Let (x, e1, e2) ->
      let s1, t1 = infer env e1 in
      let env' = apply_subst s1 env in
      let sch = generalize env' t1 in
      let s2, t2 = infer (Env.extend env' x sch) e2 in
      (compose s2 s1, t2)
 | LetRec (f, Fun (x, e1), e2) ->
    let tv1 = fresh_tyvar () in
    let tv2 = fresh_tyvar () in
    let func_type = TFun (tv1, tv2) in
    let env1 = Env.extend env f (Simple func_type) in
    let env2 = Env.extend env1 x (Simple tv1) in
    let s1, t1 = infer env2 e1 in
    let s2 = unify (apply s1 func_type) t1 in
    let final_env = apply_subst (compose s2 s1) env in
    let sch = generalize final_env (apply s2 t1) in
    let s3, t3 = infer (Env.extend final_env f sch) e2 in
    (compose s3 (compose s2 s1), t3)|
     LetRec (_, _, _) ->
    raise (TypeError "LetRec must bind a function")

  
  | If (e1, e2, e3) ->
      let s1, t1 = infer env e1 in
      let s2 = unify t1 TBool in
      let s3, t2 = infer (apply_subst s2 (apply_subst s1 env)) e2 in
      let s4, t3 = infer (apply_subst s3 (apply_subst s2 (apply_subst s1 env))) e3 in
      let s5 = unify t2 t3 in
      let s = compose s5 (compose s4 (compose s3 (compose s2 s1))) in
      (s, apply s5 t2)