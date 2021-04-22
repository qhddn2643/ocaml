type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp
  | Var of string
  | Lambda of string * exp
  | Apply of exp * exp
  | Let of string * exp * exp
  | TypeError

type environment = (string * exp) list

(*let rec step (env : environment) (e : exp) : (environment * exp) = match e with
  | True -> (env, True)
  | False -> (env, False)
  | If (e, e1, e2) -> let e' = step env e in
                      (match e' with
                      | (env, True) -> step env e1
                      | (env, False) -> step env e2
                      | _ -> env, TypeError)
  | IsZero (e) -> let e' = step env e in
                  (match e' with
                  | (env, Num 0) -> (env, True)
                  | (env, Num n) -> (env, False)
                  | _ -> env, TypeError)
  | Num e -> (env, Num e)
  | Plus(e1, e2) -> let n1 = step env e1 in
                    let n2 = step env e2 in
                    (match n1, n2 with
                    | (env1, Num i), (env2, Num j) -> (env, Num(i+j))
                    | _ -> env, TypeError)
  | Mult(e1, e2) -> let n1 = step env e1 in
                    let n2 = step env e2 in
                    (match n1, n2 with
                    | (env1, Num i), (env2, Num j) -> (env, Num(i*j))
                    | _ -> env, TypeError)
  | Var varname -> (env, Var varname)
  | Lambda(var, body) -> (env, Lambda(var, body))
  | Apply(e1, e2) -> let e1' = step env e1 in
                     (match e1' with
                     | (env, Lambda(var, body)) -> (env, Let(var, body, e2)))
  | Let(var, e1, e2) -> let var = step env e1 in step env e2
  | Let(var, e1, e2) -> let e1' = step env e1 in
                        let e2' = step env e2 in
                        (match e2' with
                        | (env, Var x) -> if x = var then e1' else e2'
                        | _ -> env, TypeError)*)

let rec multi_step (env : environment) (e : exp) : (environment * exp) = match e with
  | True -> (env, True)
  | False -> (env, False)
  | If (e, e1, e2) -> let e' = multi_step env e in
                      (match e' with
                      | (env, True) -> multi_step env e1
                      | (env, False) -> multi_step env e2
                      | _ -> env, TypeError)
  | IsZero (e) -> let e' = multi_step env e in
                  (match e' with
                  | (env, Num 0) -> (env, True)
                  | (env, Num n) -> (env, False)
                  | _ -> env, TypeError)
  | Num e -> (env, Num e)
  | Plus(e1, e2) -> let n1 = multi_step env e1 in
                    let n2 = multi_step env e2 in
                    (match n1, n2 with
                    | (env1, Num i), (env2, Num j) -> (env, Num(i+j))
                    | _ -> env, TypeError)
  | Mult(e1, e2) -> let n1 = multi_step env e1 in
                    let n2 = multi_step env e2 in
                    (match n1, n2 with
                    | (env1, Num i), (env2, Num j) -> (env, Num(i*j))
                    | _ -> env, TypeError)
  | Var varname -> (env, Var varname)
  | Lambda(var, body) -> (env, Lambda(var, body))
  | Apply(e1, e2) -> let e1' = try multi_step env e1 with _ -> (env, TypeError) in
                     (match e1' with
                     | (env, Lambda(var, body)) -> (env, Let(var, body, e2))
                     | _ -> env, TypeError)
  | Let(x, e1, e2) -> match e2 with
                      | Var var -> if var = x then (env, e1) else (env, Var var)
                      | Num n -> (env, Num n)
                      | Plus(n1, Num n) -> (match n1 with 
                                        | (Var var) -> match e1 with 
                                                       | Num i -> if var = x 
                                                                  then if e1 = Num i then (env, Num(i+n)) else (env, TypeError)
                                                                  else (env, Var var))
                      | Mult(n1, Num n) -> (match n1 with 
                                        | (Var var) -> match e1 with
                                                       | Num i -> if var = x 
                                                                  then if e2 = Num i then (env, Num(i*n)) else (env, TypeError)
                                                                  else (env, Var var))
                      | Let(var, n1, n2) -> let var = multi_step env n1 in multi_step env n2
                      | _ -> env, TypeError


(*let rec multi_step (env : environment) (e : exp) : (environment * exp) = match e with
  | True -> step (env, True)
  | False -> step (env, False)
  | If(e, e1, e2) -> step (env, If(e, e1, e2))
  | IsZero(e) -> step (env, IsZero(e))
  | Num(e) -> step (env, Num(e))
  | Plus(e1, e2) -> step (env, Plus(e1, e2))
  | Mult(e1, e2) -> step (env, Mult(e1, e2))
  | Var(var) -> step (env, Var(var))
  | Lambda(var, body) -> step (env, Lambda(var, body))
  | Apply(e1, e2) -> step (env, Apply(e1, e2))
  | Let(var, e1, e2) -> step (env, Let(var, e1, e2))*)


