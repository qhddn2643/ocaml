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


let rec multi_step (env : environment) (e : exp) : (environment * exp) = match e with
  | True -> (env, True)
  | False -> (env, False)
  | If (e, e1, e2) -> let e' = try multi_step env e with _ -> (env, TypeError) in
                      (match (try e' with _ -> (env, TypeError)) with
                      | (env, True) -> multi_step env e1
                      | (env, False) -> multi_step env e2
                      | _ -> env, TypeError)
  | IsZero (e) -> let e' = try multi_step env e with _ -> (env, TypeError) in
                  (match (try e' with _ -> (env, TypeError)) with
                  | (env, Num 0) -> (env, True)
                  | (env, Num n) -> (env, False)
                  | _ -> env, TypeError)
  | Num e -> (env, Num e)
  | Plus(e1, e2) -> let n1 = try multi_step env e1 with _ -> (env, TypeError) in
                    (match (try n1 with _ -> (env, TypeError)) with
                    | (env, Num i) -> let n2 = try multi_step env e2 with _ -> (env, TypeError) in
                                      (match (try n2 with _ -> (env, TypeError)) with
                                      | (env, Num j) -> (env, Num(i+j))
                                      | _ -> env, TypeError)
                    | _ -> env, TypeError)                  
  | Mult(e1, e2) -> let n1 = try multi_step env e1 with _ -> (env, TypeError) in
                    (match (try n1 with _ -> (env, TypeError)) with
                    | (env, Num i) -> let n2 = try multi_step env e2 with _ -> (env, TypeError) in
                                      (match (try n2 with _ -> (env, TypeError)) with
                                      | (env, Num j) -> (env, Num(i*j))
                                      | _ -> env, TypeError)
                    | _ -> env, TypeError)
  | Var varname -> let (y, t) = List.find (fun (y, t) -> y = varname) env in (env, t)
  | Lambda(var, body) -> (env, Lambda(var, body))
  | Apply(e1, e2) -> let e1' = try multi_step env e1 with _ -> env, TypeError in
                     (match (try e1' with _ -> (env, TypeError)) with
                     | env, Lambda(var, body) -> multi_step ((var, e2)::env) body
                     | _ -> env, TypeError)
  | Let(x, e1, e2) -> multi_step ((x, e1)::env) e2



