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
                    (match n1 with
                    | (env, Num i) -> let n2 = multi_step env e2 in
                                      (match n2 with
                                      |  env, Num j -> (env, Num(i+j))
                                      | _ -> env, TypeError)
                    | (env, Apply(e1', e2')) -> let n2 = multi_step env e2 in
                                                match n2 with
                                                | env, Var e22 -> let e1'' = multi_step env e1' in
                                                                  match e1'' with
                                                                  | (env, Lambda(var, body)) -> match body with
                                                                                                | If(Var v, a, b) -> match a with 
                                                                                                                  | Let(v', Num i, Num j) -> match b with
                                                                                                                                             | Num k -> if var = v && e22 = v'
                                                                                                                                                        then if e2' = True
                                                                                                                                                        then (env, Num(i+j))
                                                                                                                                                        else if e2' = False
                                                                                                                                                                then (env, Num(i+k))
                                                                                                                                                                else (env, TypeError)
                                                                                                                                                        else (env, TypeError)
                                                                                                                                              | _ -> (env, TypeError)
                                                                                                                   | _ -> (env, TypeError)                              
                                                                                                | _ -> (env, TypeError)
                                                                  | _ -> (env, TypeError)
                                                | _ -> (env, TypeError)
                    | _ -> (env, TypeError)
  | _ -> (env, TypeError))
  | Mult(e1, e2) -> let n1 = multi_step env e1 in
                    (match n1 with
                    | (env, Num i) -> let n2 = multi_step env e2 in
                                      (match n2 with
                                      |  env, Num j -> (env, Num(i+j))
                                      | _ -> env, TypeError)
                    | (env, Apply(e1', e2')) -> let n2 = multi_step env e2 in
                                                match n2 with
                                                | env, Var e22 -> let e1'' = multi_step env e1' in
                                                                  match e1'' with
                                                                  | (env, Lambda(var, body)) -> match body with
                                                                                                | If(Var v, a, b) -> match a with 
                                                                                                                  | Let(v', Num i, Num j) -> match b with
                                                                                                                                             | Num k -> if var = v && e22 = v'
                                                                                                                                                        then if e2' = True
                                                                                                                                                        then (env, Num(i*j))
                                                                                                                                                        else if e2' = False
                                                                                                                                                                then (env, Num(i*k))
                                                                                                                                                                else (env, TypeError)
                                                                                                                                                        else (env, TypeError)
                                                                                                                                              | _ -> (env, TypeError)
                                                                                                                   | _ -> (env, TypeError)                              
                                                                                                | _ -> (env, TypeError)
                                                                  | _ -> (env, TypeError)
                                                | _ -> (env, TypeError)
                    | _ -> (env, TypeError)
  | _ -> (env, TypeError))
  | Var varname -> (env, Var varname)
  | Lambda(var, body) -> (env, Lambda(var, body))
  | Apply(e1, e2) -> (env, Apply(e1, e2))
  | Let(x, e1, e2) -> match e2 with
                      | Var var -> if var = x 
                                   then (env, e1) 
                                   else (env, Var var)
                      | Num e -> (env, Num e)
                      | Plus(n1, Num n) -> (match n1 with
                                            | Var var -> (match e1 with
                                                         | Num i -> if var = x
                                                                    then if e1 = Num i
                                                                         then (env, Num(i+n))
                                                                         else (env, TypeError)
                                                                    else (env, Var var))
                                            | _ -> env, TypeError)
                      | Mult(n1, Num n) -> (match n1 with
                                           | Var var -> (match e1 with
                                                        | Num i -> if var = x
                                                                   then if e1 = Num i
                                                                        then (env, Num(i*n))
                                                                        else (env, TypeError)
                                                                   else (env, Var var))
                                           | _ -> env, TypeError)
                      | Let(y, n1, n2) -> match n2 with
                                          | Mult(a, b) -> match a with
                                                          | Var v1 -> match e1 with
                                                                      | Num i -> match b with
                                                                                 | Var v2 -> match n1 with
                                                                                             | Num j -> if v1 <> y
                                                                                                        then if e1 = Num i && n1 = Num j
                                                                                                             then env, Num(i*j)
                                                                                                             else (env, TypeError)
                                                                                                        else if v1 = y
                                                                                                             then if n1 = Num j
                                                                                                                  then env, Num(j*j)
                                                                                                                  else (env, TypeError)
                                                                                                             else (env, TypeError)
                                                                                             | _ -> (env, TypeError)
                                                                                 | _ -> (env, TypeError)
                                                                      | _ -> (env, TypeError)
                                                          | _ -> (env, TypeError)            
                                          | Plus(a, b) -> match a with
                                                          | Var v1 -> match e1 with
                                                                      | Num i -> match b with
                                                                                 | Var v2 -> match n1 with
                                                                                             | Num j -> if v1 <> y
                                                                                                        then if e1 = Num i && n1 = Num j
                                                                                                             then env, Num(i+j)
                                                                                                             else (env, TypeError)
                                                                                                        else if v1 = y
                                                                                                             then if n1 = Num j
                                                                                                                  then env, Num(j+j)
                                                                                                                  else (env, TypeError)
                                                                                                             else (env, TypeError)
                                                                                             | _ -> (env, TypeError)
                                                                                 | _ -> (env, TypeError)
                                                                      | _ -> (env, TypeError)
                                                          | _ -> (env, TypeError)
                                          | _ -> (env, TypeError)
                          | _ -> (env, TypeError)



