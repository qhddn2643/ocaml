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

let rec step (env : environment) (e : exp) : (environment * exp) = match e with
  | True -> True
  | False -> False
  | If (e, e1, e2) -> let e' = step[] e in
                      (match e' with
                      | True -> step[] e1
                      | False -> step[] e2
                      | _ -> raise Eval_error)
  | IsZero (e) -> let e' = step e in
                  (match e' with
                  | Num 0 -> True
                  | Num n -> False
                  | _ -> raise Eval_error)
  | Num e -> Num e
  | Plus(e1, e2) -> let n1 = step[] e1 in
                    let n2 = step[] e2 in
                    (match n1, n2 with
                    | Num i, Num j -> Num(i+j)
                    | _ -> raise Eval_error)
  | Mult(e1, e2) -> let n1 = step[] e1 in
  let n2 = step[] e2 in
                    (match n1, n2 with
                    | Num i, Num j -> Num(i*j)
                    | _ -> raise Eval_error)
  | Var varname -> Var varname
  | Lambda(var, body) -> Lambda(var, body)
  | Apply(e1, e2) -> let e1' = step[] e1 in
                     (match e1' with
                     | Lambda(var, body) -> step[] (substitution body var e2)
                     | _ -> raise Eval_error)


let rec multi_step (env : environment) (e : exp) : (environment * exp) = match e with
  | True -> step(True)
  | False -> step(False)
  | If (e, e1, e2) -> step(If(e, e1, e2))
  | IsZero (e) -> step(IsZero (e))
  | Num e -> step(Num e)
  | Plus(e1, e2) -> step(Plus(e1, e2))
  | Mult(e1, e2) -> step(Mult(e1, e2))
  | Var var -> step(Var var)
  | Lambda(var, typ, body) -> step(Lambda(var, typ, body))
  | Apply(e1, e2) -> step(Apply(e1, e2))

  
