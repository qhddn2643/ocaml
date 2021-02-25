type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp

let rec string_of_exp (e : exp) = match e with
  | True -> true
  | False -> false
  | If(e1 ,e2 ,e3) -> if e1 then e2 else e3
  | Num e -> eval e
  | IsZero e -> iszero(eval e)
  | Plus (e1, e2) -> eval e1 + eval e2
  | Mult (e1, e2) -> eval e1 * eval e2
