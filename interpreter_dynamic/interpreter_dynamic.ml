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

  let rec step (env : environment) (e : exp) : (environment * exp) = match e with
  let rec multi_step (env : environment) (e : exp) : (environment * exp) = match e with

  
