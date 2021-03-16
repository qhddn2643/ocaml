exception Eval_error
exception Type_error
exception Substitution_error

type typ =  TBool | TInt 
(*  | TArrow of typ * typ*)

type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp
(*  | Var of string
  | Lambda of string * typ * exp
  | Apply of exp * exp *)

let rec type_check e = match e with
  | True -> TBool
  | False -> TBool
  | Num e -> TInt
 (* | Plus (e1, e2) ->  type_check e1 in Num e1
                      | _ -> raise Type_error 
  | IsZero (e) -> match e with
    | Num e -> raise Eval_error
    | _ -> let e' = type_check e in IsZero (e)
    | v -> raise Eval_error

  

let rec substitution (e1 : exp) (x : string) (e2 : exp) = match e with
  | Var(var) -> if var = x then e2 else Var(var)

let rec step e = match e with
  | If (True, e1, e2) -> e1
  | If (False, e1, e2) -> e2
  | If (e, e1, e2) -> match e with
    | Num(n) -> raise Eval_error
    | _ -> let e' = step e in If (e', e1, e2)
  | v -> raise Eval_error
  | True -> True
  | False -> False

let rec multi_step e = match e with
  | If (e1, e2, e3) -> step (If(e1, e2, e3))
  | True -> step (True)
  | False -> step (False)*)

let() =
 (* print_endline ("Output: Small-Step Semantics");      
  print_endline ((multi_step True));
  print_endline ((multi_step False));
  print_endline ((multi_step (Num 0)));
  print_endline ((multi_step (IsZero (Num 0))));
  print_endline ((multi_step (IsZero (Plus (Num 1, Num 1)))));
  print_endline ((multi_step (IsZero (Plus (Plus (Num 2, Num (-1)), Num 1)))));
  print_endline ((multi_step (Plus (Plus (Num (-1), Num 1), Plus (Num (-1), Num 1)))));
  print_endline ((multi_step (Plus (Num (-1), Plus (Mult (Num 2, Num 2), Num 1)))));
  print_endline ((multi_step (Plus (Plus (Plus (Num 2, Num (-1)), Num 1), Num (-1)))));
  print_endline ((multi_step (Plus (IsZero (Plus (Num (-1), Num 1)), Num 1))));
  print_endline ((multi_step (IsZero (If (IsZero (Num 0), True, Num 0)))));
  print_endline ((multi_step (IsZero (If ( IsZero (Mult (Num 5, Num 0)), If (False, Num 0, IsZero (Plus (Num (-1), Num 0))), Num 0)))));
  print_endline ((multi_step (If (IsZero (Plus (Num (-1), Num 1)), Num 2, True))));
  print_endline ((multi_step (If ( If (IsZero (Mult (Plus (Num 1, Num (-1)), Num 1)), False, True), Mult (Num 1, Num 2), True))));
  print_endline ((multi_step (If ( If (IsZero (Mult (Num 0, Num 0)), IsZero (Num 2), Num 0), Mult (Num 2, Mult (Num 1, Num 1)), Plus (Plus (Plus (Plus (If (IsZero (Num 0), Num 1, Num 0), Num (-1)), Num 1), Num (-1)), Num 1)))));
  print_endline ((multi_step (If (True, If (True, Mult (If (False, Num 0, Num 1), Num 1), Num 5), Plus (Mult (Num 4, Num 1), Num 1)))));
  print_endline ((multi_step (If (IsZero (If (IsZero (Plus (Num (-1), Num 2)), Num 0, Num 1)), If (True, If (False, Mult (Num 0, Num 6), Plus (Num 0, Num 1)), Num 5), Num 5))));
  print_endline ((multi_step (If (IsZero (Plus (Num (-1), Plus (Num 1, Plus (Num (-1), Num 1)))), IsZero (True), Num 1))));
  print_endline ((multi_step (Plus (Num 1, Plus (Num (-1), If (IsZero (Plus (Num 1, If (True, Num 1, Num 2))), Plus (Num 1, Num 2), Mult (Num 2, Num 2)))))));
  print_endline ((multi_step (Plus (Num (-1), If (IsZero (Plus (Num 5, Num (-4))), Mult (Num 123, Plus (Num 5, Num (-4))), IsZero (Num 0))))));*)

  print_endline("Output: Type_Check");
  print_endline(type_check True);
  print_endline(type_check False);
  print_endline(type_check (Num 0));
