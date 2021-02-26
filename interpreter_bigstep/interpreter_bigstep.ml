exception Eval_error

(*Data Type*)
type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp

(* Syntax *)
let rec string_of_exp (e : exp) = match e with
  | True -> "true"
  | False -> "false"
  | If (e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3
  | Num e -> string_of_int e
  | IsZero (e) -> "isZero(" ^ string_of_exp e ^ ")"
  | Plus (e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
  | Mult (e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"

(* Big-Step Semantics *)
let rec eval (e : exp) = match e with
  | True -> if string_of_exp (True) = "true" then "True" else "False"
  | False -> if string_of_exp (False) = "false" then "False" else "True"                        
  | If (True, e2, e3) -> if string_of_exp e2 = "true" then eval (True) else eval (False)
  | If (True, e2, e3) -> if string_of_exp e2 = "false" then eval (False) else eval (True)
  | If (False, e2, e3) -> if string_of_exp e3 = "true" then eval (True) else eval (False)
  | If (False, e2, e3) -> if string_of_exp e3 = "false" then eval (False) else eval (True)
  | Num e -> string_of_exp(Num e)
  | IsZero (e) -> if string_of_exp e = "0" then eval (True) else eval (False)
  | Plus (e1, e2) -> match e1 with
  | Num (n) -> raise Eval_error 
  | Num e1 -> match e2 with 
  | Num (n) -> raise Eval_error
  | Num e2 -> string_of_exp (Num (e1 + e2))
  | Mult (e1, e2) -> match e1 with
  | Num (n) -> raise Eval_error
  | Num e1 -> match e2 with
  | Num (n) -> raise Eval_error
  | Num e2 -> string_of_exp (Num (e1 * e2))


let() =
  print_endline ("Output: Syntax");      
  print_endline (string_of_exp (Num 3));
  print_endline (string_of_exp (True));
  print_endline (string_of_exp (False));
  print_endline (string_of_exp (Plus (Num 3, Num 2)));
  print_endline (string_of_exp (Mult (Num 3, Num 2)));
  print_endline (string_of_exp (Plus (Num 3, Plus (Num 3, Mult (Num 2, Plus (Num 3, Num 2))))));
  print_endline (string_of_exp (If (True, Num 3, Num 5)));
  print_endline (string_of_exp (If (False, Plus (Num 3, Num 2), Plus (Num 5, Num 1))));
  print_endline (string_of_exp (If (Plus (False, True), Plus (Num 3, False), Mult (Num 3, Num 1))));
  print_endline (string_of_exp (If (IsZero (Num 1), Plus (Num 3, Num 2), Plus (Num 5, Num 1))));
  print_endline (string_of_exp (IsZero (Mult (Num 3, Num 5))));
  print_endline (string_of_exp (IsZero (If (IsZero (Num 1), Plus (Num 3, Num 2), Plus (Num 5, Num 1)))));
  print_endline (string_of_exp (Plus (Num 3, If( IsZero (Num 1), Plus (Num 3, Num 2), Plus (Num 5, Num 1)))));
  print_endline (string_of_exp (Plus (Num 3, If( IsZero (Num 1), Plus (Num 3, Num 2), Mult (Plus (Num 5, Num 1), IsZero (True))))));
  print_endline (string_of_exp (If (If (True, True, False), Plus (Num 3, Num 2), Plus (Num 5, Num 1))));
  print_endline (string_of_exp (If (True, If (IsZero (Mult (Num 3, Num 5)), Plus (Num 3, Num 2), Plus (Num 5, Num 1)), If (True, Plus (Num 3, Num 2), Mult (Num 2, Plus (Num 3, Num 2)))))) ;

  print_endline (" ");
  print_endline ("Output: Big-Step Semantics");
  print_endline (eval (True));
  print_endline (eval (False));
  print_endline (eval (Num (0)));
  print_endline (eval (IsZero (Num (0))));
  print_endline (eval (IsZero (Plus (Num 1, Num 1))));
  print_endline (eval (IsZero (Plus (Plus (Num 2, Num (-1)), Num 1))));
  print_endline (eval (Plus (Plus (Num (-1), Num 1), Plus (Num (-1), Num 1))));
  print_endline (eval (Plus (Num (-1), Plus (Mult (Num 2, Num 2), Num 1))));
  print_endline (eval (Plus (Plus (Plus (Num 2, Num (-1)), Num 1), Num (-1))));
  print_endline (eval (Plus (IsZero (Plus (Num (-1), Num 1)), Num 1)));
  print_endline (eval (IsZero (If (IsZero (Num 0), True, Num 0))));
  print_endline (eval (IsZero (If ( IsZero (Mult (Num 5, Num 0)), If (False, Num 0, IsZero (Plus (Num (-1), Num 0))), Num 0 ))));
  print_endline (eval (If (IsZero (Plus (Num (-1), Num 1)), Num 2, True)));
  print_endline (eval (If (If (IsZero (Mult (Plus (Num 1, Num (-1)), Num 1)), False, True), Mult (Num 1, Num 2), True )));
  print_endline (eval (If (If (IsZero (Mult (Num 0, Num 0)), IsZero (Num 2), Num 0), Mult (Num 2, Mult (Num 1, Num 1)), Plus (Plus (Plus( Plus (If (IsZero (Num 0), Num 1, Num 0), Num (-1)), Num 1 ), Num (-1) ), Num 1))));
  print_endline (eval (If (True, If (True, Mult (If (False, Num 0, Num 1), Num 1), Num 5), Plus (Mult (Num 4, Num 1), Num 1))));
  print_endline (eval (If (IsZero (If (IsZero (Plus (Num (-1), Num 2)), Num 0, Num 1)), If (True, If (False, Mult (Num 0, Num 6), Plus (Num 0, Num 1)), Num 5), Num 5)));
  print_endline (eval (If (IsZero (Plus (Num (-1), Plus (Num 1, Plus (Num (-1), Num 1)))), IsZero (True), Num 1)));
  print_endline (eval (Plus (Num 1, Plus (Num (-1), If (IsZero (Plus (Num 1, If (True, Num 1, Num 2))), Plus (Num 1, Num 2), Mult (Num 2, Num 2))))));
  print_endline (eval (Plus (Num (-1), If (IsZero (Plus (Num 5, Num (-4))), Mult (Num 123, Plus (Num 5, Num (-4))), IsZero (Num 0)))));








