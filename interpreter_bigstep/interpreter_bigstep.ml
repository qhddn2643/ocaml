type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp

let rec string_of_exp (e : exp) = match e with
  | True -> "true"
  | False -> "false"
  | If (e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3
  | Num e -> string_of_int e
  | IsZero (e) -> "isZero(" ^ string_of_exp e ^ ")"
  | Plus (e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
  | Mult (e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"

let() =
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
  print_endline (string_of_exp (If (True, If (IsZero (Mult (Num 3, Num 5)), Plus (Num 3, Num 2), Plus (Num 5, Num 1)), If (True, Plus (Num 3, Num 2), Mult (Num 2, Plus (Num 3, Num 2))))));





























