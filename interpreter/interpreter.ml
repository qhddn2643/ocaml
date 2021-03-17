exception Eval_error
exception Type_error
exception Substitution_error 

type typ = TBool | TInt | TArrow of typ * typ

type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp
  | Var of string
  | Lambda of string * typ * exp
  | Apply of exp * exp

let rec type_check e = match e with
  | True -> "TBool"
  | False -> "TBool"
  | If (True, e2, e3) -> if type_check e2 = "TInt" && type_check e3 = "TInt" then "TInt" else raise Type_error
  | If (False, e2, e3) -> if type_check e2 = "TInt" && type_check e3 = "TInt" then "TInt" else raise Type_error
  | If (True, e2, e3) -> if type_check e2 = "TBool" && type_check e3 = "TBool" then "TBool" else raise Type_error
  | If (False, e2, e3) -> if type_check e2 = "TBool" && type_check e3 = "TBool" then "TBool" else raise Type_error
  | Num _ -> "TInt"
  | IsZero(e) -> "TBool"
  | Plus (e1, e2) -> if type_check e1 = "TInt" && type_check e2 = "TInt" then "TInt" else raise Type_error 
  | Mult (e1, e2) -> if type_check e1 = "TInt" && type_check e2 = "TInt" then "TInt" else raise Type_error
 

(*let rec substitution (e1 : exp) (x : string) (e2 : exp) = match e with
  | Var(var) -> if var = x then e2 else Var(var)
  | Lambda(e1, x, e2) -> Lambda(e1, x, e2)*) 


let rec step e = match e with
  | True -> "true"
  | False -> "false"
  | If (True, e1, e2) -> step (e1)
  | If (False, e1, e2) -> step (e2)
(*| If (e, e1, e2) -> match e with
    | Num n -> raise Eval_error
    | _ -> let e' = step (e) in If (e', e1, e2)
  | v -> raise Eval_error*)
  | Num e -> string_of_int e
  | IsZero (Num 0) -> step (True)
  | IsZero (n) -> step (False)
(*  | Plus (e1, e2) -> (step (e1)) + (step (e2))
  | Mult (e1, e2) -> (step (e1)) * (step (e2)) *)


(*| Apply((Lambda(var, typ, body)), arg) -> match arg with ..substitution.. *)

let rec multi_step e = match e with
  | If (e1, e2, e3) -> step (If(e1, e2, e3))
  | True -> if step (True) = "true" then "True" else "False"
  | False -> if step (False) = "false" then "False" else "True"
  | Num e -> "Num " ^ step (Num e)
  | IsZero(Num 0) -> multi_step (True)
  | IsZero (n) -> multi_step (False)
(*| Plus (e1, e2) -> step (Plus (e1, e2))
  | Mult (e1, e2) -> step (Mult (e1, e2))*)

let() =
  print_endline ("");
  print_endline ("Output: Small-Step Semantics");      
  print_endline ((multi_step (True)));
  print_endline ((multi_step (False)));
  print_endline ((multi_step (Num 0)));
  print_endline ((multi_step (IsZero (Num 0))));
  print_endline ((multi_step (IsZero (Plus (Num 1, Num 1)))));
  print_endline ((multi_step (IsZero (Plus (Plus (Num 2, Num (-1)), Num 1)))));
(*print_endline ((multi_step (Plus (Plus (Num (-1), Num 1), Plus (Num (-1), Num 1)))));
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

  print_endline(""); 
  print_endline("Output: Type_Check");
  print_endline(type_check True);
  print_endline(type_check False);
  print_endline(type_check (Num 0));
  print_endline(type_check (IsZero (Num 0)));
  print_endline(type_check (IsZero (Plus (Num 1, Num 1))));
  print_endline(type_check (IsZero (Plus (Plus (Num 2, Num (-1)), Num 1))));
  print_endline(type_check (Plus (Plus (Num (-1), Num 1), Plus (Num (-1), Num 1))));
  print_endline(type_check (Plus (Num (-1), Plus (Mult (Num 2, Num 2), Num 1))));
  print_endline(type_check (Plus (Plus (Plus (Num 2, Num (-1)), Num 1), Num (-1))));
  print_endline(type_check (Plus (IsZero (Plus (Num (-1), Num 1)), Num 1)));
  print_endline(type_check (IsZero (If (IsZero (Num 0), True, Num 0))));
  print_endline(type_check (IsZero (If (IsZero (Mult (Num 5, Num 0)), If (False, Num 0, IsZero (Plus (Num (-1), Num 0))), Num 0 ))));
  print_endline(type_check (If (IsZero (Plus (Num (-1), Num 1)), Num 2, True)));
  print_endline(type_check (If (If (IsZero (Mult (Plus (Num 1, Num (-1)), Num 1)), False, True), Mult (Num 1, Num 2), True )));
  print_endline(type_check (If (If (IsZero (Mult (Num 0, Num 0)), IsZero (Num 2), Num 0), Mult (Num 2, Mult (Num 1, Num 1)), Plus (Plus (Plus (Plus (If (IsZero (Num 0), Num 1, Num 0), Num (-1)), Num 1), Num (-1)), Num 1))));
  print_endline ((type_check (If (True, If (True, Mult (If (False, Num 0, Num 1), Num 1), Num 5), Plus (Mult (Num 4, Num 1), Num 1)))));
  print_endline ((type_check (If (IsZero (If (IsZero (Plus (Num (-1), Num 2)), Num 0, Num 1)), If (True, If (False, Mult (Num 0, Num 6), Plus (Num 0, Num 1)), Num 5), Num 5))));
  print_endline ((type_check (If (IsZero (Plus (Num (-1), Plus (Num 1, Plus (Num (-1), Num 1)))), IsZero (True), Num 1))));
  print_endline ((type_check (Plus (Num 1, Plus (Num (-1), If (IsZero (Plus (Num 1, If (True, Num 1, Num 2))), Plus (Num 1, Num 2), Mult (Num 2, Num 2)))))));
  print_endline ((type_check (Plus (Num (-1), If (IsZero (Plus (Num 5, Num (-4))), Mult (Num 123, Plus (Num 5, Num (-4))), IsZero (Num 0))))));

  print_endline("");
  print_endline("Output: Lambda Expression");
 (* print_endline(substitution (Mult (Plus (Num 5, Var "x"), Plus (Num 3, Var "x"))) "x" (Num 2));
  print_endline(substitution (If (IsZero (Var "y"), Var "y", Var "x")) "y" (Mult (Num 5, Var "x")));
  print_endline(substitution (Lambda ("x", TInt, Plus (Var "x", Num 5))) "x" (Num 0));
  print_endline(substitution (Lambda ("x", TBool, If (Var "x", Apply (Var "y", Num 0), Apply (Var "y", Num 1)))) "y" (Lambda ("x", TInt, Plus (Var "x", Num 1))));
  print_endline(multi_step
       (Apply
          ( Lambda
              ("x", TInt, Mult (Plus (Num 5, Var "x"), Plus (Num 3, Var "x")))
          , Num 2 )))
  print_endline(multi_step (Lambda ("x", TInt, Plus (Var "x", Num 5))));
  print_endline(multi_step
       (Apply
          ( Apply
              ( Lambda
                  ( "y"
                  , TArrow (TInt, TInt)
                  , Lambda
                      ( "x"
                      , TBool
                      , If
                          ( Var "x"
                          , Apply (Var "y", Num 0)
                          , Apply (Var "y", Num 1) ) ) )
              , Lambda ("x", TInt, Plus (Var "x", Num 1)) )
          , False )));
  print_endline(multi_step
      (Apply
         ( Lambda ("x", TBool, If (Var "x", Num 7, Num 33))
         , Apply (Lambda ("x", TInt, IsZero (Var "x")), Num 5) )));
  print_endline(multi_step (Apply (Lambda ("x", TInt, Plus (Var "x", Var "y")), Num 0)));
  print_endline(multi_step
       (Apply
          ( Apply
              ( Apply
                  ( Lambda
                      ( "x"
                      , TInt
                      , Lambda
                          ( "x"
                          , TInt
                          , Lambda
                              ( "z"
                              , TInt
                              , Mult (Plus (Var "x", Var "x"), Var "z") ) )
                      )
                  , Num 1 )
              , Num 2 )
          , Num 3 )));
  print_endline(multi_step
       (Apply
          ( Lambda
              ( "x"
              , TArrow (TInt, TArrow (TInt, TInt))
              , Apply (Apply (Apply (Var "x", Num 7), Num 6), Num 8) )
          , Lambda
              ( "x"
              , TInt
              , Lambda
                  ( "y"
                  , TInt
                  , Lambda
                      ("z", TInt, Plus (Mult (Var "x", Var "y"), Var "z")) )
              ) )));
  print_endline(multi_step (Apply (Plus (Num 5, Num 2), Lambda ("x", TInt, Var "x"))));
  print_endline(type_check []
       (Apply
          ( Lambda
              ("x", TInt, Mult (Plus (Num 5, Var "x"), Plus (Num 3, Var "x")))
          , Num 2 )));
  print_endline(type_check [] (Lambda ("x", TInt, Plus (Var "x", Num 5))));
  print_endline(type_check []
       (Apply
          ( Apply
              ( Lambda
                  ( "y"
                  , TArrow (TInt, TInt)
                  , Lambda
                      ( "x"
                      , TBool
                      , If
                          ( Var "x"
                          , Apply (Var "y", Num 0)
                          , Apply (Var "y", Num 1) ) ) )
              , Lambda ("x", TInt, Plus (Var "x", Num 1)) )
          , False )));
  print_endline(type_check []
		   (Apply
			  ( Lambda ("x", TBool, If (Var "x", Num 7, Num 33))
			  , Apply (Lambda ("x", TInt, IsZero (Var "x")), Num 5) )));
  print_endline(type_check []
        (Apply (Lambda ("x", TInt, Plus (Var "x", Var "y")), Num 0)));
  print_endline(type_check []
        (Apply
           ( Apply
               ( Apply
                   ( Lambda
                       ( "x"
                       , TInt
                       , Lambda
                           ( "x"
                           , TInt
                           , Lambda
                               ( "z"
                               , TInt
                               , Mult (Plus (Var "x", Var "x"), Var "z") ) )
                       )
                   , Num 1 )
               , Num 2 )
           , Lambda ("x", TInt, Var "x") )));
  print_endline(type_check []
       (Apply
          ( Lambda
              ( "x"
              , TArrow (TInt, TArrow (TInt, TArrow (TInt, TInt)))
              , Apply (Apply (Var "x", Num 7), Num 6) )
          , Lambda
              ( "x"
              , TInt
              , Lambda
                  ( "y"
                  , TInt
                  , Lambda
                      ("z", TInt, Plus (Mult (Var "x", Var "y"), Var "z")) )
              ) )));
  print_endline(type_check []
        (Apply (Plus (Num 5, Num 2), Lambda ("x", TInt, Var "x"))));*)
