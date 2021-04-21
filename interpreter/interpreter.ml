exception Eval_error
exception Type_error
exception Substitution_error

type typ = 
  | TBool 
  | TInt 
  | TArrow of typ * typ

type type_environment = (string * typ) list

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


(*Type System*)
let rec type_check (te : type_environment)(e : exp)  = match e with
  | True -> TBool
  | False -> TBool
  | If (e1, e2, e3) -> if type_check te e1 = TBool 
                          then begin
                             let t2 = type_check te e2 in
                               if t2 = type_check te e3 then t2 else raise Type_error
                          end
                       else raise Type_error
  | IsZero (e) -> let e' = type_check te e in
                  (match e' with
                  | TInt -> TBool
                  | _ -> raise Type_error)
  | Num e -> TInt
  | Plus(e1, e2) -> let n1 = type_check te e1 in
                    let n2 = type_check te e2 in
                    (match n1, n2 with
                    | TInt, TInt -> TInt
                    | _ -> raise Type_error)
  | Mult(e1, e2) -> let n1 = type_check te e1 in
                    let n2 = type_check te e2 in
                    (match n1, n2 with
                    | TInt, TInt -> TInt
                    | _ -> raise Type_error)
  | Var varname -> let (y, t) = List.find (fun (y, t) -> y = varname) te in t
  | Lambda(var, typ, body) -> TArrow(typ, type_check ((var, typ)::te) body)
  | Apply(e1, e2) -> let e1' = try type_check te e1 with _ -> raise Type_error in
                     let e2' = type_check te e2 in
                     (match e1' with
                     | TArrow(e11, e12) -> if e11 = e2' then e12 else raise Type_error
                     | _ -> raise Type_error)


(*Typed Lamda Calculas*)
let rec free_variables (e : exp) = match e with
  | Var(var) -> [var]
  | Lambda(var, typ, body) -> List.filter (fun x -> var <> x) (free_variables body) 
  | Apply(e1, e2) -> let e1' = free_variables e1 in
                     let e2' = free_variables e2 in
                     List.append e1' (List.filter (fun x -> not (List.mem x e1')) e2')

let rec substitution (e1 : exp) (x : string) (e2 : exp) = match e1 with
  | Var var -> if var = x then e2 else Var var
  | Apply(n1, n2) -> Apply(substitution n1 x e2, substitution n2 x e2)
  | Lambda(var, typ, body) -> if not (x = var) then Lambda(var, typ, substitution body x e2) else Lambda(var, typ, body)
  | Lambda(var, typ, body) -> if x = var then Lambda(var, typ, body)
                              else if (not (List.mem var (free_variables e2))) then Lambda(var, typ, substitution body x e2)
                              else raise Substitution_error
  | True -> True
  | False -> False
  | If (n1, n2, n3) -> If (substitution n1 x e2, substitution n2 x e2, substitution n3 x e2) 
  | IsZero (n) -> IsZero (substitution n x e2)
  | Num (n) -> Num (n)
  | Plus(n1, n2) -> let n1' = substitution n1 x e2 in 
                    let n2' = substitution n2 x e2 in 
                    Plus (n1', n2')
  | Mult(n1, n2) -> let n1' = substitution n1 x e2 in
                    let n2' = substitution n2 x e2 in
                    Mult(n1', n2')


(*Small Semantics*)
let rec step (e : exp) = match e with
  | True -> True
  | False -> False
  | If (e, e1, e2) -> let e' = step e in
                      (match e' with
                      | True -> step e1
                      | False -> step e2
                      | _ -> raise Eval_error)
  | IsZero (e) -> let e' = step e in
                  (match e' with
                  | Num 0 -> True
                  | Num n -> False
                  | _ -> raise Eval_error)
  | Num e -> Num e
  | Plus(e1, e2) -> let n1 = step e1 in
                    let n2 = step e2 in
                    (match n1, n2 with
                    | Num i, Num j -> Num(i+j)
                    | _ -> raise Eval_error)
  | Mult(e1, e2) -> let n1 = step e1 in
                    let n2 = step e2 in
                    (match n1, n2 with
                    | Num i, Num j -> Num(i*j)
                    | _ -> raise Eval_error)
  | Var varname -> Var varname
  | Lambda(var, typ, body) -> Lambda(var, typ, body)
  | Apply(e1, e2) -> let e1' = step e1 in
                     (match e1' with
                     | Lambda(var, typ, body) -> step (substitution body var e2)
                     | _ -> raise Eval_error)




let rec multi_step (e : exp) = match e with
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




  (*let() =
  print_endline ("");
  print_endline ((multi_step (True)));
  print_endline ((multi_step (False)));
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
  print_endline ((multi_step (Plus (Num (-1), If (IsZero (Plus (Num 5, Num (-4))), Mult (Num 123, Plus (Num 5, Num (-4))), IsZero (Num 0))))));
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
  print_endline(substitution (Mult (Plus (Num 5, Var "x"), Plus (Num 3, Var "x"))) "x" (Num 2));
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
