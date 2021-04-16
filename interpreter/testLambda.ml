(* ocamlc interpreter.ml testLambda.ml -o testLambda *)

open Interpreter

let assert_equal a b = assert (a = b)

let assert_raises exc f =
  match f () with
  | exception exc' when exc = exc' -> ()
  | _ -> assert false

let test_1 () =
  assert_equal
    (substitution (Mult (Plus (Num 5, Var "x"), Plus (Num 3, Var "x"))) "x" (Num 2)) 
    (Mult (Plus (Num 5, Num 2), Plus (Num 3, Num 2)))

let test_2 () =
  assert_equal
    (substitution
       (If (IsZero (Var "y"), Var "y", Var "x"))
       "y"
       (Mult (Num 5, Var "x")))
    (If (IsZero (Mult (Num 5, Var "x")), Mult (Num 5, Var "x"), Var "x"))

let test_3 () =
  assert_equal
    (substitution (Lambda ("x", TInt, Plus (Var "x", Num 5))) "x" (Num 0))
    (Lambda ("x", TInt, Plus (Var "x", Num 5)))

let test_4 () =
  assert_equal
    (substitution
       (Lambda
          ( "x"
          , TBool
          , If (Var "x", Apply (Var "y", Num 0), Apply (Var "y", Num 1)) ))
       "y"
       (Lambda ("x", TInt, Plus (Var "x", Num 1))))
    (Lambda
       ( "x"
       , TBool
       , If
           ( Var "x"
           , Apply (Lambda ("x", TInt, Plus (Var "x", Num 1)), Num 0)
           , Apply (Lambda ("x", TInt, Plus (Var "x", Num 1)), Num 1) ) ))

let test_5 () =
  assert_equal
    (multi_step
       (Apply
          ( Lambda
              ("x", TInt, Mult (Plus (Num 5, Var "x"), Plus (Num 3, Var "x")))
          , Num 2 )))
    (Num 35)


let test_6 () =
  assert_equal
    (multi_step (Lambda ("x", TInt, Plus (Var "x", Num 5))))
    (Lambda ("x", TInt, Plus (Var "x", Num 5)))

let test_7 () =
  assert_equal
    (multi_step
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
          , False )))
    (Num 2)

let test_8 () =
  assert_equal
    (multi_step
       (Apply
          ( Lambda ("x", TBool, If (Var "x", Num 7, Num 33))
          , Apply (Lambda ("x", TInt, IsZero (Var "x")), Num 5) )))
    (Num 33)

let test_9 () =
  assert_raises Eval_error (fun _ ->
      multi_step (Apply (Lambda ("x", TInt, Plus (Var "x", Var "y")), Num 0)))

let test_10 () =
  assert_equal
    (multi_step
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
          , Num 3 )))
    (Num 12)

let test_11 () =
  assert_equal
    (multi_step
       (Apply
          ( Lambda
              ( "x"
              , TArrow (TInt, TArrow (TInt, TArrow (TInt, TInt)))
              , Apply (Apply (Apply (Var "x", Num 7), Num 6), Num 8) )
          , Lambda
              ( "x"
              , TInt
              , Lambda
                  ( "y"
                  , TInt
                  , Lambda
                      ("z", TInt, Plus (Mult (Var "x", Var "y"), Var "z")) )
              ) )))
    (Num 50)

let test_12 () =
  assert_raises Eval_error (fun _ ->
      multi_step (Apply (Plus (Num 5, Num 2), Lambda ("x", TInt, Var "x"))))

let test_13 () =
  assert_equal
    (type_check []
       (Apply
          ( Lambda
              ("x", TInt, Mult (Plus (Num 5, Var "x"), Plus (Num 3, Var "x")))
          , Num 2 )))
    TInt

let test_14 () =
  assert_equal
    (type_check [] (Lambda ("x", TInt, Plus (Var "x", Num 5))))
    (TArrow (TInt, TInt))

let test_15 () =
  assert_equal
    (type_check []
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
          , False )))
    TInt

let test_16 () =
  assert_equal
    (type_check []
       (Apply
          ( Lambda ("x", TBool, If (Var "x", Num 7, Num 33))
          , Apply (Lambda ("x", TInt, IsZero (Var "x")), Num 5) )))
    TInt

let test_17 () =
  assert_raises Type_error (fun _ ->
      type_check []
        (Apply (Lambda ("x", TInt, Plus (Var "x", Var "y")), Num 0)))

let test_18 () =
  assert_raises Type_error (fun _ ->
      type_check []
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
           , Lambda ("x", TInt, Var "x") )))

let test_19 () =
  assert_equal
    (type_check []
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
              ) )))
    (TArrow (TInt, TInt))

let test_20 () =
  assert_raises Type_error (fun _ ->
      type_check []
        (Apply (Plus (Num 5, Num 2), Lambda ("x", TInt, Var "x"))))

let () =
  print_endline "Typed Lambda-Calculus tests, your grade: 0.72";
  test_1 ();
  test_2 ();
  test_3 ();
  test_4 ();
  print_endline "Tests 1-4 passed, your grade: 0.73";
  test_5 ();
  test_6 ();
  test_7 ();
  test_8 ();
  test_9 ();
  test_10 ();
  test_11 ();
  test_12 ();
  print_endline "Tests 5-12 passed, your grade: 0.74";
  test_13 ();
  test_14 ();
  test_15 ();
  test_16 ();
  test_17 ();
  test_18 ();
  test_19 ();
  test_20 ();
  print_endline "Tests 13-20 passed, your grade: 0.75"
