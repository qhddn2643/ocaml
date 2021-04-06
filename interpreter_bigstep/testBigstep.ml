(* ocamlc interpreter_bigstep.ml testBigstep.ml -o testBigstep *)

open Interpreter_bigstep

let assert_equal a b = assert (a = b)

let assert_raises exc f =
  match f () with
  | exception exc' when exc = exc' -> ()
  | _ -> assert false

let test_1 () = assert_equal (eval True) True

let test_2 () = assert_equal (eval False) False

let test_3 () = assert_equal (eval (Num 0)) (Num 0)

let test_4 () = assert_equal (eval (IsZero (Num 0))) True

let test_5 () = assert_equal (eval (IsZero (Plus (Num 1, Num 1)))) False

let test_6 () =
  assert_equal (eval (IsZero (Plus (Plus (Num 2, Num (-1)), Num 1)))) False

let test_7 () =
  assert_equal
    (eval (Plus (Plus (Num (-1), Num 1), Plus (Num (-1), Num 1))))
    (Num 0)

let test_8 () =
  assert_equal
    (eval (Plus (Num (-1), Plus (Mult (Num 2, Num 2), Num 1))))
    (Num 4)

let test_9 () =
  assert_equal
    (eval (Plus (Plus (Plus (Num 2, Num (-1)), Num 1), Num (-1))))
    (Num 1)

let test_10 () =
  assert_raises Eval_error (fun _ ->
      eval (Plus (IsZero (Plus (Num (-1), Num 1)), Num 1)))

let test_11 () =
  assert_raises Eval_error (fun _ ->
      eval (IsZero (If (IsZero (Num 0), True, Num 0))))

let test_12 () =
  assert_raises Eval_error (fun _ ->
      eval
        (IsZero
           (If
              ( IsZero (Mult (Num 5, Num 0))
              , If (False, Num 0, IsZero (Plus (Num (-1), Num 0)))
              , Num 0 ))))

let test_13 () =
  assert_equal
    (eval (If (IsZero (Plus (Num (-1), Num 1)), Num 2, True)))
    (Num 2)

let test_14 () =
  assert_equal
    (eval
       (If
          ( If (IsZero (Mult (Plus (Num 1, Num (-1)), Num 1)), False, True)
          , Mult (Num 1, Num 2)
          , True )))
    True

let test_15 () =
  assert_equal
    (eval
       (If
          ( If (IsZero (Mult (Num 0, Num 0)), IsZero (Num 2), Num 0)
          , Mult (Num 2, Mult (Num 1, Num 1))
          , Plus
              ( Plus
                  ( Plus
                      ( Plus (If (IsZero (Num 0), Num 1, Num 0), Num (-1))
                      , Num 1 )
                  , Num (-1) )
              , Num 1 ) )))
    (Num 1)

let test_16 () =
  assert_equal
    (eval
       (If
          ( True
          , If (True, Mult (If (False, Num 0, Num 1), Num 1), Num 5)
          , Plus (Mult (Num 4, Num 1), Num 1) )))
    (Num 1)

let test_17 () =
  assert_equal
    (eval
       (If
          ( IsZero (If (IsZero (Plus (Num (-1), Num 2)), Num 0, Num 1))
          , If
              ( True
              , If (False, Mult (Num 0, Num 6), Plus (Num 0, Num 1))
              , Num 5 )
          , Num 5 )))
    (Num 5)

let test_18 () =
  assert_raises Eval_error (fun _ ->
      eval
        (If
           ( IsZero (Plus (Num (-1), Plus (Num 1, Plus (Num (-1), Num 1))))
           , IsZero True
           , Num 1 )))

let test_19 () =
  assert_equal
    (eval
       (Plus
          ( Num 1
          , Plus
              ( Num (-1)
              , If
                  ( IsZero (Plus (Num 1, If (True, Num 1, Num 2)))
                  , Plus (Num 1, Num 2)
                  , Mult (Num 2, Num 2) ) ) )))
    (Num 4)

let test_20 () =
  assert_raises Eval_error (fun _ ->
      eval
        (Plus
           ( Num (-1)
           , If
               ( IsZero (Plus (Num 5, Num (-4)))
               , Mult (Num 123, Plus (Num 5, Num (-4)))
               , IsZero (Num 0) ) )))

let () =
  print_endline "Big-step tests, your grade: 0.6";
  test_1 ();
  test_2 ();
  test_3 ();
  test_4 ();
  print_endline "Tests 1-4 passed, your grade: 0.61";
  test_5 ();
  test_6 ();
  test_7 ();
  test_8 ();
  print_endline "Tests 5-8 passed, your grade: 0.62";
  test_9 ();
  test_10 ();
  test_11 ();
  test_12 ();
  print_endline "Tests 9-12 passed, your grade: 0.63";
  test_13 ();
  test_14 ();
  test_15 ();
  test_16 ();
  print_endline "Tests 13-16 passed, your grade: 0.64";
  test_17 ();
  test_18 ();
  test_19 ();
  test_20 ();
  print_endline "Tests 17-20 passed, your grade: 0.65"
