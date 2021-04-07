(* ocamlc interpreter.ml testTypeSystem2.ml -o testTypeSystem *)

open Interpreter

let assert_equal a b = assert (a = b)

let assert_raises exc f =
  match f () with
  | exception exc' when exc = exc' -> ()
  | _ -> assert false

let test_1 () = assert_equal (type_check [] True) TBool

let test_2 () = assert_equal (type_check [] False) TBool

let test_3 () = assert_equal (type_check [] (Num 0)) TInt

let test_4 () = assert_equal (type_check [] (IsZero (Num 0))) TBool

let test_5 () =
  assert_equal (type_check [] (IsZero (Plus (Num 1, Num 1)))) TBool

let test_6 () =
  assert_equal
    (type_check [] (IsZero (Plus (Plus (Num 2, Num (-1)), Num 1))))
    TBool

let test_7 () =
  assert_equal
    (type_check [] (Plus (Plus (Num (-1), Num 1), Plus (Num (-1), Num 1))))
    TInt

let test_8 () =
  assert_equal
    (type_check [] (Plus (Num (-1), Plus (Mult (Num 2, Num 2), Num 1))))
    TInt

let test_9 () =
  assert_equal
    (type_check [] (Plus (Plus (Plus (Num 2, Num (-1)), Num 1), Num (-1))))
    TInt

let test_10 () =
  assert_raises Type_error (fun _ ->
      type_check [] (Plus (IsZero (Plus (Num (-1), Num 1)), Num 1)))

let test_11 () =
  assert_raises Type_error (fun _ ->
      type_check [] (IsZero (If (IsZero (Num 0), True, Num 0))))

let test_12 () =
  assert_raises Type_error (fun _ ->
      type_check []
        (IsZero
           (If
              ( IsZero (Mult (Num 5, Num 0))
              , If (False, Num 0, IsZero (Plus (Num (-1), Num 0)))
              , Num 0 ))))

let test_13 () =
  assert_raises Type_error (fun _ ->
      type_check [] (If (IsZero (Plus (Num (-1), Num 1)), Num 2, True)))

let test_14 () =
  assert_raises Type_error (fun _ ->
      type_check []
        (If
           ( If (IsZero (Mult (Plus (Num 1, Num (-1)), Num 1)), False, True)
           , Mult (Num 1, Num 2)
           , True )))

let test_15 () =
  assert_raises Type_error (fun _ ->
      type_check []
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

let test_16 () =
  assert_equal
    (type_check []
       (If
          ( True
          , If (True, Mult (If (False, Num 0, Num 1), Num 1), Num 5)
          , Plus (Mult (Num 4, Num 1), Num 1) )))
    TInt

let test_17 () =
  assert_equal
    (type_check []
       (If
          ( IsZero (If (IsZero (Plus (Num (-1), Num 2)), Num 0, Num 1))
          , If
              ( True
              , If (False, Mult (Num 0, Num 6), Plus (Num 0, Num 1))
              , Num 5 )
          , Num 5 )))
    TInt

let test_18 () =
  assert_raises Type_error (fun _ ->
      type_check []
        (If
           ( IsZero (Plus (Num (-1), Plus (Num 1, Plus (Num (-1), Num 1))))
           , IsZero True
           , Num 1 )))

let test_19 () =
  assert_equal
    (type_check []
       (Plus
          ( Num 1
          , Plus
              ( Num (-1)
              , If
                  ( IsZero (Plus (Num 1, If (True, Num 1, Num 2)))
                  , Plus (Num 1, Num 2)
                  , Mult (Num 2, Num 2) ) ) )))
    TInt

let test_20 () =
  assert_raises Type_error (fun _ ->
      type_check []
        (Plus
           ( Num (-1)
           , If
               ( IsZero (Plus (Num 5, Num (-4)))
               , Mult (Num 123, Plus (Num 5, Num (-4)))
               , IsZero (Num 0) ) )))

let () =
  print_endline "Type system tests, your grade: 0.69";
  test_1 ();
  test_2 ();
  test_3 ();
  test_4 ();
  test_5 ();
  test_6 ();
  print_endline "Tests 1-6 passed, your grade: 0.7";
  test_7 ();
  test_8 ();
  test_9 ();
  test_10 ();
  test_11 ();
  test_12 ();
  test_13 ();
  print_endline "Tests 7-13 passed, your grade: 0.71";
  test_14 ();
  test_15 ();
  test_16 ();
  test_17 ();
  test_18 ();
  test_19 ();
  test_20 ();
  print_endline "Tests 14-20 passed, your grade: 0.72"
