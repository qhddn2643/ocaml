(* ocamlc interpreter_bigstep.ml testSyntax.ml -o testSyntax *)

open Interpreter_bigstep

let assert_equal a b = assert (a = b)

let test_1 () = assert_equal (string_of_exp (Num 3)) "3"

let test_2 () = assert_equal (string_of_exp True) "true"

let test_3 () = assert_equal (string_of_exp False) "false"

let test_4 () = assert_equal (string_of_exp (Plus (Num 3, Num 2))) "(3 + 2)"

let test_5 () = assert_equal (string_of_exp (Mult (Num 3, Num 2))) "(3 * 2)"

let test_6 () =
  assert_equal
    (string_of_exp
       (Plus (Num 3, Plus (Num 3, Mult (Num 2, Plus (Num 3, Num 2))))))
    "(3 + (3 + (2 * (3 + 2))))"

let test_7 () =
  assert_equal
    (string_of_exp (If (True, Num 3, Num 5)))
    "if true then 3 else 5"

let test_8 () =
  assert_equal
    (string_of_exp (If (False, Plus (Num 3, Num 2), Plus (Num 5, Num 1))))
    "if false then (3 + 2) else (5 + 1)"

let test_9 () =
  assert_equal
    (string_of_exp
       (If (Plus (False, True), Plus (Num 3, False), Mult (Num 3, Num 1))))
    "if (false + true) then (3 + false) else (3 * 1)"

let test_10 () =
  assert_equal
    (string_of_exp
       (If (IsZero (Num 1), Plus (Num 3, Num 2), Plus (Num 5, Num 1))))
    "if (isZero 1) then (3 + 2) else (5 + 1)"

let test_11 () =
  assert_equal
    (string_of_exp (IsZero (Mult (Num 3, Num 5))))
    "(isZero (3 * 5))"

let test_12 () =
  assert_equal
    (string_of_exp
       (IsZero
          (If (IsZero (Num 1), Plus (Num 3, Num 2), Plus (Num 5, Num 1)))))
    "(isZero if (isZero 1) then (3 + 2) else (5 + 1))"

let test_13 () =
  assert_equal
    (string_of_exp
       (Plus
          ( Num 3
          , If (IsZero (Num 1), Plus (Num 3, Num 2), Plus (Num 5, Num 1)) )))
    "(3 + if (isZero 1) then (3 + 2) else (5 + 1))"

let test_14 () =
  assert_equal
    (string_of_exp
       (Plus
          ( Num 3
          , Mult
              ( If (IsZero (Num 1), Plus (Num 3, Num 2), Plus (Num 5, Num 1))
              , IsZero True ) )))
    "(3 + (if (isZero 1) then (3 + 2) else (5 + 1) * (isZero true)))"

let test_15 () =
  assert_equal
    (string_of_exp
       (If (If (True, True, False), Plus (Num 3, Num 2), Plus (Num 5, Num 1))))
    "if if true then true else false then (3 + 2) else (5 + 1)"

let test_16 () =
  assert_equal
    (string_of_exp
       (If
          ( True
          , If
              ( IsZero (Mult (Num 3, Num 5))
              , Plus (Num 3, Num 2)
              , Plus (Num 5, Num 1) )
          , If (True, Mult (Num 3, Num 2), Mult (Num 2, Plus (Num 3, Num 2)))
          )))
    "if true then if (isZero (3 * 5)) then (3 + 2) else (5 + 1) else if true then (3 * 2) else (2 * (3 + 2))"

let () =
  print_endline "Syntax tests, your grade: 0.0";
  test_1 ();
  test_2 ();
  test_3 ();
  test_4 ();
  test_5 ();
  test_6 ();
  test_7 ();
  test_8 ();
  test_9 ();
  test_10 ();
  test_11 ();
  test_12 ();
  test_13 ();
  test_14 ();
  test_15 ();
  test_16 ();
  print_endline "Tests 1-16 passed, your grade: 0.6";
