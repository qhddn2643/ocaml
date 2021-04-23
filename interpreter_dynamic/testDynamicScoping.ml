(* ocamlc interpreter_dynamic.ml testDynamicScoping.ml -o testDynamicScoping *)

open Interpreter_dynamic

let assert_equal a b = assert (a = b)

let test_1 () =
  assert_equal (multi_step [] (Let ("x", Num 5, Num 1)) |> snd) (Num 1)

let test_2 () =
  assert_equal
    (multi_step [] (Let ("x", Num 5, Plus (Var "x", Num 5))) |> snd)
    (Num 10)

let test_3 () =
  assert_equal
    ( multi_step []
        (Let ("x", Num 3, Let ("y", Num 5, Mult (Var "x", Var "y")))) |> snd )
    (Num 15)

let test_4 () =
  assert_equal
    ( multi_step []
        (Let ("x", Num 3, Let ("x", Num 5, Mult (Var "x", Var "x")))) |> snd )
    (Num 25)

let test_5 () =
  assert_equal
    ( multi_step []
        (Mult
           ( Apply
               ( Lambda ("b", If (Var "b", Let ("x", Num 2, Num 1), Num 0))
               , True )
           , Var "x" )) |> snd )
    (Num 2)

let test_6 () =
  assert_equal
    ( multi_step []
        (Plus
           ( Apply
               ( Lambda
                   ( "n"
                   , If
                       ( IsZero (Var "n")
                       , Let ("x", Num 5, Var "n")
                       , Let
                           ( "n"
                           , Num 6
                           , Let ("x", Plus (Var "n", Num 1), Var "n") ) ) )
               , Num 0 )
           , Var "x" )) |> snd )
    (Num 5)

let test_7 () =
  assert_equal
    ( multi_step []
        (Let
           ( "x"
           , Num 1
           , Let
               ( "f"
               , Lambda ("a", Plus (Var "x", Var "a"))
               , Let
                   ( "g"
                   , Lambda ("a", Let ("x", Num 2, Apply (Var "f", Var "a")))
                   , Apply (Var "g", Num 3) ) ) )) |> snd ) 
    (Num 5)

let () =
  print_endline "Dynamic scoping tests, your grade: 0.75";
  test_1 ();
  print_endline "Test 1 passed, your grade: 0.76";
  test_2 ();
  print_endline "Test 2 passed, your grade: 0.77";
  test_3 ();
  print_endline "Test 3 passed, your grade: 0.78";
  test_4 ();
  print_endline "Test 4 passed, your grade: 0.79";
  test_5 ();
  print_endline "Test 5 passed, your grade: 0.80";
  test_6 ();
  print_endline "Test 6 passed, your grade: 0.81";
  test_7 ();
  print_endline "Test 7 passed, your grade: 0.82";
