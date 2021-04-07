(* ocamlc interpreter_dynamic.ml testDynamicTyping.ml -o testDynamicTyping *)

open Interpreter_dynamic

let assert_equal a b = assert (a = b)

let assert_raises exc f =
  match f () with
  | exception exc' when exc = exc' -> ()
  | _ -> assert false

module Dynamic_typing = struct
  let test_1 () =
    assert_equal
      (multi_step [] (If (IsZero (Plus (True, Num 4)), Num 3, Num 4)) |> snd)
      TypeError

  let test_2 () =
    assert_equal
      ( multi_step []
          (Apply
             ( Apply
                 ( Lambda
                     ( "mybool"
                     , Lambda
                         ( "myfunction"
                         , Apply (Var "myfunction", Var "mybool") ) )
                 , Lambda ("x", Plus (Var "x", Var "x")) )
             , True ))
      |> snd )
      TypeError

  let test_3 () =
    assert_equal
      ( multi_step []
          (Apply
             ( Lambda
                 ( "b"
                 , Apply
                     ( Lambda
                         ( "x"
                         , Apply
                             (Lambda ("x", Mult (Var "x", Num 1)), Var "x")
                         )
                     , If (Var "b", Plus (Num 5, Num 37), False) ) )
             , True ))
      |> snd )
      (Num 42)

  let test_4 () =
    assert_equal
      ( multi_step []
          (Apply
             ( Lambda
                 ( "b"
                 , Apply
                     ( Lambda
                         ( "x"
                         , Apply
                             (Lambda ("x", Mult (Var "x", Num 1)), Var "x")
                         )
                     , If (Var "b", Plus (Num 5, Num 37), False) ) )
             , False ))
      |> snd )
      TypeError

  let test_5 () =
    assert_equal
      ( multi_step []
          (Apply
             ( Lambda ("f", Apply (Var "f", Num 1))
             , Lambda
                 ("x", Mult (Var "x", If (IsZero (Var "x"), False, Num 7)))
             ))
      |> snd )
      (Num 7)

  let test_6 () =
    assert_equal
      ( multi_step []
          (Apply
             ( Lambda ("f", Apply (Var "f", Num 1))
             , Lambda
                 ("x", Mult (Var "x", If (IsZero (Var "y"), False, Num 7)))
             ))
      |> snd )
      TypeError

  let test_7 () =
    assert_equal
      ( multi_step []
          (Apply
             ( Lambda
                 ( "f"
                 , Apply
                     ( Lambda
                         ( "g"
                         , Apply
                             (Var "g", Apply (Apply (Var "f", Num 0), False))
                         )
                     , Lambda
                         ( "x"
                         , Mult
                             (Var "x", If (IsZero (Var "x"), Num 1, Var "x"))
                         ) ) )
             , Lambda
                 ( "x"
                 , Lambda
                     ( "b"
                     , Plus
                         ( Var "X"
                         , If
                             ( IsZero (Var "x")
                             , If (Var "b", Num (-1), False)
                             , Num 1 ) ) ) ) ))
      |> snd )
      TypeError
end

let () =
  let open Dynamic_typing in
  print_endline "Dynamic typing tests, your grade: 0.75";
  test_1 ();
  print_endline "Test 1 passed, your grade: 0.76";
  test_2 ();
  test_3 ();
  print_endline "Tests 2-3 passed, your grade: 0.77";
  test_4 ();
  test_5 ();
  print_endline "Tests 4-5 passed, your grade: 0.78";
  test_6 ();
  test_7 ();
  print_endline "Tests 6-7 passed, your grade: 0.79"
