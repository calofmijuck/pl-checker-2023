(* Exercise 2. truefalse *)
open Ex2
open Testlib

let f1 = TRUE
let f2 = FALSE
let f3 = NOT f1
let f4 = ANDALSO (NOT f2, ANDALSO (f3, f1))
let f5 = ORELSE (ORELSE (f3, f1), f4)
let f6 = IMPLY (f4, f5)
let f7 = IMPLY (f5, ORELSE (f4, FALSE))
let f8 = ORELSE (IMPLY (NOT f6, f2), ANDALSO (ORELSE (f3, NOT f4), NOT f7))
let f9 = LESS (NUM 1, NUM 2)
let fa = LESS (PLUS (NUM 1, NUM 2), MINUS (NUM 0, NUM 121))
let fb = LESS (MINUS(PLUS (NUM 5, MINUS (NUM 1, NUM 21)), MINUS (NUM 0, NUM 100)), NUM 2)

module TestEx2: TestEx =
  struct
    type testcase =
      | TRUEFALSE of formula * bool

    let testcases: testcase list =
      [ TRUEFALSE (TRUE, true)
      ; TRUEFALSE (FALSE, false)
      ; TRUEFALSE (NOT TRUE, false)
      ; TRUEFALSE (NOT FALSE, true)
      ; TRUEFALSE (ANDALSO (TRUE, TRUE), true)
      ; TRUEFALSE (ANDALSO (TRUE, FALSE), false)
      ; TRUEFALSE (ANDALSO (FALSE, TRUE), false)
      ; TRUEFALSE (ANDALSO (FALSE, FALSE), false)
      ; TRUEFALSE (ORELSE (TRUE, TRUE), true)
      ; TRUEFALSE (ORELSE (TRUE, FALSE), true)
      ; TRUEFALSE (ORELSE (FALSE, TRUE), true)
      ; TRUEFALSE (ORELSE (FALSE, FALSE), false)
      ; TRUEFALSE (IMPLY (TRUE, TRUE), true)
      ; TRUEFALSE (IMPLY (TRUE, FALSE), false)
      ; TRUEFALSE (IMPLY (FALSE, TRUE), true)
      ; TRUEFALSE (IMPLY (FALSE, FALSE), true)
      ; TRUEFALSE (LESS (NUM 3, NUM 5), true)
      ; TRUEFALSE (LESS (NUM 3, NUM 3), false)
      ; TRUEFALSE (LESS (NUM 3, NUM 1), false)
      ; TRUEFALSE (LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1)), false)
      ; TRUEFALSE (LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13))), true)
      ; TRUEFALSE (f3, false)
      ; TRUEFALSE (f4, false)
      ; TRUEFALSE (f5, true)
      ; TRUEFALSE (f6, true)
      ; TRUEFALSE (f7, false)
      ; TRUEFALSE (f8, true)
      ; TRUEFALSE (f9, true)
      ; TRUEFALSE (fa, false)
      ; TRUEFALSE (fb, false)
      ; TRUEFALSE (IMPLY (NOT FALSE, NOT FALSE), true)
      ; TRUEFALSE (IMPLY (NOT FALSE, ANDALSO (TRUE, FALSE)), false)
      ; TRUEFALSE (IMPLY (FALSE, FALSE), true)
      ; TRUEFALSE (NOT (NOT (NOT (NOT (IMPLY (TRUE, TRUE))))), true)
      ; TRUEFALSE (ANDALSO (ORELSE(TRUE, FALSE), NOT (IMPLY (TRUE, FALSE))), true)
      ; TRUEFALSE (NOT (IMPLY (ANDALSO (FALSE, TRUE), ORELSE (FALSE, FALSE))), false)
      ; TRUEFALSE (LESS (PLUS (MINUS (NUM 4, NUM 5), MINUS (NUM 1, NUM (-1))), PLUS (MINUS (NUM 3, NUM (-5)), PLUS (NUM 4, NUM 5))), true)
      ; TRUEFALSE (ORELSE (LESS (MINUS (NUM 3, NUM 4), PLUS (NUM 3, NUM 4)), ANDALSO (NOT (TRUE), TRUE)), true)
      ; TRUEFALSE (ANDALSO (ANDALSO(TRUE, TRUE), ANDALSO (NOT (ANDALSO (TRUE, FALSE)), NOT (ORELSE (FALSE, FALSE)))), true)
      ; TRUEFALSE (IMPLY (IMPLY (LESS (NUM 3, MINUS (NUM 10, NUM 1)), ANDALSO (TRUE, TRUE)), IMPLY (ORELSE (TRUE, FALSE), NOT (TRUE))), false)
      ; TRUEFALSE (ANDALSO (ORELSE(TRUE, FALSE), NOT (IMPLY(TRUE, FALSE))), true)
      ; TRUEFALSE (ORELSE (LESS (PLUS (MINUS (NUM 3, NUM 2), NUM 9), NUM 10), FALSE), false)
      ; TRUEFALSE (IMPLY(LESS (NUM 1, NUM 0), ANDALSO(TRUE, ORELSE(NOT TRUE, LESS(NUM 2, NUM 1)))), true)
      ]

    let runner (tc: testcase): bool =
      match tc with
      | TRUEFALSE (f, result) -> eval f = result

    let string_of_tc (tc: testcase): string * string * string =
      match tc with
      | TRUEFALSE (f, result) ->
          ( ""
          , string_of_bool result
          , string_of_bool (eval f)
          )
  end

open TestEx2
let _ = wrapper testcases runner string_of_tc
