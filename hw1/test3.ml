(* Exercise 3. crazy3 *)
open Ex3
open Testlib

module TestEx3: TestEx =
  struct
    type testcase =
      | CRAZY3 of crazy3 * int

    let testcases: testcase list =
      [ CRAZY3 (ZERO NIL, 0)
      ; CRAZY3 (ONE NIL, 1)
      ; CRAZY3 (TWO NIL, 2)
      ; CRAZY3 (MONE NIL, -1)
      ; CRAZY3 (MTWO NIL, -2)
      ; CRAZY3 (ONE(ONE(TWO(TWO(TWO(TWO(TWO(TWO NIL))))))), 6556)
      ; CRAZY3 (ZERO(ONE(ONE(ZERO(ZERO(ZERO(ONE(ZERO(ONE NIL)))))))), 7302)
      ; CRAZY3 (TWO(ONE(ONE(ONE(TWO(ZERO(TWO NIL)))))), 1661)
      ; CRAZY3 (ZERO(TWO(ONE(ONE(ONE(ZERO(ONE NIL)))))), 852)
      ; CRAZY3 (ONE(TWO(TWO(ONE(ONE(ONE(ONE(ONE(ONE NIL)))))))), 9853)
      ; CRAZY3 (MONE(MTWO(ZERO(ZERO(MTWO(MONE(MONE NIL)))))), -1141)
      ; CRAZY3 (MTWO(MONE(MTWO(MTWO(MTWO(MTWO(MTWO NIL)))))), -2183)
      ; CRAZY3 (MTWO(ZERO(MONE(MONE(MONE(MONE(MTWO(MTWO NIL))))))), -6194)
      ; CRAZY3 (TWO(ZERO(ONE(ZERO(ONE(TWO(ONE NIL)))))), 1307)
      ; CRAZY3 (MONE(ZERO(MTWO(ZERO(MONE(MONE(ZERO(MTWO NIL))))))), -4717)
      ; CRAZY3 (ONE(ONE(TWO(ZERO(ONE(ZERO(ZERO(ONE NIL))))))), 2290)
      ; CRAZY3 (TWO(TWO(TWO(ZERO(ZERO(TWO(ZERO(ZERO(ONE NIL)))))))), 7073)
      ; CRAZY3 (TWO(ZERO(ONE(ONE(ZERO(ZERO(ONE NIL)))))), 767)
      ; CRAZY3 (ZERO(ONE(ONE(ONE(ZERO(ONE(TWO(ONE NIL))))))), 3927)
      ; CRAZY3 (ZERO(ZERO(ZERO(MTWO(MTWO(MTWO(ZERO(MONE NIL))))))), -2889)
      ; CRAZY3 (TWO(ONE(ZERO(ZERO(TWO(ONE(ZERO(ONE(ONE NIL)))))))), 9158)
      ; CRAZY3 (ZERO(ZERO(MONE(MTWO(MTWO(ZERO(ZERO(MTWO NIL))))))), -4599)
      ; CRAZY3 (ZERO(ZERO(ONE(TWO(TWO(TWO(ZERO(ZERO(ONE NIL)))))))), 7272)
      ; CRAZY3 (MONE(MONE(ZERO(MTWO(ZERO(MONE(MONE(MONE NIL))))))), -3217)
      ; CRAZY3 (ZERO(MTWO(ZERO(ZERO(MTWO(MONE(ZERO(MONE NIL))))))), -2598)
      ; CRAZY3 (TWO(ZERO(TWO(ZERO(ONE(ZERO(TWO(TWO NIL))))))), 5933)
      ; CRAZY3 (ZERO(MTWO(MONE(ZERO(ZERO(MONE(MTWO NIL)))))), -1716)
      ; CRAZY3 (ZERO(ZERO(TWO(ZERO(TWO(TWO(ZERO(ZERO(ONE NIL)))))))), 7227)
      ; CRAZY3 (TWO(ONE(TWO(ONE(ZERO(ZERO(TWO(ZERO(ONE NIL)))))))), 8069)
      ; CRAZY3 (MTWO(MONE(ZERO(ZERO(MTWO(MONE(MONE(MONE NIL))))))), -3326)
      ; CRAZY3 (ZERO(ZERO(MTWO(MTWO(MONE(MTWO NIL))))), -639)
      ; CRAZY3 (ONE(ZERO(ONE(ONE(TWO(ZERO(ZERO(ONE(ONE NIL)))))))), 8947)
      ; CRAZY3 (TWO(ONE(ZERO(ONE(ZERO(TWO(ZERO(ZERO(ONE NIL)))))))), 7079)
      ; CRAZY3 (ZERO(MONE(ZERO(MTWO(MTWO(MONE NIL))))), -462)
      ; CRAZY3 (ONE(TWO(ONE(ONE(ZERO(ONE(ONE(ONE NIL))))))), 3202)
      ; CRAZY3 (ZERO(MTWO(MONE(ZERO(ZERO(MTWO(MONE(MONE NIL))))))), -3417)
      ; CRAZY3 (ZERO(MONE(MTWO(ZERO(MONE(MONE(ZERO(MTWO NIL))))))), -4719)
      ; CRAZY3 (ONE(TWO(ZERO(ZERO(ZERO(TWO(ZERO(ZERO(ONE NIL)))))))), 7054)
      ; CRAZY3 (MONE(ZERO(MTWO(ZERO(ZERO(ZERO(MTWO(MONE NIL))))))), -3664)
      ; CRAZY3 (ZERO(ZERO(ONE(ZERO(ZERO(TWO(TWO(ZERO(ONE NIL)))))))), 8514)
      ; CRAZY3 (MONE(MTWO(ZERO(MTWO(MTWO(MONE(MONE NIL)))))), -1195)
      ; CRAZY3 (MTWO(MONE(MONE(MONE(ZERO(MONE(MTWO NIL)))))), -1742)
      ; CRAZY3 (ONE(TWO(ONE(ZERO(TWO(TWO(TWO(TWO NIL))))))), 6496)
      ; CRAZY3 (ONE(ZERO(ONE(ONE(TWO(ONE(ZERO(ZERO(ONE NIL)))))))), 7003)
      ; CRAZY3 (ZERO(ONE(TWO(ONE(ZERO(ONE(ONE(ONE NIL))))))), 3207)
      ]

    let runner (tc: testcase): bool =
      match tc with
      | CRAZY3 (n, ans) -> crazy3val n = ans

    let string_of_tc (tc: testcase): string * string * string =
      match tc with
      | CRAZY3 (n, ans) ->
          ( ""
          , string_of_int ans
          , string_of_int (crazy3val n)
          )
  end

open TestEx3
let _ = wrapper testcases runner string_of_tc
