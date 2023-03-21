open Ex2
open Testlib

module TestEx2: TestEx =
  struct
    type testcase =
      | CALC of exp * ans
    and ans =
      | FLOAT of float
      | EXCEPTION

    let testcases =
      [ CALC (INT 10, FLOAT 10.0)
      ; CALC (REAL 1.0, FLOAT 1.0)
      ; CALC (ADD(INT 10, INT (-9)), FLOAT 1.0)
      ; CALC (ADD(INT 3, REAL 0.3), FLOAT 3.3)
      ; CALC (ADD(REAL (-0.1), REAL 0.1), FLOAT 0.0)
      ; CALC (SUB(REAL 0.5, INT 1), FLOAT (-0.5))
      ; CALC (SUB(INT 100, INT 99), FLOAT 1.0)
      ; CALC (MUL(INT 1, INT (-1)), FLOAT (-1.0))
      ; CALC (MUL(INT 10, REAL 0.01), FLOAT 0.1)
      ; CALC (DIV(REAL 0.1, REAL (-0.1)), FLOAT (-1.0))
      ; CALC (DIV(REAL 10.0, INT 5), FLOAT 2.0)
      ; CALC (DIV(X, X), EXCEPTION)
      ; CALC (ADD(SUB(REAL 1.0, MUL(INT 3, INT 5)), REAL (-3.0)), FLOAT (-17.0))
      ; CALC (SUB(DIV(REAL 10.0, MUL(INT 1, INT (-10))), X), EXCEPTION)
      ; CALC (SIGMA(INT 1, INT 10, X), FLOAT 55.0)
      ; CALC (SIGMA(REAL 1.1, REAL 10.1, MUL(X, X)), FLOAT 385.0)
      ; CALC (SIGMA(INT 10, INT 1, X), FLOAT 0.0)
      ; CALC (SIGMA(X, X, X), EXCEPTION)
      ; CALC (SIGMA(INT 1, X, INT 3), EXCEPTION)
      ; CALC (SIGMA(INT 3, INT 5, REAL 1.0), FLOAT 3.0)
      ; CALC (SIGMA(ADD(X, X), INT 5, SUB(X, INT 1)), EXCEPTION)
      ; CALC (SIGMA(SIGMA(INT 0, INT 0, INT 1), INT 10, SUB(MUL(X, X), X)), FLOAT 330.0)
      ; CALC (INTEGRAL(INT 1000, INT 1000, X), FLOAT 0.0)
      ; CALC (INTEGRAL(REAL 1.0, REAL 2.0, X), FLOAT 1.26)
      ; CALC (INTEGRAL(REAL 2.0, REAL 1.0, X), FLOAT (-1.26))
      ; CALC (INTEGRAL(REAL (-1.0), REAL 1.0, MUL(MUL(X, X), X)), FLOAT (-0.1))
      ; CALC (INTEGRAL(MUL(INT 1, REAL 1.0), DIV(REAL 1.0, REAL 0.2), SUB(INT 0, X)), FLOAT (-11.8))
      ; CALC (INTEGRAL(REAL 1.0, REAL 2.0, SIGMA(INT 1, INT 10, X)), FLOAT 49.5)
      ; CALC (INTEGRAL(REAL 0.0, REAL 1.0, INTEGRAL(REAL 0.0, REAL 1.0, X)), FLOAT 0.45)
      ; CALC (INTEGRAL(REAL 1.0, REAL 2.0, SIGMA(X, INT 10, REAL (-19.2))), EXCEPTION)
      ; CALC (INTEGRAL(X, X, X), EXCEPTION)
      ; CALC (INTEGRAL(DIV(INT 1, X), REAL 0.0, X), EXCEPTION)
      ; CALC (INTEGRAL(REAL 0.0, X, X), EXCEPTION)
      ; CALC (X, EXCEPTION)
      ]

    let runner (tc: testcase): bool =
      match tc with
      | CALC(expr, answer) ->
        (
          try
            let result = calculate(expr) in
            match answer with
            | EXCEPTION -> false
            | FLOAT f -> abs_float(result -. f) < 0.1
          with
          | FreeVariable -> (
            match answer with
            | EXCEPTION -> true
            | _ -> false
          )
        )

    let string_of_tc (tc: testcase): string * string * string =
      match tc with
      | CALC (expr, FLOAT ans) -> ("", string_of_float ans, string_of_float (calculate(expr)))
      | CALC (expr, EXCEPTION) -> ("", "FreeVariable", string_of_float (calculate(expr)))
  end

open TestEx2
let _ = wrapper testcases runner string_of_tc
