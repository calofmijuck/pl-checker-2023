(* Exercise 1. sigma *)
open Ex1
open Testlib

module TestEx1: TestEx =
  struct
    type testcase =
      | SIGMA of int * int * (int -> int) * string * int

    let testcases: testcase list =
      [ SIGMA (20, 20, (fun x -> x), "x -> x", 20)
      ; SIGMA (20, 10, (fun x -> x), "x -> x", 0)
      ; SIGMA (1, 10, (fun x -> x), "x -> x", 55)
      ; SIGMA (10, 10, (fun x -> x*x), "x -> x*x", 100)
      ; SIGMA (5, 5, (fun x -> x+x*x*2), "x -> 2*x*x + x", 55)
      ; SIGMA (2, 1, (fun x -> x + 1 + x*x), "x -> x*x + x + 1", 0)
      ; SIGMA (-1000, 1000, (fun x -> x*x*x), "x -> x*x*x", 0)
      ; SIGMA (-100, -1000, (fun _ -> 1), "x -> 1", 0)
      ; SIGMA (-10000, 0, (fun x -> abs x), "x -> abs x", 50005000)
      ; SIGMA (1, 10, (fun x -> if x mod 2 = 0 then 1 else 0), "x -> if x mod 2 = 0 then 1 else 0", 5)
      ; SIGMA (2, 4, (fun x -> x + 1), "x -> x + 1", 12)
      ; SIGMA (1, 1000, (fun x -> 0), "x -> 0", 0)
      ; SIGMA (10, 5, (fun x -> x), "x -> x", 0)
      ; SIGMA (11, 10, (fun x -> x), "x -> x", 0)
      ; SIGMA (10, 10, (fun x -> x), "x -> x", 10)
      ; SIGMA (-1, -10, (fun x -> x * x), "x -> x*x", 0)
      ; SIGMA (-10, -1, (fun x -> x * x), "x -> x*x", 385)
      ; SIGMA (1, 10, (fun x -> x * x), "x -> x*x", 385)
      ; SIGMA (-10, -1, (fun x -> 3 * x), "x -> 3 * x", -165)
      ; SIGMA (30, 40, (fun x -> (x * x) mod 7), "x -> x * x mod 7", 26)
      ]

    let runner (tc: testcase): bool =
      match tc with
      | SIGMA (a, b, f, fs, ans) -> sigma(a,b,f) = ans

    let string_of_tc (tc: testcase): string * string * string =
      match tc with
      | SIGMA (a, b, f, fs, ans) ->
          ( Printf.sprintf "sigma(%d, %d, %s)" a b fs
          , string_of_int ans
          , string_of_int (sigma(a,b,f))
          )
  end

open TestEx1
let _ = wrapper testcases runner string_of_tc
