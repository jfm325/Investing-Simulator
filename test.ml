open OUnit2
open Stock

(* Printers *)

(* [string_of_s s] is the printer for string [s]. *)
let string_of_s s = s

(* [stock_test] is the test for module Stock. [stock] is tested for
   expected values of stock name, ticker symbol, and price at index
   [index] in the prices array of [stock]. *)
let stock_test test_name (stock : Stock.t) (name : string)
    (ticker : string) (price : float) (index : int) =
  test_name >:: fun _ ->
  assert_equal name (get_name stock) ~printer:string_of_s;
  assert_equal ticker (get_ticker stock) ~printer:string_of_s;
  assert_equal price (get_price stock index) ~printer:string_of_float

let stock_tests =
  let coke = Stock.create_stock "Coke" "COKE" "coke.txt" in
  [
    stock_test "COKE: Price at index 0 = 21.5" coke "Coke" "COKE" 21.5 0;
    stock_test "COKE: Price at index 239 = 50.47" coke "Coke" "COKE"
      50.47 239;
    stock_test "COKE: Price at index 3 = 22.5" coke "Coke" "COKE" 22.5 3;
    stock_test "COKE: Price at index 200 = 62.98" coke "Coke" "COKE"
      62.98 200;
  ]

let suite = "test suite 1" >::: List.flatten [ stock_tests ]

let _ = run_test_tt_main suite
