open OUnit2
open Stock
open User
open Interaction
open Cd_history

(* Printers *)

(* [string_of_s s] is the printer for string [s]. *)
let string_of_s s = s

(* [cd_test] is the test for module CD. *)
let cd_test test_name (cd : Cd.t) expected_apy expected_monthly_rate
    expected_length =
  test_name >:: fun _ ->
  assert_equal expected_apy (Cd.get_apy cd) ~printer:string_of_float;
  assert_equal expected_monthly_rate
    (Cd.get_monthly_rate cd)
    ~printer:string_of_float;
  assert_equal expected_length (Cd.get_length cd) ~printer:string_of_int

let cd_tests =
  let cd_1 = Cd.create_cd 10. ThreeYears 1000. in
  []

(* [stock_test] is the test for module Stock. [stock] is tested for
   expected values of stock name, ticker symbol, and price at index
   [index] in the prices array of [stock]. *)
let stock_test test_name (stock : Stock.t) (name : string)
    (ticker : string) (price : float) (index : int) =
  test_name >:: fun _ ->
  assert_equal name (get_name stock) ~printer:string_of_s;
  assert_equal ticker (get_ticker stock) ~printer:string_of_s;
  assert_equal price (get_price stock index) ~printer:string_of_float

let interaction_tests =
  [
    (* Added tests for the Interaction module here *)
    ( "Testing for buying shares" >:: fun _ ->
      assert_equal (Buy [ "COKE"; "50" ]) (parse "buy COKE 50") );
    ( "Testing for selling shares" >:: fun _ ->
      assert_equal (Sell [ "COKE"; "50" ]) (parse "sell COKE 50") );
    ("Testing for cash" >:: fun _ -> assert_equal Cash (parse "cash"));
    ( "Testing for networth" >:: fun _ ->
      assert_equal Networth (parse "networth") );
    ( "Testing for Empty" >:: fun _ ->
      assert_raises EmptyCommand (fun () -> parse "") );
    ( "Testing for Empty" >:: fun _ ->
      assert_raises EmptyCommand (fun () -> parse " ") );
    ( "Testing for Malformed" >:: fun _ ->
      assert_raises BadCommand (fun () -> parse "hi i am confused") );
  ]

let stock_tests =
  let coke = Stock.create_stock "Coke" "COKE" "coke1995.txt" in
  [
    stock_test "COKE: Price at index 0 = 28." coke "Coke" "COKE" 28. 0;
    stock_test "COKE: Price at index 239 = 97.54" coke "Coke" "COKE"
      97.54 239;
    stock_test "COKE: Price at index 3 = 32.13" coke "Coke" "COKE" 32.13
      3;
    stock_test "COKE: Price at index 200 = 56.12" coke "Coke" "COKE"
      56.12 200;
  ]

let suite =
  "test suite 1" >::: List.flatten [ stock_tests; interaction_tests ]

let _ = run_test_tt_main suite
