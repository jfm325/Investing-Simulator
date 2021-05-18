open OUnit2
open Stock
open User
open Interaction
open Cd_history
open Init
open Cd
open Portfolio

(* Printers *)

(* [string_of_s s] is the printer for string [s]. *)
let string_of_s s = s

let user =
  User.create_user 20000. stock_history_lst index_history_lst cd_history
    re_history_lst

let user2 =
  User.create_user 20000. stock_history_lst index_history_lst cd_history
    re_history_lst

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
  let p = getportfolio user in
  let stck_lst = get_stock_history p in

  let com1 = parse "buy_s COKE 7 " user in
  let com2 = parse "buy_s AAPL 9" user in
  let com3 = parse "sell_s AAPL 2" user in
  let st1 = List.hd stck_lst in
  let st2 = List.nth stck_lst 1 in
  let ticker1 = Stock_history.get_ticker st1 in
  let ticker2 = Stock_history.get_ticker st2 in
  let n = Stock_history.get_shares st2 in
  [
    (*Added tests for the Interaction module here*)
    ( "testing initial cash " >:: fun _ ->
      assert_equal 20000. (get_cash user2) ~printer:string_of_float );
    ( "Testing legal term for Cd" >:: fun _ ->
      assert_equal SixMonths (checklegalterm 1) );
    ( "Testing legal term for Cd" >:: fun _ ->
      assert_equal OneYear (checklegalterm 2) );
    ( "Testing legal term for Cd" >:: fun _ ->
      assert_equal ThreeYears (checklegalterm 3) );
    ("Testing sell" >:: fun _ -> assert_equal 7 n);
    (*testing stock purchase*)
    ("Testing Stock purchase" >:: fun _ -> assert_equal "COKE" ticker1);
    ("Testing Stock purchase" >:: fun _ -> assert_equal "AAPL" ticker2);
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
