open OUnit2
open Stock
open User
open Interaction
open Cd_history
open Init

(* Printers *)

(* [string_of_s s] is the printer for string [s]. *)
let string_of_s s = s

let user =
  User.create_user 20000. stock_history_lst index_history_lst cd_history

let user2 =
  User.create_user 20000. stock_history_lst index_history_lst cd_history

(* [cd_monthly_rate_test] is the test for the correct conversion from
   APY to monthly rate in the function [Cd.match_monthly_rate]. *)
let cd_monthly_rate_test test_name term rate expected_monthly_rate =
  "[match_monthly_rate] test: " ^ test_name >:: fun _ ->
  assert_equal expected_monthly_rate
    (Cd.match_monthly_rate term rate)
    ~printer:string_of_float

let cd_math_tests =
  [
    cd_monthly_rate_test "6months, Rate: 0.024 -> Monthly Rate: 1.00198"
      SixMonths 0.024 1.00198;
    cd_monthly_rate_test "6months, Rate: 0.10 -> Monthly Rate: 1.00797"
      SixMonths 0.10 1.00797;
    cd_monthly_rate_test "6months, Rate: 0.013 -> Monthly Rate: 1.00108"
      SixMonths 0.013 1.00108;
    cd_monthly_rate_test "6months, Rate: 0.5 -> Monthly Rate: 1.03437"
      SixMonths 0.5 1.03437;
    cd_monthly_rate_test "1yr, Rate: 0.024 -> Monthly Rate: 1.00198"
      OneYear 0.024 1.00198;
    cd_monthly_rate_test "1yr, Rate: 0.10 -> Monthly Rate: 1.00797"
      OneYear 0.10 1.00797;
    cd_monthly_rate_test "1yr, Rate: 0.011 -> Monthly Rate: 1.00091"
      OneYear 0.011 1.00091;
    cd_monthly_rate_test "1yr, Rate: 0.5 -> Monthly Rate: 1.03437"
      OneYear 0.5 1.03437;
    cd_monthly_rate_test "3yrs, Rate: 0.024 -> Monthly Rate: 1.00198"
      ThreeYears 0.024 1.00198;
    cd_monthly_rate_test "3yrs, Rate: 0.10 -> Monthly Rate: 1.00797"
      ThreeYears 0.10 1.00797;
    cd_monthly_rate_test "3yrs, Rate: 0.010 -> Monthly Rate: 1.00083"
      ThreeYears 0.010 1.00083;
    cd_monthly_rate_test "3yrs, Rate: 0.5 -> Monthly Rate: 1.03437"
      ThreeYears 0.5 1.03437;
  ]

(* [cd_test] is the test for correct values in type t in module CD. *)
let cd_test test_name (cd : Cd.t) expected_apy expected_monthly_rate
    expected_length expected_collection_value =
  "CD Test: " ^ test_name >:: fun _ ->
  assert_equal expected_apy (Cd.get_apy cd) ~printer:string_of_float;
  assert_equal expected_monthly_rate
    (Cd.get_monthly_rate cd)
    ~printer:string_of_float;
  assert_equal expected_length (Cd.get_length cd) ~printer:string_of_int;
  Cd.update_current_value cd;
  assert_equal expected_collection_value
    (Cd.get_current_value cd)
    ~printer:string_of_float

let cd_tests =
  Game.update_start_time (Unix.time ());
  let cd_1 = Cd.create_cd 0.10 ThreeYears 1000. in
  let cd_2 = Cd.create_cd 0.02 SixMonths 1000. in
  let cd_3 = Cd.create_cd 0.01 SixMonths 1000. in
  let cd_4 = Cd.create_cd 0.08 OneYear 10000. in
  let cd_5 = Cd.create_cd 0.12 ThreeYears 100000. in
  let cd_6 = Cd.create_cd 0.013 ThreeYears 100000. in
  let cd_7 = Cd.create_cd 0.019 SixMonths 100000. in
  let cd_8 = Cd.create_cd 0.15 OneYear 1000000. in
  Game.update_start_time 0.;
  [
    cd_test "CD 1" cd_1 0.11 1.00873 36 1367.41;
    cd_test "CD 2" cd_2 0.01 1.00083 6 1004.99;
    cd_test "CD 3 (Edge Case: 0.1 APY should remain the same)" cd_3 0.01
      1.00083 6 1004.99;
    cd_test "CD 4" cd_4 0.08 1.00643 12 10799.48;
    cd_test "CD 5" cd_5 0.13 1.01024 36 144305.93;
    cd_test "CD 6" cd_6 0.023 1.00190 36 107072.41;
    cd_test "CD 7" cd_7 0.009 1.00075 6 100450.84;
    cd_test "CD 8" cd_8 0.15 1.01171 12 1149932.93;
  ]

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
  let coke = Stock.create_stock "Coke" "COKE" "coke1995.txt" in
  let spy = Stock.create_stock "S&P500" "S&P500" "spy_index1995.txt" in
  [
    stock_test "COKE: Price at index 0 = 28." coke "Coke" "COKE" 28. 0;
    stock_test "COKE: Price at index 239 (Last Index) = 97.54" coke
      "Coke" "COKE" 97.54 239;
    stock_test "COKE: Price at index 3 = 32.13" coke "Coke" "COKE" 32.13
      3;
    stock_test "COKE: Price at index 200 = 56.12" coke "Coke" "COKE"
      56.12 200;
    stock_test "S&P500: Price at index 0 = 49.02" spy "S&P500" "S&P500"
      49.02 0;
    stock_test "S&P500: Price at index 50 = 123.56" spy "S&P500"
      "S&P500" 133.25 50;
    stock_test "S&P500: Price at index 200 = 125.50" spy "S&P500"
      "S&P500" 125.50 200;
    stock_test "S&P500: Price at index 239 (Last Index) = 199.45" spy
      "S&P500" "S&P500" 199.45 239;
  ]

let cash_test test_name (user : User.t) (cash : float) =
  test_name >:: fun _ ->
  assert_equal cash (User.get_cash user) ~printer:string_of_float

let buy_stock_test test_name (user : User.t) (stock_n : int)
    (shares : int) (length : int) =
  test_name >:: fun _ ->
  Stock.update_current_prices stocks (Game.get_start_time ());
  let stock = List.nth Init.stocks stock_n in
  let cost = float shares *. Stock.get_current_price stock in
  if User.get_cash user -. cost < 0.0 || shares < 0 then
    print_endline
      "TRANSACTION ERROR: You do not have enough cash to purchase this \
       stock \n"
  else User.buy_stock shares user stock;
  assert_equal length
    (User.get_shares_sh_lst
       (Portfolio.get_stock_history (User.get_portfolio user))
       0)
    ~printer:string_of_int

let buy_index_test test_name (user : User.t) (stock_name : string)
    (shares : int) (length : int) =
  test_name >:: fun _ ->
  Stock.update_current_prices index_funds (Game.get_start_time ());
  let stock = legal index_funds stock_name in
  let cost = float shares *. Stock.get_current_price stock in
  if User.get_cash user -. cost < 0.0 || shares < 0 then
    print_endline
      "TRANSACTION ERROR: You do not have enough cash to purchase this \
       stock \n"
  else User.buy_index shares user (legal index_funds stock_name);
  assert_equal length
    (User.get_shares_ih_lst
       (Portfolio.get_index_history (User.get_portfolio user))
       0)
    ~printer:string_of_int

let sell_stock_test test_name (user : User.t) (nums : int)
    (shares : int) (length : int) =
  test_name >:: fun _ ->
  Stock.update_current_prices stocks (Game.get_start_time ());
  if shares < 0 then
    print_endline
      "TRANSACTION ERROR: You do not have enough cash to purchase this \
       stock \n"
  else
    User.sell_stock shares user (List.nth Init.stocks nums) (nums : int);
  assert_equal length
    (User.get_shares_sh_lst
       (Portfolio.get_stock_history (User.get_portfolio user))
       0)
    ~printer:string_of_int

let sell_index_test test_name (user : User.t) (shares : int) (num : int)
    (length : int) =
  test_name >:: fun _ ->
  Stock.update_current_prices index_funds (Game.get_start_time ());
  if shares < 0 then
    print_endline
      "TRANSACTION ERROR: You do not have enough cash to purchase this \
       stock \n"
  else User.sell_index shares user (List.nth Init.index_funds num) num;
  assert_equal length
    (User.get_shares_ih_lst Init.index_history_lst 0)
    ~printer:string_of_int

let networth_test test_name (user : User.t) (networth : float) =
  test_name >:: fun _ ->
  Stock.update_current_prices stocks (Game.get_start_time ());
  Stock.update_current_prices index_funds (Game.get_start_time ());
  assert_equal networth
    (User.get_net_worth user Init.stocks index_funds)
    ~printer:string_of_float

let (user_tests : OUnit2.test list) =
  let bob =
    User.create_user 20000. stock_history_lst index_history_lst
      cd_history
  in
  [
    cash_test "testing for default cash" bob 20000.;
    networth_test "testing for networth with no purchased stocks" bob
      20000.;
    buy_stock_test "testing to see that buy method works with one stock"
      bob 1 1 1;
    buy_stock_test "testing to see that buy method works with no stock"
      bob 1 0 1;
    buy_stock_test
      "testing to see that buy method works after buying the same stock"
      bob 1 4 5;
    buy_stock_test
      "testing to see that buy method works after buying the different \
       stock"
      bob 2 1 6;
    sell_stock_test
      "testing to see that sell method works with one stock" bob 1 1 5;
    sell_stock_test
      "testing to see that sell method works after selling the same \
       stock"
      bob 1 3 2;
    sell_stock_test
      "testing to see that sell method works after selling the \
       different stock"
      bob 2 1 1;
    sell_stock_test
      "testing to see that sell method returns a message when you sell \
       too much stock\n\
      \      selling the same  stock" bob 1 10 1;
    sell_stock_test
      "testing to see that sell method when you sell no stock\n\
      \       no stock\n"
      bob 1 0 1;
    buy_index_test "testing to see that buy method works with one index"
      bob "S&P500" 1 1;
    buy_index_test
      "testing to see that buy method when you buy no index\n       "
      bob "S&P500" 0 1;
    buy_index_test
      "testing to see that buy method works after buying the same index"
      bob "S&P500" 3 4;
    sell_index_test
      "testing to see that sell method works after\n\
      \      selling the same  stock" bob 1 0 3;
    sell_index_test
      "testing to see that sell method returns a message when you sell \
       too much index\n\
      \      selling the same  stock" bob 10 0 3;
    sell_index_test
      "testing to see that sell method when you sell no stock\n\
      \       no stock\n"
      bob 0 0 3;
    buy_stock_test
      "testing to see that buy method does not buy when you do not \
       have enough cash"
      bob 1 9999999999999 1;
    buy_index_test
      "testing to see that buy index method does not buy when you to \
       buy negative shares have enough cash"
      bob "S&P500" (-1) 3;
    sell_index_test
      "testing to see that buy index method does not buy when you try \
       to sell negative shares"
      bob (-1) 0 3;
    buy_stock_test
      "testing to see that buy stock method does not buy when you to \
       buy negative shares have enough cash"
      bob 0 (-1) 1;
    sell_stock_test
      "testing to see that buy stock method does not buy when you try \
       to sell negative shares"
      bob 0 (-1) 1;
  ]

let suite =
  "test suite 1"
  >::: List.flatten [ cd_math_tests; cd_tests; stock_tests; user_tests ]

let _ = run_test_tt_main suite
