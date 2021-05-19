open OUnit2
open Stock
open User
open Interaction
open Cd_history
open Init

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

(*let interaction_tests = [ (* Added tests for the Interaction module
  here *) ( "Testing for buying shares" >:: fun _ -> assert_equal (Buy [
  "COKE"; "50" ]) (parse "buy COKE 50") ); ( "Testing for selling
  shares" >:: fun _ -> assert_equal (Sell [ "COKE"; "50" ]) (parse "sell
  COKE 50") ); ("Testing for cash" >:: fun _ -> assert_equal Cash (parse
  "cash")); ( "Testing for networth" >:: fun _ -> assert_equal Networth
  (parse "networth") ); ( "Testing for Empty" >:: fun _ -> assert_raises
  EmptyCommand (fun () -> parse "") ); ( "Testing for Empty" >:: fun _
  -> assert_raises EmptyCommand (fun () -> parse " ") ); ( "Testing for
  Malformed" >:: fun _ -> assert_raises BadCommand (fun () -> parse "hi
  i am confused") ); ] *)
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

let the_cash_test test_name (user : User.t) (cash : float) =
  test_name >:: fun _ ->
  assert_equal cash (User.get_cash user) ~printer:string_of_float

let the_cash_test test_name (user : User.t) (cash : float) =
  test_name >:: fun _ ->
  assert_equal cash (User.get_cash user) ~printer:string_of_float

let the_buy_stock_test test_name (user : User.t) (stock_name : string)
    (shares : int) (length : int) =
  test_name >:: fun _ ->
  Stock.update_current_prices stocks (Game.get_start_time ());
  User.buy stock_name shares user (legal stocks stock_name);
  assert_equal length
    (User.get_length_stock_history
       (Portfolio.get_stock_history (User.getportfolio user))
       0)
    ~printer:string_of_int

let the_buy_index_test test_name (user : User.t) (stock_name : string)
    (shares : int) (length : int) =
  test_name >:: fun _ ->
  Stock.update_current_prices index (Game.get_start_time ());
  User.buy_index stock_name shares user (legal index stock_name);
  assert_equal length
    (User.get_length_index_history
       (Portfolio.get_index_history (User.getportfolio user))
       0)
    ~printer:string_of_int

let the_buy_re_test test_name (user : User.t) (stock_name : string)
    (shares : int) (length : int) =
  test_name >:: fun _ ->
  Stock.update_current_prices re (Game.get_start_time ());
  User.buy_re stock_name shares user (legal re stock_name);
  assert_equal length
    (User.get_length_re_history
       (Portfolio.get_re_history (User.getportfolio user))
       0)
    ~printer:string_of_int

let the_sell_stock_test test_name (user : User.t) (stock_name : string)
    (shares : int) (length : int) =
  test_name >:: fun _ ->
  Stock.update_current_prices stocks (Game.get_start_time ());
  User.sell stock_name shares user (legal stocks stock_name);
  assert_equal length
    (User.get_length_stock_history
       (Portfolio.get_stock_history (User.getportfolio user))
       0)
    ~printer:string_of_int

let the_sell_index_test test_name (user : User.t) (stock_name : string)
    (shares : int) (length : int) =
  test_name >:: fun _ ->
  Stock.update_current_prices index (Game.get_start_time ());
  User.sell_index stock_name shares user (legal index stock_name);
  assert_equal length
    (User.get_length_index_history
       (Portfolio.get_index_history (User.getportfolio user))
       0)
    ~printer:string_of_int

let the_sell_re_test test_name (user : User.t) (stock_name : string)
    (shares : int) (length : int) =
  test_name >:: fun _ ->
  Stock.update_current_prices re (Game.get_start_time ());
  User.sell_re stock_name shares user (legal re stock_name);
  assert_equal length
    (User.get_length_re_history
       (Portfolio.get_re_history (User.getportfolio user))
       0)
    ~printer:string_of_int

let the_networth_test test_name (user : User.t) (networth : float) =
  test_name >:: fun _ ->
  Stock.update_current_prices stocks (Game.get_start_time ());
  Stock.update_current_prices index (Game.get_start_time ());
  Stock.update_current_prices re (Game.get_start_time ());
  assert_equal networth
    (User.get_net_worth user Init.stocks index re)
    ~printer:string_of_float

let (user_tests : OUnit2.test list) =
  let bob =
    User.create_user 20000. stock_history_lst index_history_lst
      cd_history re_history_lst
  in
  [
    the_cash_test "testing for default cash" bob 20000.;
    the_networth_test "testing for networth with no purchased stocks"
      bob 20000.;
    the_buy_stock_test
      "testing to see that buy method works with one stock" bob "COKE" 1
      1;
    the_buy_stock_test
      "testing to see that buy method works after buying the same stock"
      bob "COKE" 4 5;
    the_buy_stock_test
      "testing to see that buy method works after buying the different \
       stock"
      bob "AAPL" 1 6;
    the_sell_stock_test
      "testing to see that sell method works with one stock" bob "COKE"
      1 5;
    the_sell_stock_test
      "testing to see that sell method works after selling the same \
       stock"
      bob "COKE" 3 2;
    the_sell_stock_test
      "testing to see that sell method works after selling the \
       different stock"
      bob "AAPL" 1 1;
    the_buy_index_test
      "testing to see that buy method works with one index" bob "SPY" 1
      1;
    the_buy_index_test
      "testing to see that buy method works after buying the same index"
      bob "SPY" 3 4;
    the_sell_index_test
      "testing to see that sell method works after selling the same \
       stock"
      bob "SPY" 1 3;
    the_buy_re_test
      "testing to see that buy method works with one real estate stock"
      bob "SPY" 1 1;
    the_buy_re_test
      "testing to see that buy method works after buying\n\
      \      the same real  estate stock" bob "SPY" 3 4;
    the_sell_re_test
      "testing to see that sell method works after selling the same \
       real estate stock"
      bob "SPY" 2 2;
  ]

let suite = "test suite 1" >::: List.flatten [ stock_tests; user_tests ]

let _ = run_test_tt_main suite
