open Stock
open Stock_history
open User
open Cd_history
open Bot

let path = "data/"

let choose_plane_stock n =
  if n = 0 then
    let ba_stock =
      Stock.create_stock "Plane Co" "BA" (path ^ "ba1995.txt")
    in
    let ba_hist = Stock_history.create_stock_history "BA" in
    (ba_stock, ba_hist)
  else
    let lmt_stock =
      Stock.create_stock "Planes" "LMT" (path ^ "lmt1995.txt")
    in
    let lmt_hist = Stock_history.create_stock_history "LMT" in
    (lmt_stock, lmt_hist)

let choose_energy_stock n =
  if n = 0 then
    let ge_stock =
      Stock.create_stock "Energy Co" "GE" (path ^ "ge1995.txt")
    in
    let ge_hist = Stock_history.create_stock_history "GE" in
    (ge_stock, ge_hist)
  else
    let duk_stock =
      Stock.create_stock "Fuel Co" "DUK" (path ^ "duk1995.txt")
    in
    let duk_hist = Stock_history.create_stock_history "DUK" in
    (duk_stock, duk_hist)

let choose_tech_stock n =
  if n = 0 then
    let aapl_stock =
      Stock.create_stock "Tech Labs" "AAPL" (path ^ "aapl1995.txt")
    in
    let aapl_hist = Stock_history.create_stock_history "AAPL" in
    (aapl_stock, aapl_hist)
  else
    let msft_stock =
      Stock.create_stock "Tech Co" "MSFT" (path ^ "msft1995.txt")
    in
    let msft_hist = Stock_history.create_stock_history "MSFT" in
    (msft_stock, msft_hist)

let choose_oil_stock n =
  if n = 0 then
    let xom_stock =
      Stock.create_stock "OilCo" "XOM" (path ^ "ge1995.txt")
    in
    let xom_hist = Stock_history.create_stock_history "XOM" in
    (xom_stock, xom_hist)
  else
    let mro_stock =
      Stock.create_stock "Oil Co" "MRO" (path ^ "mro1995.txt")
    in
    let mro_hist = Stock_history.create_stock_history "MRO" in
    (mro_stock, mro_hist)

let choose_misc_stock n =
  match n with
  | 0 ->
      let coke_stock =
        Stock.create_stock "Soda" "COKE" (path ^ "coke1995.txt")
      in
      let coke_hist = Stock_history.create_stock_history "COKE" in
      (coke_stock, coke_hist)
  | 1 ->
      let mat_stock =
        Stock.create_stock "Toys Co" "MAT" (path ^ "mat1995.txt")
      in
      let mat_hist = Stock_history.create_stock_history "MAT" in
      (mat_stock, mat_hist)
  | _ ->
      let jnj_stock =
        Stock.create_stock "Med Labs" "JNJ" (path ^ "jnj1995.txt")
      in
      let jnj_hist = Stock_history.create_stock_history "JNJ" in
      (jnj_stock, jnj_hist)

let choose_stocks () =
  Random.self_init ();
  let rand_int1 = Random.int 2 in
  Random.self_init ();
  let rand_int2 = Random.int 2 in
  Random.self_init ();
  let rand_int3 = Random.int 2 in
  Random.self_init ();
  let rand_int4 = Random.int 2 in
  Random.self_init ();
  let rand_int5 = Random.int 3 in
  let stock1 = choose_energy_stock rand_int1 in
  let stock2 = choose_oil_stock rand_int2 in
  let stock3 = choose_plane_stock rand_int3 in
  let stock4 = choose_tech_stock rand_int4 in
  let stock5 = choose_misc_stock rand_int5 in
  [ stock1; stock2; stock3; stock4; stock5 ]

let stocks_and_indexfunds = choose_stocks ()

let stocks : Stock.t list = List.map fst stocks_and_indexfunds

let stock_history_lst = List.map snd stocks_and_indexfunds

let index_funds : Stock.t list =
  [
    Stock.create_stock "S&P500" "S&P500" (path ^ "spy_index1995.txt");
    Stock.create_stock "REAL ESTATE" "REAL ESTATE" (path ^ "amt1999.txt");
  ]

let index_history_lst =
  let index_spy_history = Index_history.create_index_history "S&P500" in
  let index_re_history =
    Index_history.create_index_history "REAL ESTATE"
  in
  [ index_spy_history; index_re_history ]

let cd_history = Cd_history.create_cd_history (path ^ "cd_rates1995.txt")

let user : User.t =
  User.create_user 20000. stock_history_lst index_history_lst cd_history

let bot : Bot.t = Bot.create_bot ()

let bar =
  "*********************************************************************"
  ^ "***************************"

let shares_str = "Shares:\t\t"

let index_fund_str = "Index Fund:\t"

let percent_str = "%" ^ "Change:\t"

let ticker_str = "Name:\t\t"

let prices_str = "Price:\t\t"

let profit_loss_str = "P/L:\t\t"

let intro_string =
  "\n\
   *************************\n\
   Stock Simulation Game\n\
   *************************\n"

let instructions =
  "You will be able to buy/sell individual stock shares\n\
   with the goal of making the most of your salary.\n\n\n\
  \   \n\
   There is also a competing bot so try and beat it by earning a \
   higher networth\n\n\n\
  \   Commands:\n\
   See stocks:         s\n\
   See index:          i\n\
   See cd info :       cd\n\
   See index_funds:    view_index \n\
   See networth:       networth\n\
   See on-hand cash:   cash\n\
   See Portfolio Percentage :  portfolio_percent\n\
   See specific stock: checkstock [ticker_symbol]\n\
   Buy_index:   buy_index [index_fund_#] [# of shares]\n\
   Sell_index:  sell_index [index_fund_#] [# of shares]\n\
   Buy shares:         buy_s [ticker_symbol] [# of shares]\n\
   Sell shares:        sell_s [ticker_symbol] [# of shares]\n\
   Buy cd:             buy_cd [term of 1/2/3] [amt] \n\
  \ where 1 ~ 6 months, 2 ~ 12 months, 3 ~ 36 months\n\
   Sell cd:            sell_cd [cd #] \n\
   View bought cd:     view_cd \n\
   See bot networth:   bot \n\
   View commands again: help\n\n"
