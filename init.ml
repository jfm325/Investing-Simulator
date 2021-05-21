open Stock
open Stock_history
open User
open Cd_history
open Bot

(* Will later randomize selection of stocks *)
let stocks : Stock.t list =
  [
    Stock.create_stock "Coke" "COKE" "coke1995.txt";
    Stock.create_stock "Apple" "AAPL" "aapl1995.txt";
    Stock.create_stock "Microsoft Corporation" "MSFT" "msft1995.txt";
  ]

let index : Stock.t list =
  [
    Stock.create_stock "SPY" "SPY" "spy_index1995.txt";
    Stock.create_stock "RE" "RE" "spy_index1995.txt";
  ]

<<<<<<< HEAD
=======
(* let re : Stock.t list = [ Stock.create_stock "SPY" "SPY"
   "spy_index1995.txt" ] *)

>>>>>>> refs/remotes/origin/master
let coke_history = Stock_history.create_stock_history "COKE"

let aapl_history = Stock_history.create_stock_history "AAPL"

let msft_history = Stock_history.create_stock_history "MSFT"

let stock_history_lst = [ coke_history; aapl_history; msft_history ]

let index_spy_history = Index_history.create_index_history "SPY"

<<<<<<< HEAD
let index_history_lst = [ index_spy_history ]

=======
let real_estate_history = Index_history.create_index_history "RE"

let index_history_lst = [ index_spy_history; real_estate_history ]

>>>>>>> refs/remotes/origin/master
let cd_history = Cd_history.create_cd_history "cd_rates1995.txt"

(* Will later send in stock history to user *)
let user : User.t =
  User.create_user 20000. stock_history_lst index_history_lst cd_history

let bot : Bot.t = Bot.create_bot

let bar = "*******************************************"

let shares_str = "Shares: "

let percent_str = "%: "

let ticker_str = "Ticker: "

let prices_str = "Price:  "

let profit_loss_str = "P/L:    "

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
   See specific stock: checkstock [ticker_symbol]\n\
   Buy_index shares:   buy_index [ticker_symbol] [# of shares]\n\
   Sell_index shares:  sell_index [ticker_symbol] [# of shares]\n\
   Buy shares:         buy_s [ticker_symbol] [# of shares]\n\
   Sell shares:        sell_s [ticker_symbol] [# of shares]\n\
   Buy cd:             buy_cd [amt] [term of 1/2/3]\n\
  \ where 1 ~ 6 months, 2 ~ 12 months, 3 ~ 36 months\n\
   Sell cd:            sell_cd [index # in cd_history list] \n\
   View bought cd:     view_cd \n\
   See bot networth:   bot \n\
   View commands again: help\n\n"
