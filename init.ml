open Stock
open Stock_history
open User
open Cd_history

(* Will later randomize selection of stocks *)
let stocks : Stock.t list =
  [
    Stock.create_stock "Coke" "COKE" "coke1995.txt";
    Stock.create_stock "Apple" "AAPL" "aapl1995.txt";
    Stock.create_stock "Microsoft Corporation" "MSFT" "msft1995.txt";
  ]

let coke_history = Stock_history.create_stock_history "COKE"

let aapl_history = Stock_history.create_stock_history "AAPL"

let msft_history = Stock_history.create_stock_history "MSFT"

let stock_history_lst = [ coke_history; aapl_history; msft_history ]

let index_coke_history = Index_history.create_index_history "COKE"

let index_history_lst = [ index_coke_history ]

let cd_history = Cd_history.create_cd_history "cd_rates1995.txt"

(* Will later send in stock history to user *)
let user : User.t =
  User.create_user 20000. stock_history_lst index_history_lst cd_history

let intro_string =
  "\n\
   *************************\n\
   Stock Simulation Game\n\
   *************************\n"

let instructions =
  "You will be able to buy/sell individual stock shares\n\
   with the goal of making the most of your salary.\n\n\
   Commands:\n\
   See stocks:         s\n\
   See networth:       networth\n\
   See on-hand cash:   cash\n\
   Buy shares:         buy [ticker_symbol] [# of shares]\n\
   Sell shares:        sell [ticker_symbol] [# of shares]\n\
   View commands again: help\n\n"
