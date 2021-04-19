open Stock
open User

(* Will later randomize selection of stocks *)
let stocks : Stock.t list =
  [
    Stock.create_stock "Coke" "COKE" "coke1995.txt";
    Stock.create_stock "Apple" "AAPL" "aapl1995.txt";
  ]

(* Will later send in stock history to user *)
let user : User.t = User.default_user 20000.

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
   Sell shares:        sell [ticker_symbol] [# of shares]\n\n"
