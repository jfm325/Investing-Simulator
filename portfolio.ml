open Stock_history

type t = {
  mutable stock_history : Stock_history.t list
}
let get_stock_history s = s.stock_history
let create_portfolio sh = {
  stock_history = sh
}

let buy_stock portfolio stock n = 
  let ticker = Stock.get_ticker stock in
  let price = Stock.get_current_price stock in
  let f sh = 
    if (ticker = Stock_history.get_ticker sh) then 
      Stock_history.buy sh price n
    else sh
  in
  create_portfolio (List.map f portfolio.stock_history)