type t = {
  stock_ticker : string;
  mutable shares : int;
  mutable buy_in_prices : (float * int) list;
}

let create_stock_history t =
  { stock_ticker = t; shares = 0; buy_in_prices = [] }

let get_ticker sh = sh.stock_ticker

let get_shares sh = sh.shares

let get_buy_in_prices sh = sh.buy_in_prices

let buy sh price n =
  sh.buy_in_prices <- (price, n) :: sh.buy_in_prices;
  sh.shares <- get_shares sh + n;
  sh
