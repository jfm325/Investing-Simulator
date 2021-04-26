type i = {
  index_ticker : string;
  mutable shares : int;
  mutable buy_in_prices : (float * int) list;
}

let create_index_history i =
  { index_ticker = i; shares = 0; buy_in_prices = [] }

let get_ticker sh = sh.index_ticker

let get_shares sh = sh.shares

let get_buy_in_prices sh = sh.buy_in_prices

let buy sh price n =
  sh.buy_in_prices <- (price, n) :: sh.buy_in_prices;
  sh.shares <- get_shares sh + n;
  sh

let rec update_sell sh n =
  match sh.buy_in_prices with
  | [] -> sh.buy_in_prices <- []
  | (k, v) :: t ->
      if v <= n then (
        sh.buy_in_prices <- t;
        update_sell sh (n - v) )
      else sh.buy_in_prices <- (k, v - n) :: t

let sell sh n =
  update_sell sh n;
  sh.shares <- get_shares sh - n
