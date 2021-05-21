type num_of_shares = int

type price = float

type t = {
  stock_ticker : string;
  mutable shares : int;
  mutable buy_in_prices : (price * num_of_shares) list;
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

let rec remove_shares_from_hist sh n =
  match sh.buy_in_prices with
  | [] -> sh.buy_in_prices <- []
  | (price, shares) :: t ->
      if shares < n then (
        sh.buy_in_prices <- t;
        remove_shares_from_hist sh (n - shares) )
      else sh.buy_in_prices <- (price, shares - n) :: t

let sell sh n =
  remove_shares_from_hist sh n;
  sh.shares <- get_shares sh - n
