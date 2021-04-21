open Stock
open Portfolio
include Init

(* type sh = { stock : string; mutable shares : int; mutable
   buy_in_prices : (float * int) list; } mutable buy_in_prices : float
   list; } *)

type t = {
  mutable portfolio : Portfolio.t;
  mutable net_worth : float;
  mutable cash : float;
  (* mutable stock_companies : sh list; *)
  mutable string_stock_companies : string list;
}

let rec legal list symb =
  match list with
  | [] -> raise Not_found
  | h :: t -> if Stock.get_ticker h = symb then h else legal t symb

let rec calculate_net_worth (u : Stock_history.t list) counter : float =
  match u with
  | [] -> counter
  | h :: t ->
      calculate_net_worth t
        ( counter
        +. float (Stock_history.get_shares h)
           *. Stock.get_current_price
                (legal stocks (Stock_history.get_ticker h)) )

let get_net_worth u =
  u.net_worth <-
    u.cash
    +. calculate_net_worth (Portfolio.get_stock_history u.portfolio) 0.;
  u.net_worth

let get_cash u = u.cash

let default_user (set_amount : float) =
  {
    net_worth = set_amount;
    cash = set_amount;
    portfolio =
      Portfolio.create_portfolio
        [ Stock_history.create_stock_history "Demo" ];
    string_stock_companies = [];
  }

let rec find x lst =
  match lst with
  | [] -> raise (Failure "Not Found")
  | h :: t -> if x = h then 0 else 1 + find x t

let change_cash_buy (s : t) (shares : int) (stock_t : Stock.t) =
  s.cash <- s.cash -. (float shares *. Stock.get_price stock_t 0)

let buy (stock_name : string) (shares : int) (user : t)
    (stock : Stock.t) =
  user.portfolio <- Portfolio.buy_stock user.portfolio stock shares
