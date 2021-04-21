open Stock
open Portfolio

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

let rec calculate_net_worth sh_lst stocks_lst counter =
  match sh_lst with
  | [] -> counter
  | h :: t ->
      let stock = legal stocks_lst (Stock_history.get_ticker h) in
      let value =
        float_of_int (Stock_history.get_shares h)
        *. Stock.get_current_price stock
      in
      calculate_net_worth t stocks_lst (counter +. value)

let get_net_worth u stocks_lst =
  let sh_lst = Portfolio.get_stock_history u.portfolio in
  let investment_value = calculate_net_worth sh_lst stocks_lst 0. in
  u.net_worth <- u.cash +. investment_value;
  u.net_worth

let get_cash u = u.cash

let create_user c sh_lst =
  {
    net_worth = c;
    cash = c;
    portfolio = Portfolio.create_portfolio sh_lst;
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
