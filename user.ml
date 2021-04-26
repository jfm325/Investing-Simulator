open Stock
open Portfolio
open Stock_history

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

let rec legal_stock_history list symb =
  match list with
  | [] -> raise Not_found
  | h :: t ->
      if Stock_history.get_ticker h = symb then h
      else legal_stock_history t symb

let create_user c sh_lst cd_h =
  {
    net_worth = c;
    cash = c;
    portfolio = Portfolio.create_portfolio sh_lst cd_h;
    string_stock_companies = [];
  }

let getportfolio u = u.portfolio

let rec find x lst =
  match lst with
  | [] -> raise (Failure "Not Found")
  | h :: t -> if x = h then 0 else 1 + find x t

let change_cash_buy (s : t) (shares : int) (stock_t : Stock.t) =
  s.cash <- s.cash -. (float shares *. Stock.get_current_price stock_t)

let change_cash_sell (s : t) (shares : int) (stock_t : Stock.t) =
  s.cash <- s.cash +. (float shares *. Stock.get_current_price stock_t)

let buy (stock_name : string) (shares : int) (user : t)
    (stock : Stock.t) =
  user.portfolio <- Portfolio.buy_stock user.portfolio stock shares;
  change_cash_buy user shares stock

let sell (stock_name : string) (shares : int) (user : t)
    (stock : Stock.t) =
  let p = user.portfolio in
  Stock_history.sell
    (legal_stock_history (Portfolio.get_stock_history p) stock_name)
    shares;
  change_cash_sell user shares stock

let rec lookup (k : (float * int) list) (acc : float) =
  match k with
  | [] -> acc
  | (k, v) :: t -> lookup t (acc +. (k *. float_of_int v))

let rec lookup_shares (k : (float * int) list) (acc : int) =
  match k with [] -> acc | (k, v) :: t -> lookup_shares t (acc + v)

let checkstock (stock_t : Stock.t) (stock : Stock_history.t) =
  Stock.get_current_price stock_t
  *. float_of_int
       (lookup_shares (Stock_history.get_buy_in_prices stock) 0)
  -. lookup (Stock_history.get_buy_in_prices stock) 0.

(*let get_stock_history stockhistory: Stock_history.t = *)
