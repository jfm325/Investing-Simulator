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

let rec index_calculate_net_worth sh_lst stocks_lst counter =
  match sh_lst with
  | [] -> counter
  | h :: t ->
      let stock = legal stocks_lst (Index_history.get_ticker h) in
      let value =
        float_of_int (Index_history.get_shares h)
        *. Stock.get_current_price stock
      in
      index_calculate_net_worth t stocks_lst (counter +. value)

let rec re_calculate_net_worth sh_lst stocks_lst counter =
  match sh_lst with
  | [] -> counter
  | h :: t ->
      let stock = legal stocks_lst (Real_estate_history.get_ticker h) in
      let value =
        float_of_int (Real_estate_history.get_shares h)
        *. Stock.get_current_price stock
      in
      re_calculate_net_worth t stocks_lst (counter +. value)

let get_net_worth u stocks_lst index_lst re_lst =
  let sh_lst = Portfolio.get_stock_history u.portfolio in
  let in_list = Portfolio.get_index_history u.portfolio in
  let re_list = Portfolio.get_re_history u.portfolio in
  let cd_h = Portfolio.get_cd_history u.portfolio in
  let cds_value = Cd_history.get_investment_value cd_h in
  let index_value = index_calculate_net_worth in_list index_lst 0. in
  let stocks_value = calculate_net_worth sh_lst stocks_lst 0. in
  let re_value = re_calculate_net_worth re_list re_lst 0. in
  u.net_worth <-
    u.cash +. stocks_value +. index_value +. cds_value +. re_value;
  u.net_worth

let get_cash u = u.cash

let rec legal_index_history list symb =
  match list with
  | [] -> raise Not_found
  | h :: t ->
      if Index_history.get_ticker h = symb then h
      else legal_index_history t symb

let rec legal_re_history list symb =
  match list with
  | [] -> raise Not_found
  | h :: t ->
      if Real_estate_history.get_ticker h = symb then h
      else legal_re_history t symb

let rec legal_stock_history list symb =
  match list with
  | [] -> raise Not_found
  | h :: t ->
      if Stock_history.get_ticker h = symb then h
      else legal_stock_history t symb

let create_user c sh_lst i_lst cd_h re_list =
  {
    net_worth = c;
    cash = c;
    portfolio = Portfolio.create_portfolio sh_lst i_lst cd_h re_list;
    string_stock_companies = [];
  }

let getportfolio u = u.portfolio

let getindexsize u = Portfolio.get_index_history

let getresize u = Portfolio.get_re_history

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
let buy_index (stock_name : string) (shares : int) (user : t)
    (stock : Stock.t) =
  user.portfolio <- Portfolio.buy_index user.portfolio stock shares;
  change_cash_buy user shares stock

let sell_index (stock_name : string) (shares : int) (user : t)
    (stock : Stock.t) =
  let p = user.portfolio in
  Index_history.sell
    (legal_index_history (Portfolio.get_index_history p) stock_name)
    shares;
  change_cash_sell user shares stock

let checkindex (stock_t : Stock.t) (stock : Index_history.i) =
  Stock.get_current_price stock_t
  *. float_of_int
       (lookup_shares (Index_history.get_buy_in_prices stock) 0)
  -. lookup (Index_history.get_buy_in_prices stock) 0.

let buy_re (stock_name : string) (shares : int) (user : t)
    (stock : Stock.t) =
  user.portfolio <- Portfolio.buy_re user.portfolio stock shares;
  change_cash_buy user shares stock

let sell_re (stock_name : string) (shares : int) (user : t)
    (stock : Stock.t) =
  let p = user.portfolio in
  Real_estate_history.sell
    (legal_re_history (Portfolio.get_re_history p) stock_name)
    shares;
  change_cash_sell user shares stock

let checkre (stock_t : Stock.t) (stock : Real_estate_history.r) =
  Stock.get_current_price stock_t
  *. float_of_int
       (lookup_shares (Real_estate_history.get_buy_in_prices stock) 0)
  -. lookup (Real_estate_history.get_buy_in_prices stock) 0.

let changecash_buycd s f = s.cash <- s.cash -. f

let changecash_sellcd s f = s.cash <- s.cash +. f

let buy_cd u amt t =
  let cd_h = Portfolio.get_cd_history u.portfolio in
  let cds_owned = Cd_history.get_cds_owned cd_h in
  if cds_owned >= 3 then
    raise (Failure "You can only have 3 cds at a time.")
  else (
    changecash_buycd u amt;
    Cd_history.buy_cd cd_h amt t )

let sell_cd u i =
  let cd_h = Portfolio.get_cd_history u.portfolio in
  let amt = Cd_history.collect_cd_value cd_h i in
  let cds_owned = Cd_history.get_cds_owned cd_h in
  if i >= 0 && i < cds_owned then (
    Cd_history.remove_cd cd_h i;
    changecash_sellcd u amt )
  else raise (Failure "Invalid cd to sell.")
