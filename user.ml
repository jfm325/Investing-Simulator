open Stock
open Portfolio
open Stock_history

type t = {
  mutable portfolio : Portfolio.t;
  mutable net_worth : float;
  mutable cash : float;
}

let create_user c sh_lst i_lst cd_h =
  {
    net_worth = c;
    cash = c;
    portfolio = Portfolio.create_portfolio sh_lst i_lst cd_h;
  }

let get_cash u = u.cash

let get_portfolio u = u.portfolio

let remove_cash u amt = u.cash <- u.cash -. amt

let add_cash u amt = u.cash <- u.cash +. amt

let add_income_cash u amt = u.cash <- u.cash +. amt

(* [calculate_networth_stock sh_lst stock_lst acc index] is the helper
   function to calculate value of shares owned in the list of stock
   histories [sh_lst]. *)
let rec calculate_networth_stock sh_lst stock_lst acc index =
  match sh_lst with
  | [] -> acc
  | sh :: t ->
      let stock = List.nth stock_lst index in
      let curr_price = Stock.get_current_price stock in
      let shares_owned = float_of_int (Stock_history.get_shares sh) in
      let value = shares_owned *. curr_price in
      calculate_networth_stock t stock_lst (acc +. value) index

(* [calculate_networth_index ih_lst index_fund_lst acc index] is the
   helper function to calculate value of shares owned in the list of
   index fund histories [ih_lst]. *)
let rec calculate_networth_index ih_lst index_fund_lst acc index =
  match ih_lst with
  | [] -> acc
  | ih :: t ->
      let index_fund = List.nth index_fund_lst index in
      let curr_price = Stock.get_current_price index_fund in
      let shares_owned = float_of_int (Index_history.get_shares ih) in
      let value = shares_owned *. curr_price in
      calculate_networth_index t index_fund_lst (acc +. value) index

let get_net_worth u stocks_lst indexfunds_lst =
  let sh_lst = Portfolio.get_stock_history u.portfolio in
  let ih_list = Portfolio.get_index_history u.portfolio in
  let cd_h = Portfolio.get_cd_history u.portfolio in
  let cds_value = Cd_history.get_investment_value cd_h in
  let index_value =
    calculate_networth_index ih_list indexfunds_lst 0. 0
  in
  let stocks_value = calculate_networth_stock sh_lst stocks_lst 0. 0 in
  u.net_worth <- u.cash +. stocks_value +. index_value +. cds_value;
  u.net_worth

let change_cash_buy u shares stock =
  let curr_price = Stock.get_current_price stock in
  let value = float shares *. curr_price in
  u.cash <- u.cash -. value

let change_cash_sell u shares stock =
  let curr_price = Stock.get_current_price stock in
  let value = float shares *. curr_price in
  u.cash <- u.cash +. value

let buy_stock shares u stock =
  if shares > 0 then (
    u.portfolio <- Portfolio.buy_stock u.portfolio stock shares;
    change_cash_buy u shares stock;
    print_string
      "You just bought shares of stock and your cash has changed\n" )
  else print_string "TRANSACTION ERROR: Invalid number of shares.\n"

let sell_stock shares u stock index =
  let p = u.portfolio in
  let sh_lst = Portfolio.get_stock_history p in
  let num_of_stocks = List.length sh_lst in
  if index < 0 || index >= num_of_stocks then
    print_string "TRANSACTION ERROR: Invalid index fund number.\n"
  else
    let stock_hist = List.nth sh_lst index in
    let shares_owned = Stock_history.get_shares stock_hist in
    if shares < 0 || shares > shares_owned then
      print_string "TRANSACTION ERROR: Invalid number of shares.\n"
    else (
      Stock_history.sell stock_hist shares;
      change_cash_sell u shares stock;
      print_string "You just sold shares \n" )

let buy_index shares user index_fund =
  user.portfolio <- Portfolio.buy_index user.portfolio index_fund shares;
  change_cash_buy user shares index_fund

let sell_index shares user index_fund index =
  let p = user.portfolio in
  let index_hist_lst = Portfolio.get_index_history p in
  let num_of_index_funds = List.length index_hist_lst in
  if index < 0 || index >= num_of_index_funds then
    print_string "TRANSACTION ERROR: Invalid index fund number.\n"
  else
    let index_fund_hist = List.nth index_hist_lst index in
    let shares_owned = Index_history.get_shares index_fund_hist in
    if shares < 0 || shares > shares_owned then
      print_string "TRANSACTION ERROR: You do not have enough shares\n"
    else (
      Index_history.sell index_fund_hist shares;
      change_cash_sell user shares index_fund;
      print_string "You just sold shares \n" )

let buy_cd u amt t =
  let cd_h = Portfolio.get_cd_history u.portfolio in
  let cds_owned = Cd_history.get_cds_owned cd_h in
  if cds_owned >= 3 then
    raise (Failure "You can only have 3 cds at a time.")
  else (
    remove_cash u amt;
    Cd_history.buy_cd cd_h amt t )

let sell_cd u i =
  let cd_h = Portfolio.get_cd_history u.portfolio in
  let amt = Cd_history.collect_cd_value cd_h i in
  let cds_owned = Cd_history.get_cds_owned cd_h in
  if i >= 0 && i < cds_owned then (
    Cd_history.remove_cd cd_h i;
    add_cash u amt )
  else print_endline "Invalid cd to sell."

(* [get_buy_in_value lst acc] is the helper function to get buy-in value
   of stocks and index funds. *)
let rec get_buy_in_value lst acc =
  match lst with
  | [] -> acc
  | (price, shares) :: t ->
      let value = price *. float_of_int shares in
      get_buy_in_value t (acc +. value)

(* [lookup_shares_owned lst acc] is the helper function to get shares
   owned for stocks and index funds. *)
let rec lookup_shares_owned lst acc =
  match lst with
  | [] -> acc
  | (_, shares) :: t -> lookup_shares_owned t (acc + shares)

let get_stocks_pl stock sh =
  let buy_in_prices = Stock_history.get_buy_in_prices sh in
  let shares_owned =
    float_of_int (lookup_shares_owned buy_in_prices 0)
  in
  let current_value = Stock.get_current_price stock *. shares_owned in
  let value_at_buy_in = get_buy_in_value buy_in_prices 0. in
  current_value -. value_at_buy_in

let get_index_pl stock ih =
  let buy_in_prices = Index_history.get_buy_in_prices ih in
  let shares_owned =
    float_of_int (lookup_shares_owned buy_in_prices 0)
  in
  let current_value = Stock.get_current_price stock *. shares_owned in
  let value_at_buy_in = get_buy_in_value buy_in_prices 0. in
  current_value -. value_at_buy_in

(* FOR TESTING PURPOSES *)

let rec get_shares_sh_lst sh_lst acc =
  match sh_lst with
  | [] -> acc
  | sh :: t ->
      let shares = Stock_history.get_shares sh in
      get_shares_sh_lst t (acc + shares)

let rec get_shares_ih_lst sh_lst acc =
  match sh_lst with
  | [] -> acc
  | ih :: t ->
      let value = Index_history.get_shares ih in
      get_shares_ih_lst t (acc + value)
