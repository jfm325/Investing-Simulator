open Stock_history
open Index_history
open Real_estate_history

type t = {
  mutable stock_history : Stock_history.t list;
  (* mutable cd_history *)
  mutable index_history : Index_history.i list;
  mutable re_history : Real_estate_history.r list;
  mutable cd_history : Cd_history.t;
}

let get_re_history_size u = List.length u.re_history

let get_stock_history_size u = List.length u.stock_history

let get_index_history_size u = List.length u.index_history

let get_stock_history p = p.stock_history

let get_index_history p = p.index_history

let get_re_history p = p.re_history

let get_cd_history p = p.cd_history

let create_portfolio sh i cd_h re =
  {
    stock_history = sh;
    index_history = i;
    cd_history = cd_h;
    re_history = re;
  }

let buy_stock portfolio stock n =
  let ticker = Stock.get_ticker stock in
  let price = Stock.get_current_price stock in
  let f sh =
    if ticker = Stock_history.get_ticker sh then
      Stock_history.buy sh price n
    else sh
  in
  create_portfolio
    (List.map f portfolio.stock_history)
    portfolio.index_history portfolio.cd_history portfolio.re_history

let buy_index portfolio stock n =
  let ticker = Stock.get_ticker stock in
  let price = Stock.get_current_price stock in
  let f sh =
    if ticker = Index_history.get_ticker sh then
      Index_history.buy sh price n
    else sh
  in
  create_portfolio portfolio.stock_history
    (List.map f portfolio.index_history)
    portfolio.cd_history portfolio.re_history

let buy_re portfolio stock n =
  let ticker = Stock.get_ticker stock in
  let price = Stock.get_current_price stock in
  let f sh =
    if ticker = Real_estate_history.get_ticker sh then
      Real_estate_history.buy sh price n
    else sh
  in
  create_portfolio portfolio.stock_history portfolio.index_history
    portfolio.cd_history
    (List.map f portfolio.re_history)
