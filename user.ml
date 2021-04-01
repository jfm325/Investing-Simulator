open Stock

type sh = {
  stock : string;
  mutable shares : int;
  mutable buy_in_prices : float list;
}

type u = {
  mutable net_worth : float;
  mutable cash : float;
  mutable stock_companies : sh list;
  mutable string_stock_companies : string list;
}

let get_net_worth u = u.net_worth

let get_cash u = u.cash

let get_stock_companies u = u.stock_companies

let get_stock u = u.stock

let get_shares u = u.shares

let default_user (set_amount : float) =
  {
    net_worth = set_amount;
    cash = set_amount;
    stock_companies = [];
    string_stock_companies = [];
  }

(* method to buy stock first check to see if stock is in the
   stockhistory list and if it is then it share gets updated else check
   if stock is in the official stock board and then creates a new
   stockhistory record and appends it to the stock history list*)
let rec find x lst =
  match lst with
  | [] -> raise (Failure "Not Found")
  | h :: t -> if x = h then 0 else 1 + find x t

let create_stock_history name_stock name_shares name_buy_in_price =
  {
    stock = name_stock;
    shares = name_shares;
    buy_in_prices = [ name_buy_in_price ];
  }

let change_cash_buy (s : u) (shares : int) (stock_t : Stock.t) =
  s.cash <- s.cash -. (float shares *. Stock.get_price stock_t 0)

let buy (stock : string) (shares : int) (firstuser : u)
    (stock_t : Stock.t) =
  if List.mem stock firstuser.string_stock_companies then (
    (List.nth firstuser.stock_companies
       (find stock firstuser.string_stock_companies)).shares <-
      (List.nth firstuser.stock_companies
         (find stock firstuser.string_stock_companies))
        .shares + shares;
    (List.nth firstuser.stock_companies
       (find stock firstuser.string_stock_companies)).buy_in_prices <-
      (List.nth firstuser.stock_companies
         (find stock firstuser.string_stock_companies))
        .buy_in_prices
      @ [ Stock.get_price stock_t 0 ];
    change_cash_buy firstuser shares stock_t )
  else (
    firstuser.stock_companies <-
      firstuser.stock_companies
      @ [
          create_stock_history stock shares (Stock.get_price stock_t 0);
        ];
    firstuser.string_stock_companies <-
      firstuser.string_stock_companies @ [ stock ];
    change_cash_buy firstuser shares stock_t )
