open Stock
include Init
open User
open Portfolio
open Cd_history
open Cd

type invest = string list

type command =
  | Buy_S of invest
  | Sell_S of invest
  | Cash
  | Networth
  (*| My_stockhistory*)
  | Checkstock of invest
  | Help
  | BuyCD of invest
  | SellCD of invest

exception EmptyCommand

exception BadCommand

let parse str =
  let lst = String.split_on_char ' ' str in
  match lst with
  | [] -> raise EmptyCommand
  | [ "" ] -> raise EmptyCommand
  | h :: invest ->
      if h = "" then raise EmptyCommand
      else if h = "cash" then Cash
      else if h = "networth" then Networth
      else if h = "help" then Help
      else if h = "sell_s" && invest <> [ "" ] then Sell_S invest
      else if h = "buy_s" && invest <> [ "" ] then Buy_S invest
      else if h = "buy_cd" && invest <> [ "" ] then BuyCD invest
      else if h = "sell_cd" && invest <> [ "" ] then SellCD invest
      else if h = "checkstock" && invest <> [ "" ] then
        Checkstock invest
        (*else if h = "my_stockhistory" then My_stockhistory*)
      else raise BadCommand

let rec legal list symb =
  match list with
  | [] -> raise Not_found
  | h :: t -> if Stock.get_ticker h = symb then h else legal t symb

let rec legal_stock_history list symb =
  match list with
  | [] -> raise Not_found
  | h :: t ->
      if Stock_history.get_ticker h = symb then h
      else legal_stock_history t symb

let checklegalterm t =
  if t = 6 then SixMonths
  else if t = 12 then OneYear
  else if t = 36 then ThreeYears
  else raise BadCommand

let view com u =
  try
    match com with
    | BuyCD invest ->
        let amt = float_of_string (List.hd invest) in
        if amt < 1000. then print_string "Amount not sufficient \n"
        else
          let term = int_of_string (List.nth invest 1) in
          let cdterm = checklegalterm term in
          let p = User.getportfolio u in
          let cd_h = Portfolio.get_cd_history p in
          changecash_buycd u amt;
          Cd_history.buy_cd cd_h amt cdterm;
          print_string "You just bought cd and your cash has changed \n"
    | SellCD invest ->
        let p = User.getportfolio u in
        let cd_h = Portfolio.get_cd_history p in
        let amt = Cd_history.collect_cd_value cd_h 0 in
        changecash_sellcd u amt;
        failwith ""
    | Help ->
        print_string
          "Commands to play the game: \n\
          \          cash to view cash, \n\
          \          networth to check your networth, \n\
          \          buy to buy stocks, \n\
          \          sell to sell stocks, \n\
          \          s to view the current stock market. \n"
    | Cash ->
        let c = string_of_float (User.get_cash u) in
        print_string ("Your current cash is " ^ c ^ "\n")
    | Networth ->
        let n = string_of_float (User.get_net_worth u Init.stocks) in
        print_string ("Your current networth is " ^ n ^ "\n")
    | Buy_S invest ->
        let s = List.hd invest in
        (*let g = legal_stock_history new_stock_history s in*)
        let n = int_of_string (List.nth invest 1) in
        let st = legal stocks s in
        if
          User.get_cash u -. (float n *. Stock.get_current_price st)
          <= 0.0
        then
          print_string
            "You do not have enough cash to purchase this stock \n"
        else (
          User.buy s n u st;
          print_string
            "You just bought stocks and your cash has changed \n" )
    | Sell_S invest ->
        let s = List.hd invest in
        (*let g = legal_stock_history new_stock_history s in*)
        let n = int_of_string (List.nth invest 1) in
        let user_portfolio = User.getportfolio u in
        let st = legal stocks s in
        if
          Stock_history.get_shares
            (User.legal_stock_history
               (Portfolio.get_stock_history user_portfolio)
               s)
          < n
        then print_string "You do not have enough shares \n"
        else (
          User.sell s n u st;
          print_string "You just sold shares \n" )
    | Checkstock invest ->
        let s = List.hd invest in
        let st = legal stocks s in
        let b = legal_stock_history stock_history_lst s in
        let c = string_of_float (User.checkstock st b) in
        if float_of_string c < 0. then
          print_string
            ("Your stocks currently has a loss $" ^ c ^ " dollars \n")
        else
          print_string
            ("Your stocks currently has a gain $" ^ c ^ " dollars \n")
    (*| My_stockhistory -> let n = string_of_float (User.get_net_worth u
      Init.stocks) in print_string ("Your current networth is " ^ n ^
      "\n")*)
  with Not_found ->
    print_string "Invalid stock not found in market. \n"
