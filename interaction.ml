open Stock
include Init
open User
open Portfolio
open Cd_history
open Cd

type invest = string list

type command =
  | Buy_Index of invest
  | Sell_Index of invest
  | Buy_S of invest
  | Sell_S of invest
  | Cash
  | Networth
  (*| My_stockhistory*)
  | Checkstock of invest
  | Help
  | BuyCD of invest
  | SellCD of invest
  | ViewCD
  | ViewIndex

exception EmptyCommand

exception BadCommand

let print_in indexlist =
  match indexlist with
  | [] -> print_string "You don't own any index funds. \n"
  | h :: t ->
      let s = Index_history.get_shares h in
      print_string
        ("Shares in index funds owned : " ^ string_of_int s ^ "\n")

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
  if t = 1 then SixMonths
  else if t = 2 then OneYear
  else if t = 3 then ThreeYears
  else raise BadCommand

let print_cd his_lst lst count =
  let penalty_warning =
    "** 10% penalty if collected before maturity **"
  in
  let bar = "**********************************************" in
  let num = "Cd:                    " in
  let term = "Term (months):         " in
  let maturity = "Months until Maturity: " in
  let apy = "APY (%):               " in
  let c = 1 in
  let rec print_cd_helper his_lst (lst : Cd.t list) num term mat f c =
    match lst with
    | [] ->
        print_endline bar;
        print_endline num;
        print_endline term;
        print_endline mat;
        print_endline f;
        print_endline bar;
        print_endline penalty_warning
    | h :: t ->
        print_cd_helper his_lst t
          (num ^ string_of_int c ^ "\t")
          (term ^ string_of_int (Cd.get_length h) ^ "\t")
          (mat ^ string_of_int (Cd.months_until_maturity h) ^ "\t")
          (f ^ string_of_float (Cd.get_apy_percentage h) ^ "\t")
          (c + 1)
  in
  print_cd_helper his_lst lst num term maturity apy c

let view com u =
  try
    match com with
    | ViewIndex ->
        let p = User.getportfolio u in
        let in_h = Portfolio.get_index_history p in
        print_in in_h
    | ViewCD ->
        let p = User.getportfolio u in
        let cd_h = Portfolio.get_cd_history p in
        let c_lst = Cd_history.get_cd_lst cd_h in
        print_cd cd_h c_lst 1
    | BuyCD invest ->
        let amt = float_of_string (List.hd invest) in
        if amt < 1000. then print_string "Amount not sufficient \n"
        else
          let term = int_of_string (List.nth invest 1) in
          let cdterm = checklegalterm term in
          User.buy_cd u amt cdterm;
          print_string
            "You just bought a cd and your cash has changed \n"
    | SellCD invest ->
        let n = int_of_string (List.hd invest) in
        let i = n - 1 in
        User.sell_cd u i;
        print_string "You just sold cd and your cash has changed \n"
    | Help -> print_string Init.instructions
    | Cash ->
        let c = string_of_float (User.get_cash u) in
        print_string ("Your current cash is " ^ c ^ "\n")
    | Networth ->
        Stock.update_current_prices stocks (Game.get_start_time ());
        Stock.update_current_prices index (Game.get_start_time ());
        let n =
          string_of_float (User.get_net_worth u Init.stocks index)
        in
        print_string ("Your current networth is " ^ n ^ "\n")
    | Buy_S invest ->
        Stock.update_current_prices stocks (Game.get_start_time ());
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
        Stock.update_current_prices stocks (Game.get_start_time ());
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
    | Buy_Index invest ->
        Stock.update_current_prices index (Game.get_start_time ());
        let s = List.hd invest in
        (*let g = legal_stock_history new_stock_history s in*)
        let n = int_of_string (List.nth invest 1) in
        let st = legal index s in
        if s <> "SPY" then print_string "this is not an index_fund \n"
        else if
          User.get_cash u -. (float n *. Stock.get_current_price st)
          <= 0.0
        then
          print_string
            "You do not have enough cash to purchase this stock \n"
        else (
          User.buy_index s n u st;
          print_string
            "You just bought stocks and your cash has changed \n" )
    | Sell_Index invest ->
        Stock.update_current_prices index (Game.get_start_time ());
        let s = List.hd invest in
        (*let g = legal_stock_history new_stock_history s in*)
        let n = int_of_string (List.nth invest 1) in
        let user_portfolio = User.getportfolio u in
        let st = legal index s in
        if
          Index_history.get_shares
            (User.legal_index_history
               (Portfolio.get_index_history user_portfolio)
               s)
          < n
        then print_string "You do not have enough shares \n"
        else (
          User.sell_index s n u st;
          print_string "You just sold shares \n" )
    | Checkstock invest ->
        Stock.update_current_prices stocks (Game.get_start_time ());
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

let parse str u =
  let lst = String.split_on_char ' ' str in
  match lst with
  | [] -> raise EmptyCommand
  | [ "" ] -> raise EmptyCommand
  | h :: invest ->
      if h = "" then raise EmptyCommand
      else if h = "cash" then view Cash u
      else if h = "view_index" then view ViewIndex u
      else if h = "networth" then view Networth u
      else if h = "help" then view Help u
      else if h = "sell_index" && invest <> [ "" ] then
        view (Sell_Index invest) u
      else if h = "buy_index" && invest <> [ "" ] then
        view (Buy_Index invest) u
      else if h = "sell_s" && invest <> [ "" ] then
        view (Sell_S invest) u
      else if h = "buy_s" && invest <> [ "" ] then view (Buy_S invest) u
      else if h = "buy_cd" && invest <> [ "" ] then
        view (BuyCD invest) u
      else if h = "sell_cd" && invest <> [ "" ] then
        view (SellCD invest) u
      else if h = "view_cd" then view ViewCD u
      else if h = "checkstock" && invest <> [ "" ] then
        view (Checkstock invest) u
        (*else if h = "my_stockhistory" then My_stockhistory*)
      else raise BadCommand
