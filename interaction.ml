open Stock
include Init
open User
open Portfolio
open Cd_history
open Cd
open Game

type invest = string list

type command =
  | Buy_Index of invest
  | Sell_Index of invest
  | Buy_S of invest
  | Sell_S of invest
  | Cash
  | Networth
  | Portfolio_percent
  | Checkstock of invest
  | Help
  | BuyCD of invest
  | SellCD of invest
  | ViewCD
  | ViewIndex
  | BotNetworth

type currency =
  | USD
  | CAD
  | EUR
  | GBP
  | CHF
  | NZD
  | AUD
  | JPY
  | Illegal

exception EmptyCommand

exception BadCommand

let curr_symb curr =
  match curr with
  | USD -> "$"
  | CAD -> "C$"
  | EUR -> "€"
  | GBP -> "£"
  | CHF -> "SFr."
  | NZD -> "NZ$"
  | AUD -> "A$"
  | JPY -> "¥"
  | Illegal -> "Illegal Currency"

let match_type_curr str =
  match str with
  | "USD" -> USD
  | "CAD" -> CAD
  | "EUR" -> EUR
  | "GBP" -> GBP
  | "CHF" -> CHF
  | "NZD" -> NZD
  | "AUD" -> AUD
  | "JPY" -> JPY
  | _ -> Illegal

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

(* [pp_print_cd] is the helper function for [print_cd] to print info
   about owned cds . *)
let pp_print_cd num term_str maturity_str apy_str =
  let penalty_warning =
    "** 10% penalty if collected before maturity **\n"
  in
  print_endline Init.bar;
  ANSITerminal.print_string [ ANSITerminal.yellow ] "YOUR CDs";
  print_endline ("\n" ^ Init.bar);
  print_endline num;
  print_endline term_str;
  print_endline maturity_str;
  print_endline apy_str;
  print_endline bar;
  ANSITerminal.print_string [ ANSITerminal.red ] penalty_warning

(* [print_cd] prints info about cds owned. *)
let print_cd his_lst lst count =
  let num = "Cd:                    " in
  let term = "Term (months):         " in
  let maturity = "Months until Maturity: " in
  let apy = "APY (%):               " in
  let c = 1 in
  let rec print_cd_helper his_lst (lst : Cd.t list) num term mat_str
      apy_str c =
    match lst with
    | [] -> pp_print_cd num term mat_str apy_str
    | h :: t ->
        print_cd_helper his_lst t
          (num ^ string_of_int c ^ "\t")
          (term ^ string_of_int (Cd.get_length h) ^ "\t")
          (mat_str ^ string_of_int (Cd.months_until_maturity h) ^ "\t")
          (apy_str ^ string_of_float (Cd.get_apy_percentage h) ^ "\t")
          (c + 1)
  in
  print_cd_helper his_lst lst num term maturity apy c

(* [times_income_received] is the numer of times the user has received
   their $10k income every 6 months. *)
let times_income_received = ref 0

(* [give_user_income_if_needed u] adds income to user [u]'s cash for
   every 6 months passed. *)
let give_user_income_if_needed u =
  let time_elapsed =
    int_of_float (Unix.time () -. Game.get_start_time ())
  in
  let i = time_elapsed / Game.s_per_month in
  (* [month] is the month index and caps at 240 months for 20yrs. *)
  let month = if i > 240 then 240 else i + 1 in
  let income_times = month / 6 in
  let diff = float_of_int (income_times - !times_income_received) in
  if diff > 0. then (
    User.add_income_cash u (diff *. 10000.);
    times_income_received := income_times )
  else ()

let botnet curr =
  let botnw = string_of_float (Bot.get_net_worth bot) in
  print_string ("The networth of the bot is " ^ curr ^ botnw ^ "\n")

let display_index u =
  let p = User.get_portfolio u in
  let in_h = Portfolio.get_index_history p in
  print_in in_h

let displaycd u =
  let p = User.get_portfolio u in
  let cd_h = Portfolio.get_cd_history p in
  let c_lst = Cd_history.get_cd_lst cd_h in
  print_cd cd_h c_lst 1

let buy_cd u invest =
  let amt = float_of_string (List.nth invest 1) in
  if amt < 1000. then print_string "Amount not sufficient \n"
  else
    let term = int_of_string (List.hd invest) in
    let cdterm = checklegalterm term in
    User.buy_cd u amt cdterm;
    print_string "You just bought a cd and your cash has changed \n"

let sell_cd u invest =
  let n = int_of_string (List.hd invest) in
  let i = n - 1 in
  User.sell_cd u i;
  print_string "You just sold cd and your cash has changed \n"

let user_networth u curr =
  Stock.update_current_prices Init.stocks (Game.get_start_time ());
  Stock.update_current_prices Init.index_funds (Game.get_start_time ());
  let n =
    string_of_float (User.get_net_worth u Init.stocks Init.index_funds)
  in
  print_string ("Your current networth is " ^ curr ^ n ^ "\n")

let buy_shares u invest =
  let stock_n = int_of_string (List.hd invest) - 1 in
  let num_of_stocks = List.length Init.stocks in
  if stock_n < 0 || stock_n >= num_of_stocks then
    print_string "TRANSACTION ERROR: Invalid stock number.\n"
  else
    let shares = int_of_string (List.nth invest 1) in
    let stock = List.nth Init.stocks stock_n in
    let curr_price = Stock.get_current_price stock in
    let cost = float shares *. curr_price in
    if User.get_cash u -. cost < 0.0 then
      print_endline
        "TRANSACTION ERROR: You do not have enough cash to purchase \
         this stock \n"
    else User.buy_stock shares u stock

let sell_shares u invest =
  Stock.update_current_prices Init.stocks (Game.get_start_time ());
  let stock_n = int_of_string (List.hd invest) - 1 in
  let shares = int_of_string (List.nth invest 1) in
  let stock = List.nth Init.stocks stock_n in
  User.sell_stock shares u stock stock_n

let buy_index u invest =
  let index_n = int_of_string (List.hd invest) - 1 in
  let num_of_index_funds = List.length Init.index_funds in
  if index_n < 0 || index_n >= num_of_index_funds then
    print_string "TRANSACTION ERROR: Invalid index fund number.\n"
  else
    let shares = int_of_string (List.nth invest 1) in
    let index_fund = List.nth Init.index_funds index_n in
    let curr_price = Stock.get_current_price index_fund in
    let cost = float shares *. curr_price in
    if User.get_cash u -. cost < 0.0 then
      print_endline
        "TRANSACTION ERROR: You do not have enough cash to purchase \
         this stock \n"
    else (
      User.buy_index shares u index_fund;
      print_string
        "You just bought into an index fund and your cash has changed\n"
      )

let sell_index u invest =
  Stock.update_current_prices Init.index_funds (Game.get_start_time ());
  let index_n = int_of_string (List.hd invest) - 1 in
  let shares = int_of_string (List.nth invest 1) in
  let index_fund = List.nth Init.index_funds index_n in
  User.sell_index shares u index_fund index_n

let checkstock_helper invest =
  Stock.update_current_prices stocks (Game.get_start_time ());
  let s = List.hd invest in
  let st = legal stocks s in
  let b = legal_stock_history stock_history_lst s in
  let c = string_of_float (User.get_stocks_pl st b) in
  if float_of_string c < 0. then
    print_string
      ("Your stocks currently has a loss $" ^ c ^ " dollars \n")
  else
    print_string
      ("Your stocks currently has a gain $" ^ c ^ " dollars \n")

let the_user_portfolio_percent u =
  Stock.update_current_prices Init.stocks (Game.get_start_time ());
  Stock.update_current_prices Init.index_funds (Game.get_start_time ());
  print_endline "Portfolio";
  let percent_cash =
    string_of_float
      (List.nth
         (User.get_portfolio_percent u Init.stocks Init.index_funds)
         0)
  in
  print_string ("Cash: (" ^ percent_cash ^ "%)" ^ "\n");
  let percent_stock =
    string_of_float
      (List.nth
         (User.get_portfolio_percent u Init.stocks Init.index_funds)
         1)
  in
  print_string ("Stock: (" ^ percent_stock ^ "%)" ^ "\n");
  let percent_index =
    string_of_float
      (List.nth
         (User.get_portfolio_percent u Init.stocks Init.index_funds)
         2)
  in
  print_string ("Index: (" ^ percent_index ^ "%)" ^ "\n");
  let percent_cd =
    string_of_float
      (List.nth
         (User.get_portfolio_percent u Init.stocks Init.index_funds)
         3)
  in
  print_string ("CD: (" ^ percent_cd ^ "%)" ^ "\n")

let view com u (curr : string) =
  give_user_income_if_needed u;
  try
    match com with
    | BotNetworth -> botnet curr
    | ViewIndex -> display_index u
    | ViewCD -> displaycd u
    | BuyCD invest -> buy_cd u invest
    | SellCD invest -> sell_cd u invest
    | Help -> print_string Init.instructions
    | Cash ->
        let c = string_of_float (User.get_cash u) in
        print_string ("Your current cash is " ^ curr ^ c ^ "\n")
    | Networth -> user_networth u curr
    | Buy_S invest -> buy_shares u invest
    | Sell_S invest -> sell_shares u invest
    | Buy_Index invest -> buy_index u invest
    | Sell_Index invest -> sell_index u invest
    | Checkstock invest -> checkstock_helper invest
    | Portfolio_percent -> the_user_portfolio_percent u
  with Not_found ->
    print_string "Invalid stock not found in market. \n"

let helper_parse player instr invest curr =
  if instr = "" then raise EmptyCommand
  else if instr = "cash" then view Cash player curr
  else if instr = "bot" then view BotNetworth player curr
  else if instr = "view_index" then view ViewIndex player curr
  else if instr = "networth" then view Networth player curr
  else if instr = "portfolio_percent" then
    view Portfolio_percent player curr
  else if instr = "help" then view Help player curr
  else if instr = "sell_index" && invest <> [ "" ] then
    view (Sell_Index invest) player curr
  else if instr = "buy_index" && invest <> [ "" ] then
    view (Buy_Index invest) player curr
  else if instr = "sell_s" && invest <> [ "" ] then
    view (Sell_S invest) player curr
  else if instr = "buy_s" && invest <> [ "" ] then
    view (Buy_S invest) player curr
  else if instr = "buy_cd" && invest <> [ "" ] then
    view (BuyCD invest) player curr
  else if instr = "sell_cd" && invest <> [ "" ] then
    view (SellCD invest) player curr
  else if instr = "view_cd" then view ViewCD player curr
  else if instr = "checkstock" && invest <> [ "" ] then
    view (Checkstock invest) player curr
  else raise BadCommand

let parse str u symb =
  let lst = String.split_on_char ' ' str in
  match lst with
  | [] -> raise EmptyCommand
  | [ "" ] -> raise EmptyCommand
  | h :: invest -> helper_parse u h invest symb
