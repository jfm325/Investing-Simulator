open Game
open Stock
open Interaction
open Init
open Stock_history
open User

let prompt_str = "> "

let currency_symbol = ref ""

(* [print_cd apy] pretty-prints info of a cd based on APY [apy]. *)
let print_cd apy =
  let six_month_apy = Cd.match_new_rate Cd.SixMonths apy in
  let three_yr_apy = Cd.match_new_rate Cd.ThreeYears apy in
  let apy_str_6mnth = string_of_float (100. *. six_month_apy) in
  let apy_str_1yr = string_of_float (100. *. apy) in
  let apy_str_3yrs = string_of_float (100. *. three_yr_apy) in
  let apy_str =
    apy_str_6mnth ^ "%\t\t" ^ apy_str_1yr ^ "%\t\t" ^ apy_str_3yrs ^ "%"
  in
  let terms = "CD Term: 6 months(1)\t1 year(2)\t3 years(3)" in
  print_endline Init.bar;
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "Certificates of Deposit\n";
  print_endline Init.bar;
  print_endline terms;
  print_endline ("APY:     " ^ apy_str);
  print_endline Init.bar

(** [print_profit_loss_helper lst] prints every float in [lst] with a
    color red for negative floats, green for positive floats, and white
    for zero. *)
let rec print_profit_loss_helper lst =
  match lst with
  | [] -> ()
  | h :: t ->
      let pl = string_of_float h in
      let color =
        match h with
        | h' when h' < 0. -> ANSITerminal.red
        | h' when h' > 0. -> ANSITerminal.green
        | _ -> ANSITerminal.default
      in
      ANSITerminal.print_string [ color ] (pl ^ "\t\t");
      print_profit_loss_helper t

(* [pp_print_stocks ticker_str price_str shares_str pl_lst]
   pretty-prints all stock information. Note: [pl_lst] is a reversed
   list of the profit/losses.*)
let pp_print_stocks ticker_str price_str shares_str pl_lst per_lst =
  let pl_rev_lst = List.rev pl_lst in
  let per_rev_lst = List.rev per_lst in
  print_endline Init.bar;
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "INDIVIDUAL STOCKS\n";
  print_endline Init.bar;
  print_endline ticker_str;
  print_endline price_str;
  print_string Init.percent_str;
  print_profit_loss_helper per_rev_lst;
  print_endline ("\n" ^ shares_str);
  print_string Init.profit_loss_str;
  print_profit_loss_helper pl_rev_lst;
  print_endline ("\n" ^ Init.bar)

let pp_print_index ticker_str price_str shares_str pl_lst per_lst =
  let pl_rev_lst = List.rev pl_lst in
  let per_rev_lst = List.rev per_lst in
  print_endline Init.bar;
  ANSITerminal.print_string [ ANSITerminal.yellow ] "INDEX FUNDS\n";
  print_endline Init.bar;
  print_endline ticker_str;
  print_endline price_str;
  print_string Init.percent_str;
  print_profit_loss_helper per_rev_lst;
  print_endline ("\n" ^ shares_str);
  print_string Init.profit_loss_str;
  print_profit_loss_helper pl_rev_lst;
  print_endline ("\n" ^ Init.bar)

(* [print_stocks_helper stock_lst ticker_str price_str shares_str
   pl_lst] is the helper function to go through [stock_lst] and fill in
   the strings for stock info. *)
let rec print_stocks_helper stock_lst ticker_str price_str shares_str
    pl_lst per_lst index =
  match stock_lst with
  | [] -> pp_print_stocks ticker_str price_str shares_str pl_lst per_lst
  | h :: t ->
      let sh = List.nth Init.stock_history_lst index in
      let ticker = Stock.get_name h in
      let num_shares = get_shares sh in
      let pl = User.get_stocks_pl h sh in
      let per = Stock.get_percent_change h in
      let index_fund_num = "(" ^ string_of_int (index + 1) ^ ")" in
      print_stocks_helper t
        (ticker_str ^ ticker ^ index_fund_num ^ "\t")
        (price_str ^ string_of_float (get_current_price h) ^ "\t\t")
        (shares_str ^ string_of_int num_shares ^ "\t\t")
        (pl :: pl_lst) (per :: per_lst) (index + 1)

(* [print_stocks s_lst] prints the stocks and stock info in [s_lst]. *)
let print_stocks (s_lst : Stock.t list) =
  print_stocks_helper s_lst Init.ticker_str Init.prices_str
    Init.shares_str [] [] 0

(* [print_index_helper index_lst ticker_str price_str shares_str pl_lst]
   is the helper function to go through [index_lst] and fill in the
   strings for index fund info. *)
let rec print_index_helper index_lst ticker_str price_str shares_str
    pl_lst per_lst index =
  match index_lst with
  | [] -> pp_print_index ticker_str price_str shares_str pl_lst per_lst
  | h :: t ->
      let sh = List.nth Init.index_history_lst index in
      let ticker = Stock.get_name h in
      let num_shares = Index_history.get_shares sh in
      let pl = User.get_index_pl h sh in
      let per = Stock.get_percent_change h in
      let index_fund_num = "(" ^ string_of_int (index + 1) ^ ")" in
      print_index_helper t
        (ticker_str ^ ticker ^ index_fund_num ^ "\t")
        (price_str ^ string_of_float (get_current_price h) ^ "\t\t")
        (shares_str ^ string_of_int num_shares ^ "\t\t")
        (pl :: pl_lst) (per :: per_lst) (index + 1)

(* [print_index index_lst] prints the index fund info and performance in
   [index_lst]. *)
let print_index index_lst =
  print_index_helper index_lst "Index Fund:\t" Init.prices_str
    Init.shares_str [] [] 0

(** [has_game_ended s] returns true when in-game time has reached or
    passed year 20 (nmonth 240). *)
let has_game_ended s =
  let current_time = int_of_float (Unix.time () -. !start_time) in
  let month = current_time / s in
  month >= 240

let view_percent () = Interaction.the_user_portfolio_percent user

let end_game_function () =
  let b = Bot.get_net_worth bot in
  Stock.update_current_prices Init.stocks (Game.get_start_time ());
  Stock.update_current_prices Init.index_funds (Game.get_start_time ());
  let n = User.get_net_worth user Init.stocks Init.index_funds in
  print_string ("Bot : " ^ string_of_float b ^ "\n");
  print_string ("User : " ^ string_of_float n ^ "\n");
  if b > n then
    ANSITerminal.print_string [ ANSITerminal.red ] "Bot wins \n"
  else ANSITerminal.print_string [ ANSITerminal.green ] "User wins \n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "User Portfolio Score \n";
  view_percent ()

(** [parse_input_helper] reads the user input and calls corresponding
    commands. *)
let parse_input_helper () =
  match read_line () with
  | exception End_of_file -> ()
  | line when line = "quit" -> exit 0
  | line when line = "cd" ->
      let p = get_portfolio user in
      let cd_h = Portfolio.get_cd_history p in
      print_cd (Cd_history.get_current_apy cd_h)
  | line when line = "s" ->
      Stock.update_current_prices stocks !start_time;
      print_stocks stocks
  | line when line = "i" ->
      Stock.update_current_prices Init.index_funds !start_time;
      print_index Init.index_funds
  | line -> (
      let symbcurr = !currency_symbol in
      try Interaction.parse line user symbcurr
      with _ -> print_endline "Invalid Command" )

(** [prompt_input] prompts user for input during the simulation. *)
let rec prompt_input () =
  if has_game_ended Game.s_per_month then end_game_function ()
  else (
    (* Bot.purchase_indexfunds bot; *)
    print_string prompt_str;
    parse_input_helper ();
    let time_elapsed =
      int_of_float (Unix.time () -. Game.get_start_time ())
    in
    let time_str = Game.str_of_year_month time_elapsed in
    print_endline time_str;
    print_endline Init.bar;
    prompt_input () )

let rec prompt_curr () =
  print_string "Enter currency \n";
  print_string
    "\n USD \n CAD \n EUR \n GBP \n CHF \n NZD \n AUD \n JPY \n";
  match read_line () with
  | exception End_of_file -> ()
  | line ->
      let trimmed = String.trim line in
      let curr_s = Interaction.match_type_curr trimmed in
      if curr_s = Illegal then (
        print_endline "Illegal Currency, Choose again";
        prompt_curr () )
      else currency_symbol := Interaction.curr_symb curr_s

(** [prompt_for_start] trims the user input and starts the game if the
    user types "start", quits the game if user types "quit". If neither,
    the user is prompted to again to choose. *)
let rec prompt_for_start () =
  print_string
    ("Type [start] to begin, or [quit] to exit game.\n" ^ prompt_str);
  match read_line () with
  | exception End_of_file -> ()
  | line ->
      let trimmed = String.trim line in
      if trimmed = "start" then (
        Game.update_start_time (Unix.time ());
        prompt_curr ();
        prompt_input () )
      else if trimmed = "quit" then exit 0
      else prompt_for_start ()

let main () =
  ANSITerminal.print_string [ ANSITerminal.green ] Init.intro_string;
  ANSITerminal.print_string [ ANSITerminal.yellow ] Init.instructions;
  prompt_for_start ()

let () = main ()
