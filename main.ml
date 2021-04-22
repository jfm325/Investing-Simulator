open Game
open Stock
open Interaction
open Init
open Stock_history
open User

let prompt_str = "> "

(* [print_stocks s_lst] prints the stocks in [s_lst]. *)
let print_stocks (s_lst : Stock.t list) (history : Stock_history.t list)
    =
  let bar = "*******************************************" in
  let shares = "Shares: " in
  let name = "Stock:  " in
  let prices = "Price:  " in
  let user_stock_performance = "U P/L:  " in
  let rec print_stocks_helper (his_lst : Stock_history.t list)
      (lst : Stock.t list) n p g z =
    match lst with
    | [] ->
        print_endline bar;
        print_endline n;
        print_endline p;
        print_endline g;
        print_endline z;
        print_endline bar
    | h :: t ->
        print_stocks_helper his_lst t
          (n ^ get_name h ^ "\t")
          (p ^ string_of_float (get_current_price h) ^ "\t")
          ( g
          ^ string_of_int
              (get_shares
                 (legal_stock_history his_lst (Stock.get_ticker h)))
          ^ "\t" )
          ( z
          ^ string_of_float
              (checkstock h
                 (legal_stock_history his_lst (Stock.get_ticker h)))
          ^ "\t" )
  in
  print_stocks_helper history s_lst name prices shares
    user_stock_performance

(** [has_game_ended s] returns true when in-game time has reached or
    passed year 20 (nmonth 240). *)
let has_game_ended s =
  let current_time = int_of_float (Unix.time () -. !start_time) in
  let month = current_time / s in
  month >= 240

let end_game_function () =
  print_endline "TODO: End of game functionality"

(** [prompt_input] prompts user for input during the simulation. *)
let rec prompt_input () =
  if has_game_ended Stock.s_per_month then end_game_function ()
  else (
    print_string prompt_str;
    match read_line () with
    | exception End_of_file -> ()
    | line when line = "quit" -> exit 0
    | line when line = "s" ->
        Stock.update_current_prices stocks !start_time;
        print_stocks stocks stock_history_lst;
        prompt_input ()
    | line -> (
        try
          let cmd = Interaction.parse line in
          match cmd with
          | Cash ->
              Interaction.view Cash user;
              prompt_input ()
          | Networth ->
              Stock.update_current_prices stocks !start_time;
              Interaction.view Networth user;
              prompt_input ()
          | Buy lst ->
              Stock.update_current_prices stocks !start_time;
              Interaction.view (Buy lst) user;
              prompt_input ()
          | Sell lst ->
              Stock.update_current_prices stocks !start_time;
              print_endline "Selling";
              prompt_input ()
          | Checkstock lst ->
              Stock.update_current_prices stocks !start_time;
              Interaction.view (Checkstock lst) user;
              prompt_input ()
        with _ ->
          print_endline "Invalid Command";
          prompt_input () ) )

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
        prompt_input () )
      else if trimmed = "quit" then exit 0
      else prompt_for_start ()

let main () =
  print_endline Init.intro_string;
  print_endline Init.instructions;
  prompt_for_start ()

let () = main ()
