open Stock
open Interaction
open Init

let prompt_str = "> "

(* [print_stocks s_lst] prints the stocks in [s_lst]. *)
let print_stocks (s_lst : Stock.t list) =
  let bar = "****************" in
  let name = "" in
  let prices = "" in
  let rec print_stocks_helper (lst : Stock.t list) n p =
    match lst with
    | [] ->
        print_endline bar;
        print_endline n;
        print_endline p;
        print_endline bar
    | h :: t ->
        print_stocks_helper t
          (n ^ get_name h ^ "\t")
          (prices ^ string_of_float (get_current_price h) ^ "\t")
  in
  print_stocks_helper s_lst name prices

let rec prompt_input () =
  print_string prompt_str;
  match read_line () with
  | exception End_of_file -> ()
  | line when line = "quit" -> exit 0
  | line when line = "s" ->
      Stock.update_current_prices stocks !start_time;
      print_stocks stocks;
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
      with _ ->
        print_endline "Invalid Command";
        prompt_input () )

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
        Init.update_start_time (Unix.time ());
        prompt_input () )
      else if trimmed = "quit" then exit 0
      else prompt_for_start ()

let main () =
  print_endline Init.intro_string;
  print_endline Init.instructions;
  prompt_for_start ()

let () = main ()
