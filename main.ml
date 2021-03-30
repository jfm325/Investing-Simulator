open Stock
include Init

(* include Countdown *)

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

let prompt_input () =
  (* Implement parsing *)
  (* Countdown.(()) *)
  print_stocks Init.stocks

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
      if trimmed = "start" then Countdown.(())
      else if trimmed = "quit" then exit 0
      else prompt_for_start ()

let main () =
  print_endline Init.intro_string;
  print_endline Init.instructions;
  prompt_for_start ()

let () = main ()
