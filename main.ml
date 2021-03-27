let intro_string =
  "\n\
   *************************\n\
   Stock Simulation Game\n\
   *************************\n"

let instructions =
  "You will be able to buy/sell individual stock shares\n\
   with the goal of making the most of your salary.\n\n\
   Commands:\n\
   See networth:       networth\n\
   See on-hand cash:   cash\n\
   Buy shares:         buy [ticker_symbol] [# of shares]\n\
   Sell shares:        sell [ticker_symbol] [# of shares]\n\n"

let prompt_str = "> "

let rec prompt_input () =
  (* Implement parsing *)
  print_string "started game\n"

let rec prompt_for_start () =
  print_string
    ("Type [start] to begin, or [quit] to exit game.\n" ^ prompt_str);
  match read_line () with
  | exception End_of_file -> ()
  | line ->
      let trimmed = String.trim line in
      if trimmed = "start" then prompt_input ()
      else if trimmed = "quit" then exit 0
      else prompt_for_start ()

let main () =
  print_endline intro_string;
  print_endline instructions;
  prompt_for_start ()

let () = main ()
