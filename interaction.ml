open User

type invest = string list

type command = Buy of invest | Sell of invest | Cash | Networth

let user = User.current_player

exception EmptyCommand

exception BadCommand

let parse str =
  let lst = String.split_on_char ' ' str in
  match lst with
  | [] -> raise EmptyCommand
  | [ "" ] -> raise EmptyCommand
  | h :: object_phrase ->
      if h = "" then raise EmptyCommand
      else if h = "cash" then Cash
      else if h = "networth" then Networth
      else if h = "sell" && object_phrase <> [ "" ] then
        Sell object_phrase
      else if h = "buy" && object_phrase <> [ "" ] then
        Buy object_phrase
      else raise BadCommand

let view command =
  match command with
  | Cash -> print_string
  | Networth -> print_string
  | Buy inv -> print_string
  | Sell inv -> print_string
