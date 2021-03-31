open Stock
include Init
open User

type invest = string list

type command = Buy of invest | Sell of invest | Cash | Networth

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
      else if h = "sell" && invest <> [ "" ] then Sell invest
      else if h = "buy" && invest <> [ "" ] then Buy invest
      else raise BadCommand

let rec legal list symb =
  match list with
  | [] -> raise Not_found
  | h :: t -> if Stock.get_ticker h = symb then h else legal t symb

let view com u =
  try
    match com with
    | Cash ->
        let c = string_of_float (User.get_cash u) in
        print_string ("Your current cash is " ^ c ^ "\n")
    | Networth ->
        let n = string_of_float (User.get_net_worth u) in
        print_string ("Your current networth is " ^ n ^ "\n")
    | Buy invest ->
        let s = List.hd invest in
        let n = int_of_string (List.nth invest 1) in
        let st = legal stocks s in
        User.buy s n u st;
        print_string
          "You just bought stocks and your cash has changed \n"
    | Sell invest -> print_string "not yet implemented"
  with Not_found ->
    print_string "Invalid stock not found in market. \n"
