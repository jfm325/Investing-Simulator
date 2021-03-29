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

(**Sell invest -> let s = List.hd invest in let n = int_of_string
   (List.nth invest 1) in let st = legal stocks s in User.sell s n u st
   (*sell not made yet*)**)

(**let v3 c = let u = User.default_user 2000.0 in match c with | Cash ->
   print_float (get_cash u) | Networth -> print_float (get_net_worth u)
   | Sell inv -> print_string "not yet implemented" | Buy inv ->
   print_string "not yet implemented" *)

let view c =
  try
    let u = User.default_user 2000.0 in
    match c with
    | Cash -> print_float (User.get_cash u)
    | Networth -> print_float (User.get_net_worth u)
    (*s is stock symbol, n is number of shares. u is user and st is
      Stock.t *)
    | Buy invest ->
        let s = List.hd invest in
        let n = int_of_string (List.nth invest 1) in
        let st = legal stocks s in
        User.buy s n u st
    | Sell invest -> print_string "not yet implemented"
  with Not_found -> print_string "Share doesn't exit"
