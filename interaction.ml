open Stock
include Init
open User

type invest = string list

type command =
  | Buy of invest
  | Sell of invest
  | Cash
  | Networth
  | Quit
  | Info
  | Share

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
      else if h = "info" then Info
      else if h = "share" then Share
      else if h = "networth" then Networth
      else if h = "sell" && invest <> [ "" ] then Sell invest
      else if h = "buy" && invest <> [ "" ] then Buy invest
      else if h = "quit" then Quit
      else raise BadCommand

let rec find_Stock list symb =
  match list with
  | [] -> raise Not_found
  | h :: t -> if Stock.get_ticker h = symb then h else find_Stock t symb

(*let legal_sell s n u = let stocks = User.get_stock_companies u in
  match stocks with | [] -> raise Not_found | h :: t -> if
  User.get_stock h = s && User.get_shares h < n then true else false*)

(*let rec print stocklist = match stocklist with | [] -> print_string
  "--- \n" | h :: t -> let s = User.get_stock h in let n = string_of_int
  (User.get_shares h) in print_string (s ^ " " ^ n); print_string "\n ";
  print t*)

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

let view com u =
  try
    match com with
    | Share -> failwith ""
    | Info -> failwith ""
    | Quit -> failwith ""
    | Cash ->
        let c = string_of_float (User.get_cash u) in
        print_string ("Your current cash is " ^ c ^ "\n")
    | Networth ->
        let n = string_of_float (User.get_net_worth u) in
        print_string ("Your current networth is " ^ n ^ "\n")
    | Buy invest ->
        let s = List.hd invest in
        let n = int_of_string (List.nth invest 1) in
        let st = find_Stock stocks s in
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
    | Sell invest -> print_string "not yet implemented"
  with Not_found ->
    print_string "Invalid stock not found in market. \n"

(*let view com u = try match com with | Share ->
  Stock.update_current_prices stocks !start_time; print_stocks stocks |
  Info -> print_string "Your current portfolio information will be
  displayed \n\ \ \n"; let s = User.get_stock_companies u in print s |
  Quit -> Stdlib.exit 0 | Cash -> let c = string_of_float (User.get_cash
  u) in print_string ("Your current cash is " ^ c ^ "\n") | Networth ->
  let n = string_of_float (User.get_net_worth u) in print_string ("Your
  current networth is " ^ n ^ "\n") | Buy invest -> let s = List.hd
  invest in let n = int_of_string (List.nth invest 1) in let st =
  find_Stock stocks s in if User.get_cash u -. (float n *.
  Stock.get_current_price st) <= 0.0 then print_string "You do not have
  enough cash to purchase this stock. \n" else ( User.buy s n u st;
  print_string "Your purchase was succesful. \n" ) | Sell invest -> let
  s = List.hd invest in let n = int_of_string (List.nth invest 1) in let
  st = find_Stock stocks s in if legal_sell s n u then print_string "You
  cannot make this sale. \n" else ( (*User.sell s n u st;*) print_string
  "Your sale was succesful. \n " ) with Not_found -> print_string
  "Invalid stock not found in market. \n"*)
