type stockHistory = {
  stock : string;
  mutable shares : int;
  mutable buy_in_prices : float list;
}

type user = {
  mutable net_worth : float;
  mutable cash : float;
  mutable stock_companies : stockHistory list;
  mutable string_stock_companies : string list;
}

let get_net_worth u = u.net_worth

let get_cash u = u.cash

let get_stock_companies u = u.stock_companies

let get_stock u = u.stock

let get_shares u = u.shares

let default_user (set_amount : float) =
  {
    net_worth = set_amount;
    cash = set_amount;
    stock_companies = [];
    string_stock_companies = [];
  }

(* method to buy stock first check to see if stock is in the
   stockhistory list and if it is then it share gets updated else check
   if stock is in the official stock board and then creates a new
   stockhistory record and appends it to the stock history list*)
let rec find x lst =
  match lst with
  | [] -> raise (Failure "Not Found")
  | h :: t -> if x = h then 0 else 1 + find x t

let create_stock_history name_stock name_shares =
  { stock = name_stock; shares = name_shares; buy_in_prices = [] }

let buy (stock : string) (shares : int) (firstuser : user) =
  if List.mem stock firstuser.string_stock_companies then
    (List.nth firstuser.stock_companies
       (find stock firstuser.string_stock_companies)).shares <-
      (List.nth firstuser.stock_companies
         (find stock firstuser.string_stock_companies))
        .shares + shares
  else (
    firstuser.stock_companies <-
      firstuser.stock_companies @ [ create_stock_history stock shares ];
    firstuser.string_stock_companies <-
      firstuser.string_stock_companies @ [ stock ]
    (*default_user.cash <- update_cash (- shares current_price)*) )

(*Test Cases (this will be placed onto the test.ml file later on)*)
let firstuser = default_user 2000.0

let () =
  firstuser.stock_companies <-
    [ { stock = "TSLA"; shares = 2; buy_in_prices = [ 5.25; 5.15 ] } ]

let () = firstuser.string_stock_companies <- [ "TSLA" ]

let () = buy "TSLA" 2 firstuser

let () = buy "TSLA" 62 firstuser

let () = buy "UMC" 1 firstuser

let () = buy "UMC" 1 firstuser

let () = buy "FUNKO" 3 firstuser

let () = print_int (List.nth firstuser.stock_companies 0).shares
