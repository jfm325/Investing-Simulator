type stockHistory = {
  stock: string; mutable shares: int; mutable buy_in_prices: float list 
}

type user = {
  mutable net_worth: float; mutable cash: float; 
  mutable stock_companies: stockHistory list ; mutable string_stock_companies: 
  string list
} 
let default_user = { net_worth = 2000.0 ;cash = 2000.0; stock_companies = []; 
string_stock_companies = []}

(* method to buy stock first check to see if stock is in the stockhistory list 
  and if it is then it share gets updated else
  check if stock is in the official stock board and then creates a new 
    stockhistory record and appends it to the stock history list*)

    (*let buy (stock: string) =
  let () = if List.mem stock default_user.string_stock_companies then
    (List.nth default_user.stock_companies 0).shares <- 
    (List.nth default_user.stock_companies 0).shares + 1 else if *)



(*Test Cases (this will be placed onto the test.ml file later on)*)
let tesla = {stock = "TSLA"; shares = 2 ; buy_in_prices = [5.25;5.15]}
let () = default_user.stock_companies <- [tesla]
let () = print_int (List.nth default_user.stock_companies 0).shares