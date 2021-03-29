(*  This module represents the data stored in stock_and_users files  *)
(*The type of record stockHistory*)
type stockHistory = {
  stock: string; mutable shares: int; mutable buy_in_prices: float list 
}
(*The type of record user*)
type user = {   mutable net_worth: float; mutable cash: float; 
mutable stock_companies: stockHistory list ; mutable 
tring_stock_companies: string list
}
(*the type of the variable default user. It has the starting amount of cash and
 the user net worth for the game. User starts with no stock history*)
val default_user : user
