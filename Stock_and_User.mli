(* This module represents the data stored in stock_and_users files *)
(*The type of record stockHistory*)
type stockHistory = {
  stock : string;
  mutable shares : int;
  mutable buy_in_prices : float list;
}

(*The type of record user*)
type user = {
  mutable net_worth : float;
  mutable cash : float;
  mutable stock_companies : stockHistory list;
  mutable tring_stock_companies : string list;
}

(*the type of the variable default user. It has the starting amount of
  cash and the user net worth for the game. User starts with no stock
  history*)
val default_user : float -> user

val get_net_worth : user -> float

val get_cash : user -> float

val get_stock_companies : user -> stockHistory list

val get_stock : stockHistory -> string

val get_shares : stockHistory -> int

val find : 'a -> 'a list -> int

val create_stock_history : string -> int -> float -> stockHistory

val change_cash_buy : user -> int -> user -> Stock.t -> unit

val buy : string -> int -> user -> Stock.t -> unit
