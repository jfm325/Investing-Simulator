(* This module represents the data stored in stock_and_users files *)
(*The type of record stockHistory*)
type sh

(*The type of record user*)
type u

(*the type of the variable default user. It has the starting amount of
  cash and the user net worth for the game. User starts with no stock
  history*)
val default_user : float -> u

val get_net_worth : u -> float

val get_cash : u -> float

val get_stock_companies : u -> sh list

val get_stock : sh -> string

val get_shares : sh -> int

val find : 'a -> 'a list -> int

val create_stock_history : string -> int -> float -> sh

val change_cash_buy : u -> int -> Stock.t -> unit

val buy : string -> int -> u -> Stock.t -> unit
