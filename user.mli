(* This module represents the data stored in stock_and_users files *)
(*The type of record stockHistory*)
(* type sh *)

(*The type of record user*)
type t

(*the type of the variable default user. It has the starting amount of
  cash and the user net worth for the game. User starts with no stock
  history*)
val default_user : float -> t

(*[get_net_worth u] return the net worth of the user*)
val get_net_worth : t -> float

(*[get_cash u] return the cash of the user*)
val get_cash : t -> float

(*[get_stock_companies u] return the list of stock companies the user
  owns*)
(* val get_stock_companies : u -> sh list *)

(*[get_stock sh] return the stocks*)
(* val get_stock : sh -> string *)

(*[get_share sh] return the amount of shares owned from a stock*)
(* val get_shares : sh -> int *)

val find : 'a -> 'a list -> int

(*[create_stock_history name_stock name_shares name_buy_in_price]
  creates a record with the stock name [name_stock] and with shares
  [name_shares] and a list of when they bought their shares
  [name_buy_in_price]*)
(* val create_stock_history : string -> int -> float -> sh *)

val change_cash_buy : t -> int -> Stock.t -> unit

val buy : string -> int -> t -> Stock.t -> unit
