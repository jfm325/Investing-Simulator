(* This module represents the data stored in stock_and_users files *)
(*The type of record stockHistory*)
(* type sh *)

(*The type of record user*)
type t

(* [create_user c sh_lst] is the type of user.t . It has the starting
   amount of [c] and the user net worth for the game. User starts with
   stock history of [sh_lst]. *)
val create_user : float -> Stock_history.t list -> t

(*[get_net_worth u stocks_lst] return the net worth of the user. *)
val get_net_worth : t -> Stock.t list -> float

(*[get_cash u] return the cash of the user*)
val get_cash : t -> float

val checkstock : Stock.t -> Stock_history.t -> float

val find : 'a -> 'a list -> int

val change_cash_buy : t -> int -> Stock.t -> unit

val buy : string -> int -> t -> Stock.t -> unit

val legal_stock_history :
  Stock_history.t list -> string -> Stock_history.t
