(* Representation of a User.

   This module contains the data stored for a user's financial info.
   Contains current cash, networth, and their portfolio. *)

(* The type of record user. *)
type t

(* [create_user c sh_lst] is the type of user.t . It has the starting
   amount of [c] and the user net worth for the game. User starts with
   stock history of [sh_lst]. *)
val create_user :
  float ->
  Stock_history.t list ->
  Index_history.i list ->
  Cd_history.t ->
  t

(* [get_cash u] returns the cash of user [u]. *)
val get_cash : t -> float

(* [get_portfolio u] returns the portfolio of user [u]. *)
val get_portfolio : t -> Portfolio.t

(** [remove_cash u amt] removes [amt] from user [u]'s cash. *)
val remove_cash : t -> float -> unit

(** [add_cash u amt] adds [amt] to user [u]'s cash. *)
val add_cash : t -> float -> unit

(** [add_income_cash u amt] adds income of [amt] to user [u]'s cash. *)
val add_income_cash : t -> float -> unit

(* [get_net_worth u stocks_lst indexfunds_lst] return the net worth of
   user [u]. *)
val get_net_worth : t -> Stock.t list -> Stock.t list -> float

(** [change_cash_buy u shares stock] changes the user [u]'s cash when
    buying shares of an index fund or stock. *)
val change_cash_buy : t -> int -> Stock.t -> unit

(** [change_cash_sell u shares stock] changes the user [u]'s cash when
    selling shares of an index fund or stock. *)
val change_cash_sell : t -> int -> Stock.t -> unit

(** [buy_stock shares u stock] buys [shares] shares of stocks [stock]
    for user [u]. *)
val buy_stock : int -> t -> Stock.t -> unit

(** [sell_stock index shares u stock] sells [shares] shares from the
    stock history at index [index] in the stock history list for user
    [u]. Requires: [index] is a valid index in the stock history list
    for [user]. *)
val sell_stock : int -> t -> Stock.t -> int -> unit

(** [buy_index shares user index_fund] buys [shares] shares of index
    fund [index_fund] for user [u]. *)
val buy_index : int -> t -> Stock.t -> unit

(** [sell_index shares user index_fund index] sells [shares] shares from
    the index fund history at index [index] in the index fund history
    list for user [u]. Requires: [index] is a valid index in the stock
    history list for [user]. *)
val sell_index : int -> t -> Stock.t -> int -> unit

(** [buy_cd u amt t] buys a cd for user [u] for an amount [amt] and for
    a maturity length of [t]. Updates the user's cash. *)
val buy_cd : t -> float -> Cd.term -> unit

(** [sell_cd u i] sells the cd for user [u] at index [i]. Requires: [i]
    is a valid index in the list of currently owned cds. *)
val sell_cd : t -> int -> unit

(** [get_stocks_pl stock sh] is the profit/loss of stock [stock]
    compared to the buy-in value from shares owned in stock history
    [sh]. *)
val get_stocks_pl : Stock.t -> Stock_history.t -> float

(** [get_index_pl stock ih] is the profit/loss of stock [stock] compared
    to the buy-in value from shares owned in index history [ih] *)
val get_index_pl : Stock.t -> Index_history.i -> float

(* [get_shares_sh_lst sh_lst acc] is the total number of shares in the
   list of stock histories [sh_lst]. *)
val get_shares_sh_lst : Stock_history.t list -> int -> int

(* [get_shares_ih_lst ih_lst acc] is the total number of shares in the
   list of index fund histories [ih_lst]. *)
val get_shares_ih_lst : Index_history.i list -> int -> int
