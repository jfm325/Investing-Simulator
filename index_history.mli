(** Representation of investment history in stocks.

    This module represents a user's investment history in stocks.
    Includes buy-in prices and number of shares owned. *)

(** The abstract type for a portfolio. *)
type i

(** [create_stock_history t] is the stock history for a stock with
    ticker symbol [t]. *)
val create_index_history : string -> i

(** [buy sh price n] updates stock history [sh] with the new buy order
    of [n] shares at price [price]. *)
val buy : i -> float -> int -> i

(** [get_ticker sh] is the ticker symbol for stock history [sh]. *)
val get_ticker : i -> string

(** [get_shares sh] is the number of shares owned for stock history
    [sh]. *)
val get_shares : i -> int

(** [sell ih n] calls the helper method update_sell to sell the stock
    and subtracts n shares from the stock *)
val sell : i -> int -> unit

(** [update_sell sh n] removes shares from the index.buy_in_prices *)
val update_sell : i -> int -> unit

(** [get_buy_in_prices sh] is the buy_in_prices for stock history [sh]. *)
val get_buy_in_prices : i -> (float * int) list
