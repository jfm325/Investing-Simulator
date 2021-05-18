(* Representation of investment history in stocks.

   This module represents a user's investment history in stocks.
   Includes buy-in prices and number of shares owned. *)

(** The abstract type for a portfolio. *)
type r

(** [create_re_history t] is the real estate history for a stock with
    ticker symbol [t]. *)
val create_re_history : string -> r

(** [buy sh price n] updates real estate history [sh] with the new buy order
    of [n] shares at price [price]. *)
val buy : r -> float -> int -> r

(** [get_ticker sh] is the ticker symbol for real estate history [sh]. *)
val get_ticker : r -> string

(** [get_shares sh] is the number of shares owned for real estate history
    [sh]. *)
val get_shares : r -> int
val sell : r -> int -> unit
val update_sell : r -> int -> unit
(** [get_buy_in_prices sh] is the buy_in_prices for real estate history [sh]. *)
val get_buy_in_prices : r -> (float * int) list


