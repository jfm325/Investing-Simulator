(* Representation of investment history in stocks.

   This module represents a user's investment history in stocks. Includes 
   buy-in prices and number of shares owned. *)

(** The abstract type for a portfolio. *)
type t 

(** [create_stock_history t] is the stock history for a stock with ticker 
symbol [t]. *)
val create_stock_history : string -> t

(** [buy sh price n] updates stock history [sh] with the new buy order of [n] 
shares at price [price]. *)
val buy : t -> float -> int -> t

(** [get_ticker sh] is the ticker symbol for stock history [sh]. *)
val get_ticker: t -> string

(** [get_shares sh] is the number of shares owned for stock history [sh]. *)
val get_shares: t -> int

(** [get_buy_in_prices sh] is the buy_in_prices for stock history [sh]. *)
val get_buy_in_prices : t -> (float * int) list