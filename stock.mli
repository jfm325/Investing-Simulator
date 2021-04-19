(* Representation of a stock.

   This module represents a stock in the simulation and contains the
   name of the company, the ticker symbol for distinguishability, and
   the historical prices t *)

(** The abstract type for a stock. *)
type t

(** The type for a stock names in-game. *)
type stock_name = string

(** The type for a stock's ticker symbol. *)
type ticker_symbol = string

(** [s_per_month] is the number of seconds before updating stock prices. *)
val s_per_month : int

(** [get_name s] is the name of stock [s]. *)
val get_name : t -> stock_name

(** [get_ticker s] is the ticker symbol of stock [s]. *)
val get_ticker : t -> ticker_symbol

(* [get_price s i] is the price of the stock [s] at index [i] of the
   array of prices. Requires: [i] is a valid index in the range 0 -
   length of array. *)
val get_price : t -> int -> float

(* [get_current_price s] is the current price of the stock [s]. *)
val get_current_price : t -> float

(* [update_current_prices lst start_time] updates the current prices of
   stocks in [lst] based on the current time.*)
val update_current_prices : t list -> float -> unit

(** [create_stock n t f] is the stock with name [n], ticker symbol [t],
    and a prices array built from prices are listed on file [f].
    Requires: [f] is a valid filename. *)
val create_stock : stock_name -> ticker_symbol -> string -> t
