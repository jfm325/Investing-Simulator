(** Representation of an investment portfolio.

    This module represents a portfolio in the simulation and contains a
    user's investments. *)

(** The abstract type for a portfolio. *)
type t

(** [create_portfolio sh] is the portfolio with a list of stock
    histories given by [sh]. *)
val create_portfolio :
  Stock_history.t list -> Index_history.i list -> Cd_history.t -> t

(** [get_cd_history p] is the cd history in portfolio [p]. *)
val get_cd_history : t -> Cd_history.t

(** [get_index_history p] is the size of index history in portfolio [p]. *)
val get_index_history_size : t -> int

(** [buy_stock p stock n] updates portfolio [p] with a buy order of [n]
    shares of stock [stock]. *)
val buy_stock : t -> Stock.t -> int -> t

(** [buy_index p stock n] updates portfolio [p] with a buy order of [n]
    shares of index [stock]. *)
val buy_index : t -> Stock.t -> int -> t

(** [get_stock_history p] is the stock history list in portfolio [p]. *)
val get_stock_history : t -> Stock_history.t list

(** [get_index_history p] is the index history list isn portfolio [p]. *)
val get_index_history : t -> Index_history.i list

(** [get_stock_history p] is the size of stock history in portfolio [p]. *)
val get_stock_history_size : t -> int
