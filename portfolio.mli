(* Representation of an investment portfolio.

   This module represents a portfolio in the simulation and contains a
   user's investments. *)

(** The abstract type for a portfolio. *)
type t

(** [create_portfolio sh] is the portfolio with a list of stock
    histories given by [sh]. *)
val create_portfolio : Stock_history.t list -> Index_history.i list -> Cd_history.t -> Real_estate_history.r list -> t
(** [get_cd_history p] is the cd history in portfolio [p]. *)
val get_cd_history : t -> Cd_history.t
val get_index_history_size: t -> int
val get_re_history_size: t -> int

(** [buy_stock p stock n] updates portfolio [p] with a buy order of [n]
    shares of stock [stock]. *)
val buy_stock : t -> Stock.t -> int -> t
val buy_index : t -> Stock.t -> int -> t
val buy_re : t -> Stock.t -> int -> t
(** [get_stock_history p] is the stock history list in portfolio [p]. *)
val get_stock_history : t -> Stock_history.t list
val get_index_history : t -> Index_history.i list
val get_re_history : t -> Real_estate_history.r list
val get_stock_history_size: t -> int
val get_re_history_size: t -> int
val get_index_history_size: t -> int