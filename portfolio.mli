(* Representation of an investment portfolio.

   This module represents a portfolio in the simulation and contains a
   user's investments. *)

(** The abstract type for a portfolio. *)
type t

(** [get_cd_history p] is the cd history in portfolio [p]. *)
val get_cd_history : t -> Cd_history.t

(** [get_stock_history p] is the stock history list in portfolio [p]. *)
val get_stock_history : t -> Stock_history.t list

(** [create_portfolio sh cd_h] is the portfolio with a list of stock
    histories given by [sh] and cd_history [cd_h]. *)
val create_portfolio : Stock_history.t list -> Cd_history.t -> t

(** [buy_stock p stock n] updates portfolio [p] with a buy order of [n]
    shares of stock [stock]]. *)
val buy_stock : t -> Stock.t -> int -> t
