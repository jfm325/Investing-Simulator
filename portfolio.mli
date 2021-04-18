(* Representation of an investment portfolio.

   This module represents a portfolio in the simulation and contains a
   user's investments. *)

(** The abstract type for a portfolio. *)
type t 

(** [create_portfolio sh] is the portfolio with a list of sotck histories given 
by [sh]. *)
val create_portfolio : Stock_history.t list -> t

(** [buy_stock p stock n] updates portfolio [p] with a buy order of [n] shares 
of stock [stock]]. *)
val buy_stock : t -> Stock.t -> int -> t