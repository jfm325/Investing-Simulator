(* Representation of investment history in cds (Certificate of
   Deposits).

   This module represents a user's investment history in cds. *)

(** The abstract type for a portfolio. *)
type t

(** [buy_cd cd_hist amt l] adds a new cd to the cd history [cd_hist]
    with an amount of [amt], for [l] months, and at the current interest
    rate. *)
val buy_cd : t -> float -> int -> t

(** [create_cd_history filename] is the cd history for a cd with
    interest rates pulled from file [filename]. *)
val create_cd_history : string -> t
