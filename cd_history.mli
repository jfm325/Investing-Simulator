(* Representation of investment history in cds (Certificate of
   Deposits).

   This module represents a user's investment history in cds. *)

(** The abstract type for a portfolio. *)
type t

(** [get_cd_lst cd_hist] is the list of cds in cd history [cd_hist]. *)
val get_cd_lst : t -> Cd.t list

(** [get_cds_owned cd_hist] is the number of cds owned in cd history
    [cd_hist]. *)
val get_cds_owned : t -> int

(** [collect_cd cd_hist i] is the cd *)
val collect_cd : t -> int -> t

(** [buy_cd cd_hist amt l] adds a new cd to the cd history [cd_hist]
    with an amount of [amt], for [l] months, and at the current interest
    rate. *)
val buy_cd : t -> float -> Cd.term -> t

(** [create_cd_history filename] is the cd history for a cd with
    interest rates pulled from file [filename]. *)
val create_cd_history : string -> t
