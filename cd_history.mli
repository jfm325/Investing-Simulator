(** Representation of investment history in cds (Certificate of
    Deposits).

    This module represents a user's investment history in cds. *)

(** The abstract type for investment history in cds. *)
type t

(** [get_current_apy cd_h] is the current APY from the interest rates in
    [cd_h]. *)
val get_current_apy : t -> float

(** [get_cd_lst cd_hist] is the list of cds in cd history [cd_hist]. *)
val get_cd_lst : t -> Cd.t list

(** [get_cds_owned cd_hist] is the number of cds owned in cd history
    [cd_hist]. *)
val get_cds_owned : t -> int

(** [update_cd_lst_values cd_hist] updates the list of cds with current
    value of investment. *)
val update_cd_lst_values : t -> unit

(** [collect_cd_value cd_hist i] is the value of collecting the cd at
    index [i] in [cd_hist]. A 10% penalty is applied if the cd is being
    collected before maturity. Requires: [i] is a valid index in the cd
    list in [cd_hist]. *)
val collect_cd_value : t -> int -> float

(** [remove_cd cd_hist i] is the cd history [cd_hist] with the cd at
    index [i] removed. Requires: [i] >= 0 and [i] is less than the
    number of cds owned. *)
val remove_cd : t -> int -> unit

(** [buy_cd cd_hist amt l] adds a new cd to the cd history [cd_hist]
    with an amount of [amt], for [l] months, and at the current interest
    rate. *)
val buy_cd : t -> float -> Cd.term -> unit

(** [get_investment_value cd_h] is the value of all the cds owned in cd
    history [cd_h]. *)
val get_investment_value : t -> float

(** [create_cd_history filename] is the cd history for a cd with
    interest rates pulled from file [filename]. *)
val create_cd_history : string -> t
