(* Representation of a cd (Certificate of Deposit).

   This module represents a cd in the simulation and contains the *)

(** The abstract type for the length until maturity of a cd. *)
type term

(** The abstract type for a stock. *)
type t

(** [get_current_value cd] is the current value of [cd]. *)
val get_current_value : t -> float

(** [is_cd_matured cd] is true if cd [cd] has matured. *)
val is_cd_matured : t -> bool

(** [update_current_value cd] is the cd [cd] with an updated current
    value of the investment. *)
val update_current_value : t -> t

(** [create_cd rate length amt] is the cd with interest rate [rate],
    length til maturity [length], amount placed of [amt], and the month
    it was bought in (given by current time). *)
val create_cd : float -> term -> float -> t
