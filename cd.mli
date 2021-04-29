(* Representation of a cd (Certificate of Deposit).

   This module represents a cd in the simulation and contains the *)

(** The abstract type for the length until maturity of a cd. *)
type term = SixMonths | OneYear | ThreeYears

(** The abstract type for a stock. *)
type t

(* [match_new_rate l r] is the new rate based on the rate for 1 year
   [r], and the length [l] of the cd. *)
val match_new_rate : term -> float -> float

(** [get_apy_percentage cd] is the percent form of the apy for cd [cd]. *)
val get_apy_percentage : t -> float

(** [get_apy cd] is the APY (decimal form) of [cd]. *)
val get_apy : t -> float

(** [get_monthly_rate cd] is the monthly percentage yield of [cd]. *)
val get_monthly_rate : t -> float

(** [get_length cd] is the length until maturity of [cd]. *)
val get_length : t -> int

(** [get_current_value cd] is the current value of [cd]. *)
val get_current_value : t -> float

(** [months_until_maturity cd] is the number of months until [cd]
    matures or it is 0 if [cd] has matured. *)
val months_until_maturity : t -> int

(** [is_cd_matured cd] is true if cd [cd] has matured. *)
val is_cd_matured : t -> bool

(** [update_current_value cd] updates the current value of the
    investment for cd [cd]. *)
val update_current_value : t -> unit

(** [create_cd rate length amt] is the cd with interest rate [rate],
    length til maturity [length], amount placed of [amt], and the month
    it was bought in (given by current time). *)
val create_cd : float -> term -> float -> t
