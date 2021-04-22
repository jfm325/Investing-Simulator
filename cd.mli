(* Representation of a cd (Certificate of Deposit).

   This module represents a cd in the simulation and contains the *)

(** The abstract type for a stock. *)
type t

(** [create_cd rate l amt] is the cd with interest rate [rate], length
    til maturity [l], and amount placed of [amt]. *)
val create_cd : float -> int -> float -> t
