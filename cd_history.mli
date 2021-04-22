(* Representation of investment history in cds (Certificate of
   Deposits).

   This module represents a user's investment history in cds. *)

(** The abstract type for a portfolio. *)
type t

(** [create_cd_history filename] is the cd history for a cd with
    interest rates pulled from file [filename]. *)
val create_cd_history : string -> t
