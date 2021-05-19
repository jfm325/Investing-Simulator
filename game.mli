(* This module handles the time in the game and determining when the
   game has ended. *)

val start_time : float ref

val s_per_month : int

val update_start_time : float -> unit

val get_start_time : unit -> float

(** [str_of_year_month time_elapsed] is the string in format "Year _
    Month _" based on the current [elapsed_time] in the game. *)
val str_of_year_month : int -> string

(** [game_ended s] is true if the game has ended calculated based on the
    number of seconds [s] in which stocks are updated. *)
val game_ended : int -> bool
