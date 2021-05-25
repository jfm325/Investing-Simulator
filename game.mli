(* This module handles the time in the game and determines when the game
   has ended. *)

(** [start_time] is the start time of the game represented in float
    format *)
val start_time : float ref

(** [s_per_month] sets 1 second to one month in the game*)
val s_per_month : int

(** [update_start_time] updates the start time *)
val update_start_time : float -> unit

(** [get_start_time] getter for the start time *)
val get_start_time : unit -> float

(** [str_of_year_month time_elapsed] is the string in format "Year _
    Month _" based on the current [elapsed_time] in the game. *)
val str_of_year_month : int -> string

(** [game_ended ()] is true if the game has ended calculated based
    [s_per_month] in which stocks are updated. *)
val game_ended : unit -> bool
