open Stock
open User

val stocks : Stock.t list

val user : User.u

val start_time : float ref

val update_start_time : float -> unit

val get_start_time : unit -> float

val intro_string : string

val instructions : string
