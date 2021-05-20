type t

val create_index_price_array : string -> int -> float array

val create_bot : t

val get_net_worth : t -> float

val purchase_indexfunds : t -> unit
