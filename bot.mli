(* Representation of a competing bot.*)

(*This module represents a bot capable of investing in index funds every
  6 months *)

(** The abstract type for the bot *)
type t

(*creates the float array containing all the prices of the index fund*)
val create_index_price_array : string -> int -> float array

(*creates a bot in the game*)
val create_bot : t

(*returns the networth of the bot after it invests in a half-yearly
  basis*)
val get_net_worth : t -> float

(*makes the bot invest 200000. in index funds semi-annually until game
  ends*)
val purchase_indexfunds : t -> unit
