type term = SixMonths | OneYear | ThreeYears

type t = {
  interest_rate : float;
  starting_month : int;
  length : int;
  amount : float;
}

(* [match_new_rate l r] is the new rate based on the rate for 1 year
   [r], and the length of the cd [l]. *)
let match_new_rate l r =
  match l with
  | SixMonths -> if r -. 1. < 0. then r else r -. 1.
  | OneYear -> r
  | ThreeYears -> r +. 1.

(* [term_to_months l] is the length of a cd in terms of months. *)
let term_to_months l =
  match l with SixMonths -> 6 | OneYear -> 12 | ThreeYears -> 36

let create_cd rate length amt =
  let current_time = Unix.time () in
  let start_time = Game.get_start_time () in
  let time = int_of_float (current_time -. start_time) in
  let month_index = time / Game.s_per_month in
  let l = term_to_months length in
  let new_rate = match_new_rate length rate in
  {
    interest_rate = new_rate;
    starting_month = month_index;
    length = l;
    amount = amt;
  }
