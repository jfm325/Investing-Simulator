type term = SixMonths | OneYear | ThreeYears

type t = {
  apy : float;
  monthly_rate : float;
  starting_month_index : int;
  length : int;
  amount : float;
  mutable current_value : float;
}

let get_current_value cd = cd.current_value

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

let is_cd_matured cd =
  let current_time = Unix.time () in
  let start_time = Game.get_start_time () in
  let time = int_of_float (current_time -. start_time) in
  let current_month_index = time / Game.s_per_month in
  let current_term = current_month_index - cd.starting_month_index in
  current_term >= cd.length

let update_current_value cd =
  let current_time = Unix.time () in
  let start_time = Game.get_start_time () in
  let time = int_of_float (current_time -. start_time) in
  let current_month_index = time / Game.s_per_month in
  let current_term = current_month_index - cd.starting_month_index in
  if current_term < cd.length then (
    let r = cd.monthly_rate ** float_of_int current_term in
    cd.current_value <- cd.amount *. r;
    cd )
  else
    let r = cd.monthly_rate ** float_of_int cd.length in
    cd.current_value <- cd.amount *. r;
    cd

(* [match_monthly_rate l r] is the monthly rate given by APY [r] and
   based on the length of the *)
let match_monthly_rate l r =
  match l with
  | SixMonths -> (r /. 6.) +. 1.
  | OneYear -> (r /. 12.) +. 1.
  | ThreeYears -> (r /. 36.) +. 1.

let create_cd rate length amt =
  let current_time = Unix.time () in
  let start_time = Game.get_start_time () in
  let time = int_of_float (current_time -. start_time) in
  let month_index = time / Game.s_per_month in
  let l = term_to_months length in
  let new_rate = match_new_rate length rate in
  let m_rate = match_monthly_rate length new_rate in
  {
    apy = new_rate;
    monthly_rate = m_rate;
    starting_month_index = month_index;
    length = l;
    amount = amt;
    current_value = amt;
  }
