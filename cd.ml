type term = SixMonths | OneYear | ThreeYears

type t = {
  (* [apy] is represented in decimal form. Ex) 10% => 0.10*)
  apy : float;
  (* [monthly_rate] is represented in decimal form and with 1 infront.
     Ex) 10% => 1.10*)
  monthly_rate : float;
  starting_month_index : int;
  length : int;
  amount : float;
  mutable current_value : float;
}

let get_apy_percentage cd = cd.apy *. 100.

let get_apy cd = cd.apy

let get_monthly_rate cd = cd.monthly_rate

let get_length cd = cd.length

let get_current_value cd =
  let round_to_2 n = Float.round (n *. 100.) /. 100. in
  round_to_2 cd.current_value

let match_new_rate l r =
  match l with
  | SixMonths -> if r -. 0.01 <= 0. then r else r -. 0.01
  | OneYear -> r
  | ThreeYears -> r +. 0.01

(* [term_to_months l] is the length of a cd in terms of months. *)
let term_to_months l =
  match l with SixMonths -> 6 | OneYear -> 12 | ThreeYears -> 36

(* [get_current_month_index ()] is a helper function to get the current
   month index from the starting time of the game. *)
let get_current_month_index () =
  let current_time = Unix.time () in
  let start_time = Game.get_start_time () in
  let time = int_of_float (current_time -. start_time) in
  let current_month_index = time / Game.s_per_month in
  current_month_index

let months_until_maturity cd =
  let current_month_index = get_current_month_index () in
  let current_term = current_month_index - cd.starting_month_index in
  let months_left = cd.length - current_term in
  if months_left < 0 then 0 else months_left

let is_cd_matured cd =
  let current_month_index = get_current_month_index () in
  let current_term = current_month_index - cd.starting_month_index in
  current_term >= cd.length

let update_current_value cd =
  let current_month_index = get_current_month_index () in
  let current_term = current_month_index - cd.starting_month_index in
  if current_term < cd.length then
    let r = cd.monthly_rate ** float_of_int current_term in
    cd.current_value <- cd.amount *. r
  else
    let r = cd.monthly_rate ** float_of_int cd.length in
    cd.current_value <- cd.amount *. r

let match_monthly_rate l r =
  let round_to_5 n = Float.round (n *. 100000.) /. 100000. in
  match l with
  | SixMonths ->
      let total_apy = (r +. 1.) ** (1. /. 2.) in
      round_to_5 (total_apy ** (1. /. 6.))
  | OneYear ->
      let total_apy = r +. 1. in
      round_to_5 (total_apy ** (1. /. 12.))
  | ThreeYears ->
      let total_apy = (r +. 1.) ** 3. in
      round_to_5 (total_apy ** (1. /. 36.))

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
