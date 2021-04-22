open Cd
open Game

type t = {
  interest_rates : float array;
  mutable cds_owned : int;
  mutable cd_lst : Cd.t list;
}

(* let collect_cd cd_hist i = if i < cd_hist.cds_owned then *)

let buy_cd cd_hist amt l =
  let current_time = Unix.time () in
  let start_time = Game.get_start_time () in
  let time = int_of_float (current_time -. start_time) in
  let seconds_per_yr = Game.s_per_month * 12 in
  let i = time / seconds_per_yr in
  let rate = cd_hist.interest_rates.(i) in
  let new_cd = Cd.create_cd rate l amt in
  let new_lst = new_cd :: cd_hist.cd_lst in
  cd_hist.cd_lst <- new_lst;
  cd_hist.cds_owned <- cd_hist.cds_owned + 1;
  cd_hist

(** [create_rates_arr filename n] is an array of length [n] filled with
    interest rates from file [filename]. Requires: [filename] has a
    float on each line and there are a minimum of [n] lines. *)
let create_rates_arr filename n =
  let channel = open_in filename in
  let line = ref "" in
  let index = ref 0 in
  let rec fill_array a =
    try
      line := input_line channel;
      a.(!index) <- float_of_string line.contents;
      index := !index + 1;
      fill_array a
    with End_of_file -> a
  in
  fill_array (Array.create_float n)

let create_cd_history filename =
  let rates_arr = create_rates_arr filename 20 in
  { interest_rates = rates_arr; cds_owned = 0; cd_lst = [] }
