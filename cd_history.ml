open Cd
open Game

type t = {
  (* [interest_rates] is an array of interest rates in decimal form. Ex)
     .10 *)
  interest_rates : float array;
  mutable cd_lst : Cd.t list;
}

let get_current_apy cd_h =
  let current_time = Unix.time () in
  let start_time = Game.get_start_time () in
  let time = int_of_float (current_time -. start_time) in
  let seconds_per_year = Game.s_per_month * 12 in
  let index = time / seconds_per_year in
  cd_h.interest_rates.(index)

let get_cd_lst cd_hist = cd_hist.cd_lst

let get_cds_owned cd_hist = List.length cd_hist.cd_lst

let update_cd_lst_values cd_hist =
  List.iter Cd.update_current_value cd_hist.cd_lst

let collect_cd_value cd_hist i =
  let cd_opt = List.nth_opt cd_hist.cd_lst i in
  match cd_opt with
  | None -> raise (Failure "Index invalid in [collect_cd].")
  | Some cd ->
      Cd.update_current_value cd;
      let value = Cd.get_current_value cd in
      if Cd.is_cd_matured cd then value else value *. 0.9

let remove_cd cd_hist i =
  let cds_owned = get_cds_owned cd_hist in
  if i < cds_owned && i >= 0 then
    let f index cd = not (index = i) in
    let new_cd_lst = List.filteri f cd_hist.cd_lst in
    cd_hist.cd_lst <- new_cd_lst
  else raise (Failure "Index out of bounds in [collect_cd].")

let buy_cd cd_hist amt l =
  let current_time = Unix.time () in
  let start_time = Game.get_start_time () in
  let time = int_of_float (current_time -. start_time) in
  let seconds_per_yr = Game.s_per_month * 12 in
  let i = time / seconds_per_yr in
  let rate = cd_hist.interest_rates.(i) in
  let new_cd = Cd.create_cd rate l amt in
  let new_lst = new_cd :: cd_hist.cd_lst in
  cd_hist.cd_lst <- new_lst

let get_investment_value cd_h =
  update_cd_lst_values cd_h;
  let f acc cd =
    let value = Cd.get_current_value cd in
    acc +. value
  in
  let cd_lst_value = List.fold_left f 0. cd_h.cd_lst in
  cd_lst_value

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
      let apy = float_of_string line.contents in
      a.(!index) <- apy /. 100.;
      index := !index + 1;
      fill_array a
    with End_of_file -> a
  in
  fill_array (Array.create_float n)

let create_cd_history filename =
  let rates_arr = create_rates_arr filename 20 in
  { interest_rates = rates_arr; cd_lst = [] }
