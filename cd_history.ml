open Cd

type t = {
  interest_rates : float array;
  mutable cds_owned : int;
  mutable cd_lst : Cd.t list;
}

(* let buy_cd cd_hist amount length = *)
(* cd_hist.cd_lst <- cd_hist.cd_lst :: *)

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
