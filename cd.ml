type t = {
  interest_rate : float;
  starting_month : int;
  length : int;
  amount : float;
}

let create_cd rate l amt =
  let current_time = Unix.time () in
  let start_time = Game.get_start_time () in
  let time = int_of_float (current_time -. start_time) in
  let month_index = time / Game.s_per_month in
  {
    interest_rate = rate;
    starting_month = month_index;
    length = l;
    amount = amt;
  }
