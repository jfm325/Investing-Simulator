type t = {
  interest_rate : float;
  starting_month : int;
  length : int;
  amount : float;
}

let create_cd rate l amt =
  (* let current_month = Unix.sta *)
  { interest_rate = rate; starting_month = 0; length = l; amount = amt }
