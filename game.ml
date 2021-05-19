let start_time = ref 0.

let s_per_month = 1

let update_start_time t = start_time := t

let get_start_time () = !start_time

let str_of_year_month time_elapsed =
  let index = time_elapsed / s_per_month in
  let month =
    let m = index + 1 in
    if m mod 12 = 0 then 12 else m mod 12
  in
  let year =
    let m = index + 1 in
    if m mod 12 = 0 then m / 12 else (m / 12) + 1
  in
  let year_str = string_of_int year in
  let month_str = string_of_int month in
  if index >= 240 then "End of Game"
  else "Year " ^ year_str ^ " Month " ^ month_str

let game_ended s =
  let current_time = int_of_float (Unix.time () -. !start_time) in
  let index = current_time / s in
  index >= 240
