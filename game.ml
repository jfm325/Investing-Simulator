let start_time = ref 0.

let update_start_time t = start_time := t

let get_start_time () = !start_time

let game_ended s =
  let current_time = int_of_float (Unix.time () -. !start_time) in
  let month = current_time / s in
  let year =
    if month = 0 then 1
    else if month mod 12 = 0 then month / 12
    else (month / 12) + 1
  in
  (* End game when we reach last index. *)
  if month >= 240 then (
    (* Should update state in main.ml *)
    print_endline "End of game";
    true )
  else (
    print_endline
      ("Year " ^ string_of_int year ^ " Month " ^ string_of_int month);
    false )
