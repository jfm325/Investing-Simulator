open User
open Game
open Stock

type t = {
  mutable index_hist : Index_history.i list;
  index_price_arr : float array;
  mutable networth : float;
  mutable shares : int;
}

let create_index_price_array filename n =
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

let create_bot =
  {
    index_hist = [];
    index_price_arr = create_index_price_array "spy_index1995.txt" 240;
    networth = 0.;
    shares = 0;
  }

let get_index_price_lst bot = bot.index_price_arr

let get_current_price bot =
  let ended = Game.game_ended () in
  let time = int_of_float (Unix.time () -. Game.get_start_time ()) in
  let i = if ended then 239 else time / Game.s_per_month in
  let price = (get_index_price_lst bot).(i) in
  price

let get_net_worth bot = bot.networth

let rec calc_net_worth lst bot counter =
  match lst with
  | [] -> counter
  | h :: t ->
      let value = float_of_int bot.shares *. get_current_price bot in
      calc_net_worth t bot (counter +. value)

let buy_index_bot bot i =
  let price = (get_index_price_lst bot).(i) in
  let n = int_of_float (200000. /. price) in
  let index_new = Index_history.create_index_history "SPY" in
  let sh = Index_history.buy index_new price n in
  bot.index_hist <- sh :: bot.index_hist;
  bot.shares <- bot.shares + n;
  bot.networth <- calc_net_worth bot.index_hist bot 0.

let purchase_indexfunds bot =
  let ended = Game.game_ended () in
  let time = int_of_float (Unix.time () -. Game.get_start_time ()) in
  let i = if ended then 239 else time / Game.s_per_month in
  if i mod 6 = 0 then buy_index_bot bot i
