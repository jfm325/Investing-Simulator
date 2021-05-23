(* open User *)
open Game
open Stock

type t = {
  mutable index_hist : Index_history.i list;
  index_price_arr : float array;
  mutable networth : float; (* mutable shares : int; *)
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
    networth = 0. (* shares = 0; *);
  }

let get_index_price_lst bot = bot.index_price_arr

let get_current_price bot =
  let ended = Game.game_ended () in
  let time = int_of_float (Unix.time () -. Game.get_start_time ()) in
  let i = if ended then 239 else time / Game.s_per_month in
  let price = (get_index_price_lst bot).(i) in
  price

(* [calc_networth_helper acc times_to_buy shares cash] is the helper
   function to get number of shares owned at the current time for the
   bot. *)
let rec calc_networth_helper price_arr acc times_to_buy shares cash =
  if acc > times_to_buy then (shares, cash)
  else
    let index = (6 * acc) - 1 in
    let price = price_arr.(index) in
    let shares_to_buy = int_of_float (cash /. price) in
    let left_over_cash =
      20000. -. (float_of_int shares_to_buy *. price)
    in
    let new_shares = shares + shares_to_buy in
    (* Adding 10,000 for income. *)
    let new_cash = cash +. left_over_cash +. 10000. in
    calc_networth_helper price_arr (acc + 1) times_to_buy new_shares
      new_cash

let calc_net_worth bot =
  let current_time = Unix.time () in
  let start_time = Game.get_start_time () in
  let time = int_of_float (current_time -. start_time) in
  let curr_index = time / Game.s_per_month in
  let index = if curr_index > 239 then 239 else curr_index in
  let times_to_buy = (index / 6) + 1 in
  let intial_price = bot.index_price_arr.(0) in
  let intial_shares = int_of_float (20000. /. intial_price) in
  let left_over_cash =
    20000. -. (float_of_int intial_shares *. intial_price)
  in
  let total_shares, total_cash =
    calc_networth_helper bot.index_price_arr 1 times_to_buy
      intial_shares left_over_cash
  in
  let invest_value =
    get_current_price bot *. float_of_int total_shares
  in
  invest_value +. total_cash

let get_net_worth bot = calc_net_worth bot

(* let buy_index_bot bot i = let price = (get_index_price_lst bot).(i)
   in let n = int_of_float (200000. /. price) in let index_new =
   Index_history.create_index_history "SPY" in let sh =
   Index_history.buy index_new price n in bot.index_hist <- sh ::
   bot.index_hist; bot.shares <- bot.shares + n; bot.networth <-
   calc_net_worth bot.index_hist bot 0. *)

(* let purchase_indexfunds bot = let ended = Game.game_ended () in let
   time = int_of_float (Unix.time () -. Game.get_start_time ()) in let i
   = if ended then 239 else time / Game.s_per_month in if i mod 3 = 0
   then buy_index_bot bot i *)
