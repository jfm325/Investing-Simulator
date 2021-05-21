open Game

type stock_name = string

type ticker_symbol = string

type t = {
  name : stock_name;
  ticker : ticker_symbol;
  prices : float array;
  mutable current_price : float;
  mutable percent_change : float;
}

let get_name s = s.name

let get_ticker s = s.ticker

let get_price s i = s.prices.(i)

let get_percent_change s = s.percent_change

let get_current_price s = s.current_price

let update_current_prices lst start_time =
  let time = int_of_float (Unix.time () -. start_time) in
  let i = time / Game.s_per_month in
  let i' = if i > 239 then 239 else i in
  let prev = if i' <= 0 then 0 else i' - 1 in
  let update_price (stock : t) =
    stock.current_price <- get_price stock i';
    let diff = stock.current_price -. get_price stock prev in
    let round_to_2 n = Float.round (n *. 100.) /. 100. in
    let new_percent =
      round_to_2 (diff /. stock.current_price *. 100.0)
    in
    stock.percent_change <- new_percent
  in
  List.iter update_price lst

(* [create_prices_array filename n] is the float array of prices of size
   [n] constructed from floats in file [filename].*)
let create_prices_array filename n =
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

let create_stock n t file =
  let prices_arr = create_prices_array file 240 in
  {
    name = n;
    ticker = t;
    prices = prices_arr;
    current_price = prices_arr.(0);
    percent_change = 0.;
  }
