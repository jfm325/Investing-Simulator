type stock_name = string

type ticker_symbol = string

type t = {
  name : stock_name;
  ticker : ticker_symbol;
  prices : float array;
  current_price : float;
}

let get_name s = s.name

let get_ticker s = s.ticker

let get_price s i = s.prices.(i)

let get_current_price s = s.current_price

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
  }
