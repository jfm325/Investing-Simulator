open Stock

let stocks : Stock.t list =
  [
    Stock.create_stock "Coke" "COKE" "coke1995.txt";
    Stock.create_stock "Apple" "AAPL" "aapl1995.txt";
    Stock.create_stock "Microsoft Corporation" "MSFT" "msft1995.txt";
  ]

let coke_history = Stock_history.create_stock_history "COKE"

let aapl_history = Stock_history.create_stock_history "AAPL"

let msft_history = Stock_history.create_stock_history "MSFT"

let stock_history_lst = [ coke_history; aapl_history; msft_history ]
