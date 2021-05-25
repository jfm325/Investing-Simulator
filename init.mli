(* Representation of info to initialize game.

   This module contains info to intiliaze stocks, index funds, cds,
   user, and bot in the game. Also contains strings used in display. *)

(** [stocks] is the list of stocks in the game. *)
val stocks : Stock.t list

(** [index_funds] is the list of index funds in the game. *)
val index_funds : Stock.t list

(** [stock_history_lst] is the list of history for each stock for the
    user. *)
val stock_history_lst : Stock_history.t list

(** [index_history_lst] is the list of history for each index fund for
    the user. *)
val index_history_lst : Index_history.i list

(** [cd_history] contains the history of certificates of deposits for
    the user. *)
val cd_history : Cd_history.t

(** [user] contains info about the user in the game. *)
val user : User.t

(** [bot] contains info about the bot in the game. *)
val bot : Bot.t

(** [bar] string representation of bar *)
val bar : string

(** [shares_str] string representation of shares *)
val shares_str : string

(** [percent_str] string representation of percent *)
val percent_str : string

(** [tickers_str] string representation of tickers *)
val ticker_str : string

(** [index_fund_str] string representation of index funds *)
val index_fund_str : string

(** [prices_str] string representation of prices *)
val prices_str : string

(** [profit_loss_str] string representation of profit or loss *)
val profit_loss_str : string

(** [intro_string ] string representation of intro of the game *)
val intro_string : string

(** [instructions] string representation of instructions of the game *)
val instructions : string
