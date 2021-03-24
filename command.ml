type object_phrase = string list 

type command = 
  | Buy of object_phrase
  | Sell of object_phrase
  | Cash
  | Networth

exception Empty
exception Malformed

let parse str = 
  let lst = String.split_on_char ' ' str in 
  match lst with 
  | []-> raise Empty
  | [""]-> raise Empty
  | h:: object_phrase -> 
    if h = "" then raise Empty 
    else if h = "cash" && object_phrase = [""] then Cash 
    else if h = "networth" && object_phrase = [""] then Networth 
    else if h = "sell" &&  object_phrase <> [""] then Sell object_phrase 
    else if h = "buy" &&  object_phrase <> [""] then Buy object_phrase 
    else raise Malformed