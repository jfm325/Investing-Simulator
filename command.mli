(** Parsing of player commands. *)

(**********************************************************************
 
 **********************************************************************)

(** The type [object_phrase] represents the object phrase that can be
    part of a player command. Each element of the list represents a word
    of the object phrase, where a {i word} is defined as a consecutive
    sequence of non-space characters. Thus, no element of the list
    should contain any leading, internal, or trailing spaces. The list
    is in the same order as the words in the original player command.
    For example:

    - If the player command is ["buy TSLA 50"], then the object
      phrase is [\["TSLA"; "50"\]].

    - The first element in the object pharse represents the stock and the 
    second element is the #shares purchased/sold in the transaction

    An [object_phrase] is not permitted to be the empty list. *)
    type object_phrase = string list

    (** The type [command] represents a player command that is decomposed
        into a verb and possibly an object phrase. *)
    type command =
      | Buy of object_phrase
      | Sell of object_phrase
      | Cash
      | Networth 
    
    (** Raised when an empty command is parsed. *)
    exception Empty
    
    (** Raised when a malformed command is encountered. *)
    exception Malformed
    
    (** [parse str] parses a player's input into a [command], as follows.
        The first word (i.e., consecutive sequence of non-space characters)
        of [str] becomes the activity, so the player can buy/sell/choose to 
        display cash/networth. The rest of the words, if any, become the
        object phrase. Examples:
    
        - [parse "buy TSLA 50"] is [Buy \["TSLA"; "50"\]]
        - [parse "cash"] is [Cash].
    
        Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
        characters (only ASCII character code 32; not tabs or newlines,
        etc.).
    
        Raises: [Empty] if [str] is the empty string or contains only
        spaces.
    
        Raises: [Malformed] if the command is malformed. A command is
        {i malformed} if the verb is neither "quit" nor "go", or if the verb
        is "cash" / "networth" and there is a non-empty object phrase, or if 
        the verb is "buy" / "sell" and there is an empty object phrase.*)
    val parse : string -> command
    