(* open Core *)
open Async

let rec timer () =
  after (Core.sec 5.) >>= fun () ->
  (*call func to read next line of prices*)
  print_endline "STOCK PRICES ARE NOW UPDATED";
  timer ()

let () =
  let cmd =
    Command.async ~summary:"Periodic timer" Command.Spec.empty timer
  in
  Command.run cmd
