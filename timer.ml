open Core
open Async
let rec timer () =
  after (sec 5.)
  >>= fun _ ->
    print_endline "Hello";
    timer ()

let _ =
  Command.async ~summary:"Periodic timer"
    Command.Spec.empty timer
  |> Command.run
  