open! Async

let main () =
  let rec loop () = Deferred.return () >>= fun () -> loop () in
  don't_wait_for (loop ());
  printf "test!\n";
  return ()

let () =
  let cmd = Command.async_spec ~summary:"" Command.Spec.empty main in
  Command.run cmd
