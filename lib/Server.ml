open Core
open Async
open Util

let start_server ~port ~nick ~uniqueMessageNumber ~uniqueAcknowledgementNumber ~stdin_pipe =
  Deferred.ignore_m (
  Monitor.protect (fun () ->
    printf "Starting server on port %d with nickname '%s'\n" port nick;
    try_with (fun () ->
      Tcp.Server.create
        ~on_handler_error:`Raise
        ~max_connections:1
        (* ~drop_incoming_connections:true *)
        (Tcp.Where_to_listen.of_port port)
        (fun _addr reader writer ->
          printf "Connected socket address: %s\n" (Socket.Address.to_string _addr);
          printf "Reader type: %s\n" (Async.Reader.sexp_of_t reader |> Sexp.to_string_hum);
          printf "Writer type: %s\n" (Async.Writer.sexp_of_t writer |> Sexp.to_string_hum);
          printf "The message id is: %d\n" !uniqueMessageNumber;
          printf "The acknowledgement id is: %d\n" !uniqueAcknowledgementNumber;
          Deferred.unit
        )
    ) >>= function
    | Ok _ -> Deferred.never ()
    | Error exn ->
      let%bind () = Deferred.return (Pipe.close_read stdin_pipe) in
      begin match Monitor.extract_exn exn with
      | Unix.Unix_error (Unix.Error.EADDRINUSE, _, _) ->
        let error_message = sprintf "Port %d is already in use.\n%!" port in
        let pretty_erorr_message = pretty_error_message_string error_message in
        let () = print_endline pretty_erorr_message in
        Shutdown.exit 0
      | exn_message ->
        let pretty_error_message = (Exn.to_string exn_message) in 
        let () = print_endline pretty_error_message in
        Shutdown.exit 1
      end
  )
  ~finally:(fun () ->
    let info_message = sprintf "Server on port %d has stopped.\n" port in
    let pretty_info_message = pretty_info_message_string info_message in
    let () = print_endline pretty_info_message in
    Deferred.unit
  )
)