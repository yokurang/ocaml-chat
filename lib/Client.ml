open Core
open Async
open Utils
open DataTypes

let start_client ~host ~port ~nick ~global_state ~stdin_pipe =
  Deferred.ignore_m (
  Monitor.protect (fun () ->
    printf "Client has joined the chat in address %s:%d with nickname %s\n%!" host port nick;
    try_with (fun () ->
      Tcp.with_connection
        (Tcp.Where_to_connect.of_host_and_port { host; port })
        ?timeout:(Some (Time_float_unix.Span.of_sec 5.))
        ?interrupt:(Some (Deferred.never ()))
        (fun _sock reader writer ->
          printf "Connected to server\n%!";
          (* Extract the socket address from the socket object *)
          let sock_addr = Socket.getpeername _sock in
          let sock_str = Socket.Address.to_string sock_addr in
          printf "Connected socket address: %s\n" sock_str;
          printf "Reader type: %s\n" (Async.Reader.sexp_of_t reader |> Sexp.to_string_hum);
          printf "Writer type: %s\n" (Async.Writer.sexp_of_t writer |> Sexp.to_string_hum);
          printf "The message id is: %d\n" !(global_state.uniqueMessageNumber);
          printf "The acknowledgement id is: %d\n" !(global_state.uniqueAcknowledgementNumber);

          printf "Waiting for server to acknowledge connection...\n%!";

          Deferred.never()
        )
    ) >>= function
    | Ok () -> Deferred.unit
    | Error exn ->
      let%bind () = Deferred.return (Pipe.close_read stdin_pipe) in
      begin match Monitor.extract_exn exn with
      | Unix.Unix_error (Unix.Error.ECONNREFUSED, _, _) ->
        let error_message = sprintf "Server is not running on %s:%d\n%!" host port in
        let pretty_error_message = pretty_error_message_string error_message in
        let () = print_endline pretty_error_message in
        Shutdown.exit 0
      | exn_message ->
        let pretty_error_message = pretty_error_message_string (Exn.to_string exn_message) in
        let () = print_endline pretty_error_message in
        Shutdown.exit 1
      end
  )
  ~finally:(fun () ->
    let info_message = sprintf "Client %s has disconnected from the chat on %s:%d\n%!" nick host port in
    let pretty_info_message = pretty_info_message_string info_message in
    let () = print_endline pretty_info_message in
    Deferred.unit
  )
)