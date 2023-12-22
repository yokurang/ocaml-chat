open Core
open Async
open Utils
open DataTypes

let start_client ~host ~port ~nick ~global_state ~sender_type ~stdin_reader_pipe =
  Deferred.ignore_m (
  Monitor.protect (fun () ->
    let () = global_state.client_nickname := (Some nick) in
    try_with (fun () ->
      Tcp.with_connection
        (Tcp.Where_to_connect.of_host_and_port { host; port })
        ?timeout:(Some (Time_float_unix.Span.of_sec 5.))
        ?interrupt:(Some (Deferred.never ()))
        (fun _sock reader writer ->
          let server_socket_addr_str = Socket.getpeername _sock |> Socket.Address.to_string in
          let () = global_state.server_connection_address := (Some server_socket_addr_str) in
          let () = printf "\nWaiting for server to acknowledge connection...\n%!" in
          let connection_request_from = ConnectionRequest { user_nickname = nick } in
          let socket_reader_pipe = Reader.pipe reader in
          let socket_writer_pipe = Writer.pipe writer in
          let () = InputOutputHandlers.write_message socket_writer_pipe connection_request_from in
          InputOutputHandlers.handle_connection
            ~global_state
            ~sender_type
            ~socket_reader_pipe
            ~socket_writer_pipe
            ~stdin_reader_pipe
        ) 
    ) >>= function
    | Ok () ->
      let info_message = Printf.sprintf "Server %s has disconnected from the chat on %s:%d\n%!" nick host port in
      let pretty_info_message = pretty_info_message_string info_message in
      let () = print_endline pretty_info_message in
      let%bind () = Deferred.return (Pipe.close_read stdin_reader_pipe) in
      Shutdown.exit 0
    | Error exn ->
      let%bind () = Deferred.return (Pipe.close_read stdin_reader_pipe) in
      begin match Monitor.extract_exn exn with
      | Unix.Unix_error (Unix.Error.ECONNREFUSED, _, _) ->
        let error_message = Printf.sprintf "Server is not running on %s:%d\n%!" host port in
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
    let info_message = Printf.sprintf "Server %s has disconnected from the chat on %s:%d\n%!" nick host port in
    let pretty_info_message = pretty_info_message_string info_message in
    let () = print_endline pretty_info_message in
    Deferred.unit
  )
)