open Core
open Async
open Utils
open DataTypes
open InputOutputHandlers

let start_client ~host ~port ~nick ~global_state ~sender_type ~stdin_reader_pipe =
  Deferred.ignore_m (
  Monitor.protect (fun () ->
    let () = global_state.client_nickname := (Some nick) in
    try_with (fun () ->
      Tcp.with_connection
        (Tcp.Where_to_connect.of_host_and_port { host; port })
        ?timeout:(Some (Time_float_unix.Span.of_sec 5.))
        (fun _sock reader writer ->
          let () = printf "\nWaiting to connect to server...\n%!" in
          let server_socket_addr = Socket.getpeername _sock in
          let server_socket_addr_str = Socket.Address.to_string server_socket_addr in
          let () = global_state.server_connection_address := (Some server_socket_addr_str) in
          let socket_reader_pipe = Reader.pipe reader in
          let socket_writer_pipe = Writer.pipe writer in
          let client_connection_message = ClientConnection { client_nickname = nick } in
          let () = write_message socket_writer_pipe client_connection_message in
          handle_connection
            ~global_state
            ~sender_type
            ~socket_reader_pipe
            ~socket_writer_pipe
            ~stdin_reader_pipe
        ) 
    ) >>= function
    | Ok () ->
      Deferred.unit
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
    let info_message = Printf.sprintf "Closing connection..." in
    let pretty_info_message = pretty_info_message_string info_message in
    let () = print_endline pretty_info_message in
    Deferred.unit
  )
)