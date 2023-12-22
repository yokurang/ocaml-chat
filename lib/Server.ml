open Core
open Async
open DataTypes
open Utils

let start_server ~port ~nick ~global_state ~sender_type ~stdin_reader_pipe =
  Deferred.ignore_m (
  Monitor.protect (fun () ->
    let () = global_state.server_nickname := (Some nick) in
    let header = sprintf "\n|| Server Startup Information ||\n" in
    let info_message = sprintf "Server '%s' has started on port %d. Awaiting connections..." nick port in
    let pretty_info_message = pretty_info_message_string info_message in
    let start_up_message = sprintf "%s\n%s\n" header pretty_info_message in
    print_endline start_up_message;
    try_with (fun () ->
      Tcp.Server.create
        ~on_handler_error:`Raise
        ~max_connections:1
        (* ~drop_incoming_connections:false *)
        (Tcp.Where_to_listen.of_port port)
        (fun _addr reader writer ->
          let client_socket_addr_str = Socket.Address.to_string _addr in
          let () = global_state.client_connection_address := (Some client_socket_addr_str) in
          let socket_reader_pipe = Reader.pipe reader in
          let socket_writer_pipe = Writer.pipe writer in
          InputOutputHandlers.handle_connection
            ~global_state
            ~sender_type
            ~socket_reader_pipe: socket_reader_pipe
            ~socket_writer_pipe: socket_writer_pipe
            ~stdin_reader_pipe: stdin_reader_pipe
        )
    ) >>= function
    | Ok _ -> 
      Deferred.never ()
    | Error exn ->
      let%bind () = Deferred.return (Pipe.close_read stdin_reader_pipe) in
      begin match Monitor.extract_exn exn with
      | Unix.Unix_error (Unix.Error.EADDRINUSE, _, _) ->
        let error_message = Printf.sprintf "Port %d is already in use.\n%!" port in
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
    let info_message = Printf.sprintf "Shutting down..." in
    let pretty_info_message = pretty_info_message_string info_message in
    let () = print_endline pretty_info_message in
    Deferred.unit
  )
)