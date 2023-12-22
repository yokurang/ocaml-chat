open Core
open Async
open Utils

let start_server ~port ~nick ~global_state ~participant_type ~stdin_reader_pipe =
  Deferred.ignore_m (
  Monitor.protect (fun () ->
    let start_up_message ~nick ~port =
      let header = Printf.sprintf "\n|| Server Startup Information ||\n" in
      let info_message = Printf.sprintf "Server '%s' has started on port %d.\nAwaiting connections..." nick port in
      Printf.sprintf "%s\n%s\n" header info_message
    in let startup_message = start_up_message ~nick ~port in
    print_endline startup_message;
    try_with (fun () ->
      Tcp.Server.create
        ~on_handler_error:`Raise
        ~max_connections:1
        (* ~drop_incoming_connections:true *)
        (Tcp.Where_to_listen.of_port port)
        (fun _addr reader writer ->
          let socket_addr_str = Socket.Address.to_string _addr in
          let addr_log = pretty_info_message_string (Printf.sprintf "The Server has the socket address: %s" socket_addr_str) in
          let () = print_endline addr_log in
          let socket_reader_pipe = Reader.pipe reader in
          let socket_writer_pipe = Writer.pipe writer in
          InputOutputHandlers.handle_connection
            ~global_state
            ~participant_type
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
    let info_message = Printf.sprintf "Server on port %d has stopped.\n" port in
    let pretty_info_message = pretty_info_message_string info_message in
    let () = print_endline pretty_info_message in
    Deferred.unit
  )
)