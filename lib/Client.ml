open Core
open Async
open Utils
open InputOutputHandlers

let start_client ~host ~port ~stdin_reader_pipe =
  Deferred.ignore_m (
  Monitor.protect (fun () ->
    let () = print_endline (pretty_info_message_string
    (sprintf "Connecting to server on host %s and port %d" host port)) in
    let greeting_phrases : string list = [
      "Say something nice!"; "Will sparks fly this time?";
      "Maybe ask about their day!"; "Why not ask about their day?"
      ] in
    try_with (fun () ->
      Tcp.with_connection
        (Tcp.Where_to_connect.of_host_and_port { host; port })
        ?timeout:(Some (Time_float_unix.Span.of_sec 3.)) (* is this too short? *)
        ?interrupt:(Some (Deferred.never ())) (* never interrupt connection *)
        (fun _sock reader writer ->
          let server_socket_addr = Socket.getpeername _sock in
          let server_socket_addr_str = Socket.Address.to_string server_socket_addr in
          let greeting_phrase = List.random_element_exn greeting_phrases in
          let () = print_endline (pretty_info_message_string
          (sprintf "Connected to %s. %s"
          server_socket_addr_str greeting_phrase)) in
          let socket_reader_pipe = Reader.pipe reader in
          let socket_writer_pipe = Writer.pipe writer in
          handle_connection
            ~socket_reader_pipe:socket_reader_pipe
            ~socket_writer_pipe:socket_writer_pipe
            ~stdin_reader_pipe:stdin_reader_pipe
            ~connection_address:server_socket_addr_str
        ) 
    ) >>= function
    | Ok () ->
      Deferred.return (Pipe.close_read stdin_reader_pipe)
    | Error exn ->
      let%bind () = Deferred.return (Pipe.close_read stdin_reader_pipe) in
      begin match Monitor.extract_exn exn with
      | Unix.Unix_error (Unix.Error.ECONNREFUSED, _, _) ->
        let error_message = sprintf "Server is not running on %s:%d%!\n" host port in
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
    let info_message = sprintf "Closing connection..." in
    let pretty_info_message = pretty_info_message_string info_message in
    let () = print_endline pretty_info_message in
    Deferred.unit
  )
)
