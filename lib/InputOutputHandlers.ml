open Core
open Async
open DataTypes
open Utils

let write_message (w : string Pipe.Writer.t) (given_message : message) : unit =
  match given_message with
  | Fail {error_message} ->
    let err_message = Printf.sprintf "Failed to send message: %s" error_message in
    let pretty_error_message = pretty_error_message_string err_message in
    print_endline pretty_error_message
  | _ ->
    if not (Pipe.is_closed w)
    then
      sexp_of_message given_message |> Sexp.to_string_hum |> Pipe.write w >>>
      fun () ->()
    else 
      let error_message = "Writer pipe is closed" in
      let pretty_error_message = pretty_error_message_string error_message in
      print_endline pretty_error_message

let handle_stdin_payload payload ~sender_type writer_pipe : unit Deferred.t =
  try_with (fun () ->
    match payload with
    | InputEof -> 
      return ()
    | InputOk message ->
      let sender = sender_type in
      let receiver = show_recipient_type ~sender_type in
      let time_ns_now = Time_ns_unix.to_string (Time_ns_unix.now ()) in
      let new_message = create_message
        ~content:(Some message)
        ~message_from:sender
        ~message_to:receiver
        ~timestamp:time_ns_now 
      in
      let () = write_message writer_pipe new_message in
      return ()
  )
  >>= function
  | Ok () ->
    return ()
  | Error exn ->
      let error_message = pretty_error_message_string (Exn.to_string exn) in
      print_endline error_message;
      return ()

let parse_messages message =
  try 
    let sexps = Sexp.of_string_many message in
    List.map sexps ~f:message_of_sexp
  with
  | exn -> 
    let error_message = pretty_error_message_string (Exn.to_string exn) in
    let error_payload = Fail { error_message = error_message } in
    [error_payload]

let rec handle_socket_message messages ~global_state ~sender_type writer_pipe : bool Deferred.t =
  let time_ns_now = Time_ns_unix.to_string (Time_ns_unix.now ()) in
  match messages with
  | [] -> return true
  | Acknowledgement { message_content; message_from; message_to; message_timestamp } :: tl ->
    let ack = Acknowledgement {
      message_content;
      message_from;
      message_to;
      message_timestamp;
    } in let connection_addres = show_connection_address ~global_state ~sender_type in
    let () = print_acknowledgement connection_addres ack time_ns_now in
    let () = print_acknowledgement_log ~global_state connection_addres ack time_ns_now in
    handle_socket_message tl ~global_state ~sender_type writer_pipe
  | Message { message_content; message_from; message_to; timestamp } :: tl ->
    let msg = Message {
      message_content;
      message_from;
      message_to;
      timestamp;
    } in let () = print_chat_message ~global_state ~sender_type msg in
      let ack_message = create_acknowledgement_from_message msg in
      let () = write_message writer_pipe ack_message in
      handle_socket_message tl ~global_state ~sender_type writer_pipe
  | ClientConnection { client_nickname } :: tl ->
    let client_connection_address = show_client_connection_address ~global_state in
    let connection_request_message = Printf.sprintf "\"%s\" has connected to %s" client_nickname client_connection_address in
    let pretty_connection_request_message = pretty_info_message_string connection_request_message in
    let () = print_endline pretty_connection_request_message in
    let server_nickname = show_server_nickname ~global_state in
    let serverAck = ServerConnection { server_nickname = server_nickname } in
    let () = write_message writer_pipe serverAck in
    handle_socket_message tl ~global_state ~sender_type writer_pipe
  | ServerConnection { server_nickname } :: tl ->
    let server_connection_address = show_server_connection_address ~global_state in
    let server_connection_message = Printf.sprintf "\"%s\" has connected to %s" server_nickname server_connection_address in
    let pretty_server_connection_message = pretty_info_message_string server_connection_message in
    let () = print_endline pretty_server_connection_message in
    handle_socket_message tl ~global_state ~sender_type writer_pipe
  | Fail { error_message } :: tl ->
    let () = print_endline (pretty_error_message_string error_message) in
    handle_socket_message tl ~global_state ~sender_type writer_pipe

let handle_socket_payload stdin_payload ~global_state ~sender_type writer_pipe : bool Deferred.t =
  match stdin_payload with
  | InputEof ->
    return false
  | InputOk message -> 
    try_with (fun () ->
      let messages = parse_messages message in
      handle_socket_message messages ~global_state ~sender_type writer_pipe >>| fun _ ->
      true 
    ) >>= function
    | Ok success -> return success
    | Error exn ->
      let error_message = pretty_error_message_string (Exn.to_string exn) in
      let () = print_endline error_message in
      return false

let rec handle_connection ~global_state ~sender_type ~socket_reader_pipe ~socket_writer_pipe ~stdin_reader_pipe : unit Deferred.t =
  Deferred.choose [
    Deferred.Choice.map (Pipe.read_choice_single_consumer_exn socket_reader_pipe [%here]) 
      ~f:(function 
          | `Eof -> Socket InputEof 
          | `Ok message -> Socket (InputOk message));
    Deferred.Choice.map (Pipe.read_choice_single_consumer_exn stdin_reader_pipe [%here]) 
      ~f:(function 
          | `Eof -> Stdin InputEof 
          | `Ok message -> Stdin (InputOk message))
  ]
  >>= function
  | Socket InputEof | Stdin InputEof -> 
    let my_nickname = show_nickname ~global_state ~sender_type in 
    let get_recipient = show_recipient_type ~sender_type in
    let recipient_connection_address = show_connection_address ~global_state ~sender_type:get_recipient in
    let disconnected_message = Printf.sprintf "\"%s\" has disconnected from %s" my_nickname recipient_connection_address in
    let pretty_disconnected_message = pretty_info_message_string disconnected_message in
    let () = print_endline pretty_disconnected_message in
    return ()
  | Socket payload ->
    let ping_pong = handle_socket_payload payload ~global_state ~sender_type socket_writer_pipe in
    Deferred.bind ping_pong ~f:(fun result ->
      if result then
        handle_connection ~global_state ~sender_type ~socket_reader_pipe ~socket_writer_pipe ~stdin_reader_pipe
      else
        return ()
    )
  | Stdin payload ->
      handle_stdin_payload payload ~sender_type socket_writer_pipe
  >>= fun () ->
  handle_connection ~global_state ~sender_type ~socket_reader_pipe ~socket_writer_pipe ~stdin_reader_pipe
