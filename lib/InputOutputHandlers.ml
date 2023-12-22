open Core
open Async
(* open Sexplib.Std *)
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
      let () = print_endline pretty_error_message in
      Pipe.close w

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

let rec handle_socket_message messages ~global_state writer_pipe : bool Deferred.t =
  let time_ns_now = Time_ns_unix.to_string (Time_ns_unix.now ()) in
  match messages with
  | [] -> return true
  | Acknowledgement { message_content; message_from; message_to; message_timestamp } :: tl ->
    let ack = Acknowledgement {
      message_content;
      message_from;
      message_to;
      message_timestamp;
    } in
    let client_connection_address = show_client_connection_address ~global_state in
    let () = print_acknowledgement client_connection_address ack time_ns_now in
    let () = print_acknowledgement_log ~global_state client_connection_address ack time_ns_now in
    handle_socket_message tl ~global_state writer_pipe
  | Message { message_content; message_from; message_to; timestamp } :: tl ->
    let msg = Message {
      message_content;
      message_from;
      message_to;
      timestamp;
    } in
    let client_connection_address = show_client_connection_address ~global_state in
    let () = print_chat_message ~global_state client_connection_address msg in
    let ack_message = create_acknowledgement_from_message msg in
    let () = write_message writer_pipe ack_message in
    handle_socket_message tl ~global_state writer_pipe
  | ConnectionRequest { user_nickname } :: tl ->
    let client_connection_address = show_client_connection_address ~global_state in
    let info_str = Printf.sprintf "Client %s has connected from address %s" user_nickname client_connection_address in
    let pretty_info_str = pretty_info_message_string info_str in
    let () = print_endline pretty_info_str in
    let server_nickname = show_server_nickname ~global_state in
    let connection_accepted_message = ConnectionAccepted { user_nickname = server_nickname } in
    let () = write_message writer_pipe connection_accepted_message in
    handle_socket_message tl ~global_state writer_pipe
  | ConnectionAccepted { user_nickname } :: tl ->
    let client_connection_address = show_server_connection_address ~global_state in
    let info_str = Printf.sprintf "Server %s has accepted your connection request from address %s" user_nickname client_connection_address in
    let pretty_info_str = pretty_info_message_string info_str in
    let () = print_endline pretty_info_str in
    handle_socket_message tl ~global_state writer_pipe
  | Fail { error_message } :: tl ->
    let () = print_endline (pretty_error_message_string error_message) in
    handle_socket_message tl ~global_state writer_pipe
      
let parse_messages message =
  try 
    let sexps = Sexp.of_string_many message in
    List.map sexps ~f:message_of_sexp
  with
  | exn -> 
    let error_message = pretty_error_message_string (Exn.to_string exn) in
    let error_payload = Fail { error_message = error_message } in
    [error_payload]

let handle_socket_payload stdin_payload ~global_state writer_pipe : bool Deferred.t =
  match stdin_payload with
  | InputEof -> begin
    let () = print_client_disconnection_message ~global_state in
    return false
  end
  | InputOk message -> 
    try_with (fun () ->
      let messages = parse_messages message in
      handle_socket_message messages ~global_state writer_pipe >>| fun _ ->
      true 
    ) >>= function
    | Ok success -> return success
    | Error exn ->
      let error_message = pretty_error_message_string (Exn.to_string exn) in
      print_endline error_message;
      let () = print_client_disconnection_message ~global_state in
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
    begin match sender_type with
    | Client ->
      let server_nickname = show_server_nickname ~global_state in
    let () = print_disconnected_message server_nickname ~global_state
      in return ()
    | Server ->
      let client_nickname = show_client_nickname ~global_state in
      let () = print_disconnected_message client_nickname ~global_state
      in return () end
  | Socket payload ->
    let bounce_back = handle_socket_payload payload ~global_state socket_writer_pipe in
    Deferred.bind bounce_back ~f:(fun result ->
      if result then
        handle_connection ~global_state ~sender_type ~socket_reader_pipe ~socket_writer_pipe ~stdin_reader_pipe
      else
        return ()
    )
  | Stdin payload ->
      handle_stdin_payload payload ~sender_type socket_writer_pipe
  >>= fun () ->
  handle_connection ~global_state ~sender_type ~socket_reader_pipe ~socket_writer_pipe ~stdin_reader_pipe
