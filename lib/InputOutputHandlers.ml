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

let handle_stdin_payload payload ~global_state ~participant_type writer_pipe : unit Deferred.t =
  try_with (fun () ->
    match payload with
    | InputEof -> 
      return ()
    | InputOk message ->
      let sender = show_sender_type ~participant_type in
      let receiver = show_recipient_type ~participant_type in
      let time_ns_now = Time_ns_unix.to_string (Time_ns_unix.now ()) in
      let message_num = succ !(global_state.uniqueMessageNumber) in
      let new_message = create_message
        ~content:(Some message)
        ~message_id:message_num
        ~message_from:sender
        ~message_to:receiver
        ~timestamp:time_ns_now 
        ~global_state
      in
      let () = write_message writer_pipe new_message in
      return ()
  )
  >>= function
  | Ok () -> return ()
    (* handle_stdin_payload payload ~global_state ~participant_type writer_pipe *)
  | Error exn ->
      let error_message = pretty_error_message_string (Exn.to_string exn) in
      print_endline error_message;
      return ()

let handle_socket_payload stdin_payload ~global_state writer_pipe : unit Deferred.t =
  let time_ns_now = Time_ns_unix.to_string (Time_ns_unix.now ()) in
  match stdin_payload with
  | InputEof -> 
    return ()
  | InputOk message -> 
    try_with (fun () ->
      let parsed_message = try Sexp.of_string message |> message_of_sexp with
        | _ ->
          let exn_message = "Failed to parse message" in
          Fail {error_message=exn_message} in
      match parsed_message with
      | Acknowledgement { acknowledgement_id; message_id; message_from; message_to; message_timestamp } ->
        let ack = Acknowledgement {
          acknowledgement_id;
          message_id;
          message_from;
          message_to;
          message_timestamp;
        } in
        let connection_address = show_connection_address ~global_state in
        let () = print_message_received connection_address message_id in
        let () = print_acknowledgement_received connection_address ack time_ns_now in
        (* ~global_state *)
        return ()
      | Message { message_content; message_id; message_from; message_to; timestamp } ->
        let msg = Message {
          message_content;
          message_id;
          message_from;
          message_to;
          timestamp;
        } in
        let connection_address = show_connection_address ~global_state in
        let () = print_chat_message connection_address msg in
        (* ~global_state *)
        let ack_id = succ !(global_state.uniqueAcknowledgementNumber) in
        let ack_message = create_acknowledgement_from_message
        ~acknowledgement_id:ack_id
        ~global_state
        msg in
        let () = write_message writer_pipe ack_message in
        return ()
      | Fail fail ->
        let () = print_endline (pretty_error_message_string fail.error_message) in
        return ()
    ) >>= function
    | Ok () -> return ()
      (* handle_socket_payload stdin_payload ~global_state writer_pipe *)
    | Error exn ->
        let error_message = pretty_error_message_string (Exn.to_string exn) in
        print_endline error_message;
        return ()

let rec handle_connection ~global_state ~participant_type ~socket_reader_pipe ~socket_writer_pipe ~stdin_reader_pipe : unit Deferred.t =
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
    begin match participant_type with
    | Client ->
      let server_nickname = show_server_nickname ~global_state in
    let () = print_disconnected_message server_nickname ~global_state
      in return ()
    | Server ->
      let client_nickname = show_client_nickname ~global_state in
      let () = print_disconnected_message client_nickname ~global_state
      in return () end
  | Socket payload ->
      handle_socket_payload payload ~global_state socket_writer_pipe
  | Stdin payload ->
      handle_stdin_payload payload ~global_state ~participant_type socket_writer_pipe
  >>= fun () ->
  handle_connection ~global_state ~participant_type ~socket_reader_pipe ~socket_writer_pipe ~stdin_reader_pipe
