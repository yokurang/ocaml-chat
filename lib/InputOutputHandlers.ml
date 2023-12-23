open Core
open Async
open DataTypes
open Utils

let write_message (w : string Pipe.Writer.t) (given_message : message) : unit Deferred.t=
  match given_message with
  | Fail {error_message} ->
    let err_message = Printf.sprintf "Failed to send message: %s" error_message in
    let pretty_error_message = pretty_error_message_string err_message in
    let () = print_endline pretty_error_message in return ()
  | _ ->
    if not (Pipe.is_closed w)
    then
      sexp_of_message given_message |> Sexp.to_string |> Pipe.write w 
    else 
      let error_message = "Writer pipe is closed" in
      let pretty_error_message = pretty_error_message_string error_message in
      let () = print_endline pretty_error_message in return ()

let handle_stdin_payload payload writer_pipe ~message_created_at_timestamp_queue : unit Deferred.t =
  try_with (fun () ->
    match payload with
    | InputEof -> 
      return ()
    | InputOk message ->
      let time_ns_now = Time_ns_unix.to_string (Time_ns_unix.now ()) in
      let new_message = create_message
        ~content:(Some message)
        ~timestamp:time_ns_now 
      in
      let%bind () = write_message writer_pipe new_message in
      let () = Queue.enqueue message_created_at_timestamp_queue time_ns_now in
      return ()
  )
  >>= function
  | Ok () ->
    return ()
  | Error exn ->
      let error_message = pretty_error_message_string (Exn.to_string exn) in
      print_endline error_message;
      return ()

let parse_string_to_message_sexp (message : string) : message =
  try 
    let sexp = Sexp.of_string message in
    message_of_sexp sexp
  with 
  | exn -> 
    let error_message = pretty_error_message_string (Exn.to_string exn) in
    Fail { error_message = error_message }

let handle_socket_message message ~connection_address writer_pipe ~message_created_at_timestamp_queue : bool Deferred.t =
  let time_ns_now = Time_ns_unix.to_string (Time_ns_unix.now ()) in
  match message with
  | Acknowledgement { message_timestamp } ->
    let ack = Acknowledgement {
      message_timestamp;
    } in let connection_address = connection_address
    in let message_created_at_timestamp_option = Queue.dequeue message_created_at_timestamp_queue in
    let message_created_at_timestamp = Option.value_exn message_created_at_timestamp_option in
    if (String.equal message_created_at_timestamp message_timestamp)
    then
      let () = print_acknowledgement connection_address ack time_ns_now in
      return true
    else
      let error_message = "Acknowledgement timestamp does not match" in
      let pretty_error_message = pretty_error_message_string error_message in
      let () = print_endline pretty_error_message in return false
    | Message { message_content; timestamp } ->
    let msg = Message {
      message_content;
      timestamp;
    } in let connection_address = connection_address in
      let () = print_chat_message ~connection_address msg in
      let ack_message = create_acknowledgement_from_message msg in
      let%bind () = write_message writer_pipe ack_message in
      return true
  | Fail { error_message } ->
    let () = print_endline (pretty_error_message_string error_message) in return false

let handle_socket_payload stdin_payload ~connection_address writer_pipe ~message_created_at_timestamp_queue : bool Deferred.t =
  match stdin_payload with
  | InputEof ->
    return false
  | InputOk message -> 
    try_with (fun () ->
      let message = parse_string_to_message_sexp message in
      handle_socket_message message ~connection_address writer_pipe ~message_created_at_timestamp_queue
    ) >>= function
    | Ok result -> return result
    | Error exn ->
      let error_message = pretty_error_message_string (Exn.to_string exn) in
      let () = print_endline error_message in
      return false

let rec handle_connection ~socket_reader_pipe ~socket_writer_pipe ~stdin_reader_pipe ~connection_address ~message_created_at_timestamp_queue : unit Deferred.t =
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
    let connection_address = connection_address in
    let disconnected_message = sprintf "%s disconnected" connection_address in
    let pretty_disconnected_message = pretty_info_message_string disconnected_message in
    let () = print_endline pretty_disconnected_message in
    return ()
  | Socket payload ->
    let ping_pong = handle_socket_payload payload ~connection_address socket_writer_pipe ~message_created_at_timestamp_queue in
    Deferred.bind ping_pong ~f:(fun result ->
      if result then
        handle_connection ~socket_reader_pipe ~socket_writer_pipe ~stdin_reader_pipe ~connection_address ~message_created_at_timestamp_queue
      else
        return ()
    )
  | Stdin payload ->
      handle_stdin_payload payload socket_writer_pipe ~message_created_at_timestamp_queue
  >>= fun () ->
  handle_connection ~socket_reader_pipe ~socket_writer_pipe ~stdin_reader_pipe ~connection_address ~message_created_at_timestamp_queue
