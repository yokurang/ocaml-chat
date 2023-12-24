open Core
open Async
open DataTypes
open Utils

(** A function to write a message to a writer pipe *)
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

(** A function to handle user stdin *)
let handle_stdin_payload payload writer_pipe : unit Deferred.t =
  try_with (fun () ->
    match payload with
    | InputEof -> 
      return ()
    | InputOk message ->
      let time_ns_now = Time_ns_unix.to_string (Time_ns_unix.now ()) in
      write_message writer_pipe (Message {
        message_content = Some message;
        timestamp = time_ns_now;
      })
  )
  >>= function
  | Ok () ->
    return ()
  | Error exn ->
      let error_message = pretty_error_message_string (Exn.to_string exn) in
      print_endline error_message;
      return ()

(** A helper function to parse a message as a string to a
    message via S-expression
    
    This implementation is prone to bugs. See the function below for an
    explanation of the bug and my solution to it. *)
let parse_string_to_message_sexp (message : string) : message =
  try 
    let sexp = Sexp.of_string message in
    message_of_sexp sexp
  with 
  | exn -> 
    let error_message = pretty_error_message_string
    (sprintf "Failed to parse message: %s with error: %s"
    message (Exn.to_string exn)) in
    Fail { error_message = error_message }

(** A helper function to parse a message as a string to a
    list of messages via S-expressions
    
    The reason we need this function is because using the function above, we
    may occasionally get the following error:

    [ERROR] [24/12/2023 10:11:29] - Failed to parse message: (Message(message_content("asd\n"))(timestamp"2023-12-24 10:11:29.630252000+07:00"))(Message(message_content("asd\n"))(timestamp"2023-12-24 10:11:29.630270000+07:00")) with error: (Failure
    "Sexplib.Sexp.of_string: got multiple S-expressions where only one was expected.")
    
    This error message occurs because more than one S-expression was received when
    only one was expected. After trying different methods, I found that the
    easiest way to solve this problem is to parse the string into a list of
    S-expressions and then map each S-expression to a message. This way, we can
    correctly parse the message even if we receive multiple S-expressions. *)
let parse_string_to_message_sexp_list (message : string) : message list =
  try 
    let sexp = Sexp.of_string_many message in
    List.map sexp ~f:(fun sexp -> message_of_sexp sexp)
  with 
  | exn -> 
    let error_message = pretty_error_message_string (Exn.to_string exn) in
    [Fail { error_message }]

(** A function to handle the application logic depending on the given message
  1. If a message of type Message is obtained, print out the chat message in a
  pretty format and write an acknowledgement message over the writer pipe
  for that message
  2. If a message of type acknowledgement is obtained, compute the RTT and
  pretty print the acknowledgement message
  3. If a message of type Fail is obtained, print out the error message
*)
let handle_socket_message message ~connection_address writer_pipe : unit Deferred.t =
  match message with
  | Message { message_content; timestamp } ->
    let connection_address = connection_address in
    let pretty_timestamp = pretty_date_from_timestamp_str timestamp in
    let () = print_endline
    (sprintf "[Chat Message] [%s]: %s says %s"
    pretty_timestamp connection_address (Option.value_exn message_content)) in
    write_message writer_pipe (Acknowledgement {
      message_timestamp = timestamp;
    })
  | Acknowledgement { message_timestamp } ->
    let connection_address = connection_address in
    let rtt = Time_ns_unix.diff (Time_ns_unix.now ()) (Time_ns_unix.of_string message_timestamp) in
    let () = print_endline
    (sprintf "[Acknowledgement from %s] - RTT: %s ms, Status: Message Received\n"
    connection_address (Time_ns.Span.to_ms rtt |> Float.to_string)) in
    return ()
  | Fail { error_message } ->
    let () = print_endline error_message in return ()

(** A function to handle the application logic depending on the given message.
  This function is the same as handle_socket_message, but the input is a list
  of messages.
  1. If the head of the list of messages is a message of type Message, print out
  the chat message in a pretty format and write an acknowledgement message over
  the writer pipe for that message
  2. If the head of the list of messages is a message of type Acknowledgement,
  compute the RTT and pretty print the acknowledgement message with the RTT.
  3. If the head of the list of messages is a message of type Fail, print out
  the error message.
  4. If the list of messages is empty, then there are no messages to handle and
  we return unit. *)
let handle_socket_message_list messages ~connection_address writer_pipe : unit Deferred.t =
  (** We only provide the list of messages into the recursive auxillary
  function because only the list of messages is structurally recursive. *)
  let rec aux messages =
    match messages with
    | [] -> return ()
    | Message { message_content; timestamp } :: tl ->
      let connection_address = connection_address in
      let pretty_timestamp = pretty_date_from_timestamp_str timestamp in
      let () = print_endline
      (sprintf "[Chat Message] [%s]: %s says %s"
      pretty_timestamp connection_address (Option.value_exn message_content)) in
      let%bind () = write_message writer_pipe (Acknowledgement {
        message_timestamp = timestamp;
      }) in aux tl;
    | Acknowledgement { message_timestamp } :: tl ->
      let connection_address = connection_address in
      let rtt = Time_ns_unix.diff (Time_ns_unix.now ()) (Time_ns_unix.of_string message_timestamp) in
      let () = print_endline
      (sprintf "[Acknowledgement from %s] - RTT: %s ms, Status: Message Received\n"
      connection_address (Time_ns.Span.to_ms rtt |> Float.to_string)) in
      aux tl
    | Fail { error_message } :: tl ->
      let () = print_endline error_message in
      let%bind () = aux tl in
      return ()   
  in aux messages 

(** A function to handle socket input that has been read from the reader pipe *)
let handle_socket_payload stdin_payload ~connection_address writer_pipe : bool Deferred.t =
  match stdin_payload with
  | InputEof ->
    return false
  | InputOk message -> 
    try_with (fun () ->
      let message = parse_string_to_message_sexp message in
      handle_socket_message message ~connection_address writer_pipe
    ) >>= function
    | Ok () -> return true
    | Error exn ->
      let error_message = pretty_error_message_string (Exn.to_string exn) in
      let () = print_endline error_message in
      return false

(** A function to handle socket input that has been read from the reader pipe,
    which is the same as handle_socket_payload, but uses
    handle_socket_message_list instead of handle_socket_message. *)
let handle_socket_payload_list stdin_payload ~connection_address writer_pipe : bool Deferred.t =
  match stdin_payload with
  | InputEof ->
    return false
  | InputOk message -> 
    try_with (fun () ->
      let messages = parse_string_to_message_sexp_list message in
      handle_socket_message_list messages ~connection_address writer_pipe
    ) >>= function
    | Ok () -> return true
    | Error exn ->
      let error_message = pretty_error_message_string (Exn.to_string exn) in
      let () = print_endline error_message in
      return false

(** A single function that can handle between reading and handling from stdin
    reading and handling from socket input simultaneously. This implements the
    message sending logic. *)
let rec handle_connection ~socket_reader_pipe ~socket_writer_pipe ~stdin_reader_pipe ~connection_address : unit Deferred.t =
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
  | Stdin InputEof | Socket InputEof ->
    let connection_address = connection_address in
    let disconnected_message = sprintf "%s disconnected" connection_address in
    let pretty_disconnected_message = pretty_info_message_string disconnected_message in
    let () = print_endline pretty_disconnected_message in
    return ()
  | Socket payload -> (* after processing the socket payload,
      wait for another stdin. We bounce back between processing the payload and
      asking for a stdin like a ping pong *)
    (* let ping_pong = handle_socket_payload payload ~connection_address socket_writer_pipe in *)
    let ping_pong = handle_socket_payload_list payload ~connection_address socket_writer_pipe in
    Deferred.bind ping_pong ~f:(fun result ->
      if result then
        handle_connection ~socket_reader_pipe ~socket_writer_pipe ~stdin_reader_pipe ~connection_address
      else
        return ()
    )
  | Stdin payload ->
      handle_stdin_payload payload socket_writer_pipe
  >>= fun () ->
  handle_connection ~socket_reader_pipe ~socket_writer_pipe ~stdin_reader_pipe ~connection_address
