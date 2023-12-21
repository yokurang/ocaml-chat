open Core
open DataTypes

let pretty_log_message_string (log_level : string) (message : string) : string =
  let timestamp = Time_ns_unix.to_string (Time_ns.now ()) in
  sprintf "[%s] [%s] - %s\n" log_level timestamp message

let pretty_info_message_string (message : string) : string =
  pretty_log_message_string "INFO" message

let pretty_error_message_string (error_message : string) : string =
  pretty_log_message_string "ERROR" error_message

let pretty_debug_message_string (message : string) : string =
  pretty_log_message_string "DEBUG" message

let pretty_date_from_timestamp_str (timestamp_str : string) : string =
  try
    let timestamp_ns = Time_float_unix.of_string timestamp_str in
    Time_float_unix.format timestamp_ns "%d/%m/%Y %H:%M:%S" ~zone:(Lazy.force Time_float_unix.Zone.local)
  with
  | Invalid_argument _ ->
    failwith "Invalid timestamp format"

let parse_timestamp_str_to_ns (ts_str : string) : Time_ns.t =
  try 
    Time_ns_unix.of_string ts_str
  with 
  | Invalid_argument _ -> 
    failwith "Invalid timestamp format"

let calculate_rtt (sent_timestamp_str : string) (received_timestamp_str : string) : string =
  let sent_timestamp = parse_timestamp_str_to_ns sent_timestamp_str in
  let received_timestamp = parse_timestamp_str_to_ns received_timestamp_str in
  let duration = Time_ns_unix.diff received_timestamp sent_timestamp in
  (* rtt should be in miliseconds according to my Google search *)
  Int63.to_float (Time_ns_unix.Span.to_int63_ns duration) /. 1e6
  |> Float.to_string

(* the invariant in our chat application *)
let is_message_id_equal_acknowledgement_id ~uniqueMessageNumber ~uniqueAcknowledgementNumber : bool =
  if !uniqueMessageNumber = !uniqueAcknowledgementNumber
  then
    let log_message = sprintf "Message ID %d is equal to acknowledgement ID %d" !uniqueMessageNumber !uniqueAcknowledgementNumber in
    let () = print_endline (pretty_info_message_string log_message) in
    true
  else
    let log_message = sprintf "Message ID %d is not equal to acknowledgement ID %d" !uniqueMessageNumber !uniqueAcknowledgementNumber in
    let () = print_endline (pretty_info_message_string log_message) in
    false

let update_unique_message_number ~uniqueMessageNumber (given_message : message) : unit =
  match given_message with
  | Message {message_id; _} ->
    if message_id = succ !uniqueMessageNumber then
      let () = incr uniqueMessageNumber in
      let log_message = sprintf "Message ID has been updated to %d" !uniqueMessageNumber in
      let () = print_endline (pretty_info_message_string log_message) in
      ()
    else begin
      let error_message = pretty_error_message_string "The Message ID is out of order somehow. Updating global state to the latest Message ID." in
      let () = print_endline error_message in
      let () = uniqueMessageNumber := max message_id !uniqueMessageNumber in
      ()
    end
  | _ ->
    let error_message = pretty_error_message_string "Cannot update unique message number from non-message" in
    failwith error_message  (* No action for other message types *)

let update_unique_acknowledgement_number ~uniqueAcknowledgementNumber (given_message : message) : unit =
  match given_message with
  | Acknowledgement {acknowledgement_id; _} ->
    if acknowledgement_id = succ !uniqueAcknowledgementNumber then
      let () = incr uniqueAcknowledgementNumber in
      let log_message = sprintf "Acknowledgement ID has been updated to %d" !uniqueAcknowledgementNumber in
      let () = print_endline (pretty_info_message_string log_message) in
      ()
    else begin
      let error_message = pretty_error_message_string "The Acknowledgement ID is out of order somehow. Updating global state to the latest Acknowledgement ID." in
      let () = print_endline error_message in
      let () = uniqueAcknowledgementNumber := max acknowledgement_id !uniqueAcknowledgementNumber in
      ()
    end
  | _ -> 
    let error_message = pretty_error_message_string "Cannot update unique acknowledgement number from non-acknowledgement" in
    failwith error_message  (* No action for other message types *)

let create_message ~content ~message_id ~message_from ~message_to ~timestamp ~uniqueMessageNumber : message =
  let message = Message {
    message_content = content;
    message_id = message_id;
    message_from = message_from;
    message_to = message_to;
    timestamp = timestamp;
  } in message |> update_unique_message_number ~uniqueMessageNumber;
  let () = print_endline (pretty_debug_message_string "Message created") in
  message
  
let create_acknowledgement_from_message ~acknowledgement_id ~uniqueAcknowledgementNumber (given_message : message) : message =
  match given_message with
  | Message {message_id; message_from; message_to; timestamp; _} ->
    let acknowledgement = Acknowledgement {
      acknowledgement_id = acknowledgement_id;
      message_id = message_id;
      message_from = message_from;
      message_to = message_to;
      message_timestamp = timestamp;
    } in acknowledgement |> update_unique_acknowledgement_number ~uniqueAcknowledgementNumber;
    let () = print_endline (pretty_debug_message_string "Acknowledgement created") in
    acknowledgement
  | _ ->
    let error_message = pretty_error_message_string "Cannot create acknowledgement from non-message" in
    failwith error_message  (* No action for other message types *)  

let print_startup_message (nick : string) : unit =
  (let message = Printf.sprintf "Server \"%s\" has started successfully." nick in
  let startup_message = pretty_info_message_string message in
  printf "%s" startup_message;)

let print_connected_message (nick : string) : unit =
  (let message = Printf.sprintf "Client \"%s\" has connected." nick in
  let connected_message = pretty_info_message_string message in
  printf "%s" connected_message;)

let print_disconnected_message (nick : string) : unit =
  (let message = Printf.sprintf "\"%s\" has disconnected." nick in
  let disconnected_message = pretty_info_message_string message in
  printf "%s" disconnected_message;)

