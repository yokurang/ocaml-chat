open Core
open DataTypes

let pretty_log_message_string (log_level : string) (message : string) : string =
  let timestamp = Time_ns_unix.to_string (Time_ns.now ()) in
  Printf.sprintf "[%s] [%s] - %s\n" log_level timestamp message

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
let is_message_id_equal_acknowledgement_id ~global_state : bool =
  if !(global_state.uniqueMessageNumber) = !(global_state.uniqueAcknowledgementNumber)
  then
    let log_message = Printf.sprintf "Message ID %d is equal to acknowledgement ID %d" !(global_state.uniqueMessageNumber) !(global_state.uniqueAcknowledgementNumber) in
    let () = print_endline (pretty_info_message_string log_message) in
    true
  else
    let log_message = Printf.sprintf "Message ID %d is not equal to acknowledgement ID %d" !(global_state.uniqueMessageNumber) !(global_state.uniqueAcknowledgementNumber) in
    let () = print_endline (pretty_info_message_string log_message) in
    false

let update_unique_message_number ~global_state (given_message : message) : unit =
  match given_message with
  | Message {message_id; _} ->
    if message_id = succ !(global_state.uniqueMessageNumber) then
      let () = incr (global_state.uniqueMessageNumber) in
      let log_message = Printf.sprintf "Message ID has been updated to %d" !(global_state.uniqueMessageNumber) in
      let () = print_endline (pretty_info_message_string log_message) in
      ()
    else begin
      let error_message = pretty_error_message_string "The Message ID is out of order somehow. Updating global state to the latest Message ID." in
      let () = print_endline error_message in
      let () = (global_state.uniqueMessageNumber) := max message_id !(global_state.uniqueMessageNumber) in
      ()
    end
  | _ ->
    let error_message = pretty_error_message_string "Cannot update unique message number from non-message" in
    failwith error_message  (* No action for other message types *)

let update_unique_acknowledgement_number ~global_state (given_message : message) : unit =
  match given_message with
  | Acknowledgement {acknowledgement_id; _} ->
    if acknowledgement_id = succ !(global_state.uniqueAcknowledgementNumber) then
      let () = incr global_state.uniqueAcknowledgementNumber in
      let log_message = Printf.sprintf "Acknowledgement ID has been updated to %d" !(global_state.uniqueAcknowledgementNumber) in
      let () = print_endline (pretty_info_message_string log_message) in
      ()
    else begin
      let error_message = pretty_error_message_string "The Acknowledgement ID is out of order somehow. Updating global state to the latest Acknowledgement ID." in
      let () = print_endline error_message in
      let () = global_state.uniqueAcknowledgementNumber := max acknowledgement_id !(global_state.uniqueAcknowledgementNumber) in
      ()
    end
  | _ -> 
    let error_message = pretty_error_message_string "Cannot update unique acknowledgement number from non-acknowledgement" in
    failwith error_message  (* No action for other message types *)

let create_message ~content ~message_id ~message_from ~message_to ~timestamp ~global_state : message =
  let () = print_endline (pretty_debug_message_string "Message created") in
  let message = Message {
    message_content = content;
    message_id = message_id;
    message_from = message_from;
    message_to = message_to;
    timestamp = timestamp;
  } in message |> update_unique_message_number ~global_state;
  message
  
let create_acknowledgement_from_message ~acknowledgement_id ~global_state (given_message : message) : message =
  match given_message with
  | Message {message_id; message_from; message_to; timestamp; _} ->
    let acknowledgement = Acknowledgement {
      acknowledgement_id = acknowledgement_id;
      message_id = message_id;
      message_from = message_from;
      message_to = message_to;
      message_timestamp = timestamp;
    } in acknowledgement |> update_unique_acknowledgement_number ~global_state;
    let () = print_endline (pretty_debug_message_string "Acknowledgement created") in
    acknowledgement
  | _ ->
    let error_message = pretty_error_message_string "Cannot create acknowledgement from non-message" in
    failwith error_message  (* No action for other message types *)  

let show_sender_type ~participant_type =
  match participant_type with
  | Server -> Server
  | Client -> Client

let show_recipient_type ~participant_type =
  match participant_type with
  | Server -> Client
  | Client -> Server

let show_connection_address ~global_state =
  match !(global_state.connection_address) with
  | None -> "No one"
  | Some address -> address

let show_server_nickname ~global_state =
  match !(global_state.server_nickname) with
  | None -> "THE DEATH STAR"
  | Some nickname -> nickname

let show_client_nickname ~global_state =
  match !(global_state.client_nickname) with
  | None -> "THE MILLENIUM FALCON"
  | Some nickname -> nickname

let show_message_sender ~global_state (given_message : message) : string =
  match given_message with
  | Message {message_from; _} ->
    begin
      match message_from with
      | Server -> show_server_nickname ~global_state
      | Client -> show_client_nickname ~global_state
    end
  | _ -> 
    let error_message = pretty_error_message_string "Cannot show message sender from non-message" in
    failwith error_message  (* No action for other message types *)

let show_message_receiver ~global_state (given_message : message) : string =
  match given_message with
  | Message {message_to; _} ->
    begin
      match message_to with
      | Server -> show_server_nickname ~global_state
      | Client -> show_client_nickname ~global_state
    end
  | _ -> 
    let error_message = pretty_error_message_string "Cannot show message receiver from non-message" in
    failwith error_message  (* No action for other message types *)

(* ~global_state *)
let print_message_received (_addr : string) message_id =
    (* let message_sender = show_message_sender ~global_state given_message in *)
    (* let message_receiver = show_message_receiver ~global_state given_message in *)
    Printf.printf "[%s:Message Received] - Message ID: %d\n" _addr message_id
  
(* ~global_state *)
let print_acknowledgement_received (_addr : string) (given_message: message) (acknowledged_timestamp : string)=
  match given_message with
  | Acknowledgement {acknowledgement_id; message_id; message_timestamp; _} ->
    let pretty_date = pretty_date_from_timestamp_str message_timestamp in
    let message_sender = "You" in
    let message_receiver = "Me" in
    (* let message_sender = show_message_sender ~global_state given_message in *)
    (* let message_receiver = show_message_receiver ~global_state given_message in *)
    let rtt = calculate_rtt message_timestamp acknowledged_timestamp in
    Printf.printf "[%s:Acknowledgement Received] [%s] [RTT:%s seconds] - Ack ID: %d, Message ID: %d, From: %s, To: %s\n" 
      _addr pretty_date rtt acknowledgement_id message_id message_sender message_receiver
  | _ -> 
    begin
      let error_message = pretty_error_message_string "Cannot pretty print non-acknowledgement" in
      Printf.printf "%s" error_message
    end

let print_chat_message (_addr : string) (given_message : message) =
  match given_message with
  (* message_from; message_to;  *)
  | Message { message_content = Some content; message_id; timestamp; _ } ->
    let pretty_date = pretty_date_from_timestamp_str timestamp in
    let message_sender = "You" in
    let message_receiver = "Me" in
    Printf.printf "[%s: Chat Message] [%s] - Message ID: %d, From: %s, To: %s, Content: \"%s\"\n" 
      _addr pretty_date message_id message_sender message_receiver content
  | Message {message_content = None; _} ->
    let error_message = "Message content is empty" in
    Printf.printf "[%s: Error] %s\n" _addr error_message
  | _ -> 
    let error_message = "Cannot pretty print non-chat message" in
    Printf.printf "[%s: Error] %s\n" _addr error_message

let print_connected_message (nick : string) ~global_state : unit =
  let connection_address_str = show_connection_address ~global_state in
  (let message = Printf.sprintf "Client \"%s\" has connected from %s." nick connection_address_str in
  let connected_message = pretty_info_message_string message in
  Printf.printf "%s" connected_message;)

let print_disconnected_message (nick : string) ~global_state : unit =
  let connection_address_str = show_connection_address ~global_state in
  (let message = Printf.sprintf "\"%s\" has disconnected from %s." nick connection_address_str in
  let disconnected_message = pretty_info_message_string message in
  Printf.printf "%s" disconnected_message;)

