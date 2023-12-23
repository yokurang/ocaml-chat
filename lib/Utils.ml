open Core
open DataTypes

(** A function to parse a timestamp string from Time_float_unix
  into a pretty date string for logging *)
let pretty_date_from_timestamp_str (timestamp_str : string) : string =
  try
    let timestamp_ns = Time_float_unix.of_string timestamp_str in
    Time_float_unix.format timestamp_ns "%d/%m/%Y %H:%M:%S" ~zone:(Lazy.force Time_float_unix.Zone.local)
  with
  | Invalid_argument _ ->
    failwith "Invalid timestamp format"

(** A general function for pretty printing log messages.
    You can customize the log level and the message to be printed. *)
let pretty_log_message_string (log_level : string) (message : string) : string =
  let timestamp = Time_ns_unix.to_string (Time_ns.now ()) in
  let pretty_date = pretty_date_from_timestamp_str timestamp in
  sprintf "\n[%s] [%s] - %s\n" log_level pretty_date message

(** A function for pretty printing info messages.
    You can customize the message to be printed. *)
let pretty_info_message_string (message : string) : string =
  pretty_log_message_string "INFO" message

(** A function for pretty printing error messages.
    You can customize the message to be printed. *)
let pretty_error_message_string (error_message : string) : string =
  pretty_log_message_string "ERROR" error_message

(** A function for pretty printing debug messages.
    You can customize the message to be printed.
    Useful to see what's going on in the background during development. *)
let pretty_debug_message_string (message : string) : string =
  pretty_log_message_string "DEBUG" message

(**  A function for parsing a timestamp string into a Time_ns.t type *)
let parse_timestamp_str_to_ns (ts_str : string) : Time_ns.t =
  try 
    Time_ns_unix.of_string ts_str
  with 
  | Invalid_argument _ -> 
    failwith "Invalid timestamp format"

(** A function for calculating the RTT between two timestamps *)
let calculate_rtt (sent_timestamp_str : string) (received_timestamp_str : string) : string =
  let sent_timestamp = parse_timestamp_str_to_ns sent_timestamp_str in
  let received_timestamp = parse_timestamp_str_to_ns received_timestamp_str in
  let duration = Time_ns_unix.diff received_timestamp sent_timestamp in
  (* rtt should be in miliseconds according to my Google search *)
  Int63.to_float (Time_ns_unix.Span.to_int63_ns duration) /. 1e6
  |> Float.to_string

(** A function for creating a new message *)
let create_message
  ~(content : string option)
  ~(timestamp : string) : message =
  (* let () = print_endline (pretty_debug_message_string "Message created") in *)
  Message {
    message_content = content;
    timestamp = timestamp;
  } 

(** A function for creating a new acknowledgement from a message *)
let create_acknowledgement_from_message (given_message : message) : message =
  match given_message with
  | Message { timestamp; _ } ->
    (* let () = print_endline (pretty_debug_message_string "Acknowledgement created") in *)
    Acknowledgement {
      message_timestamp = timestamp;
    }
  | _ ->
    let error_message = pretty_error_message_string "Cannot create acknowledgement from non-message" in
    failwith error_message  (* No action for other message types *)  

(** A function to show the content of a message *)
let show_message_content (given_message : message) : string =
  match given_message with
  | Message {message_content; _} ->
    begin
      match message_content with
      | Some content -> content
      | None -> failwith "Message content is empty"
    end
  | _ -> 
    let error_message = pretty_error_message_string "Cannot show message content from non-message" in
    failwith error_message  (* No action for other message types *)

(** A function to get the message timestamp *)
let show_message_timestamp (given_message : message) : string =
  match given_message with
  | Message {timestamp; _} -> timestamp
  | Acknowledgement {message_timestamp; _} -> message_timestamp
  | _ -> 
    let error_message = pretty_error_message_string "Cannot get message timestamp from non-message" in
    failwith error_message  (* No action for other message types *)

(** A function to print an acknowledgement *)
let print_acknowledgement (connection_addr : string) (given_message: message) (acknowledged_timestamp : string) : unit =
  let message_timestamp = show_message_timestamp given_message in
  let rtt = calculate_rtt message_timestamp acknowledged_timestamp in
  printf "[%s:ACK] - RTT: %s ms, Status: Message Received\n"
  connection_addr rtt
(** A function to print a chat message *)
let print_chat_message ~connection_address (given_message : message) =
  let connection_address = connection_address in
  let timestamp = show_message_timestamp given_message in
  let content = show_message_content given_message in
  printf "[Chat Message] [%s]: %s says %s" 
  timestamp connection_address content
