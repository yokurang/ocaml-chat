open Core

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
  sprintf "[%s] [%s] - %s\n" log_level pretty_date message

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
