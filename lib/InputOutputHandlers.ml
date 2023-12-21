open Core
open Async
(* open Sexplib.Std *)
open DataTypes
open Utils

let write_message (w : string Pipe.Writer.t) (given_message : message) : unit =
  match given_message with
  | Fail {error_message} ->
    let err_message = sprintf "Failed to send message: %s" error_message in
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

(* let rec handle_user_stdin ~uniqueMessageNumber ~stdin_pipe : unit Deferred.t=
  Pipe.read_choice_single_consumer_exn stdin_pipe [%here]
  |> Deferred.Choice.map ~f:(function
  | `Eof -> return ()
  | `Ok message ->
   
    Deferred.unit
  ) 
   *)