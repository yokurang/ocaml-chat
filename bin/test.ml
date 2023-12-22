open InputOutputHandlers
open Core
open Async
open DataTypes (* Assuming DataTypes contains the definition of `message` and `Fail` *)

(* let test_handle_user_payload () =
  (* Create a mock global state *)
  let mock_global_state = {
    uniqueMessageNumber = ref 0;
    uniqueAcknowledgementNumber = ref 0;
    client_nickname = ref (Some "ClientNickname");
    server_nickname = ref (Some "ServerNickname");
    connection_address = ref (Some "127.0.0.1:8000");
  } in

  (* Simulate receiving an InputOk payload *)
  let w = Lazy.force Writer.stdout in
  let piped_w = Writer.pipe w in
  let payload = InputOk "Test message" in
  let participant_type = Client in (* Or Server, depending on the test case *)
  print_endline "Testing InputOk payload...\n";
  handle_user_payload payload piped_w ~global_state:mock_global_state ~participant_type

let () =
  Printf.printf "Staring test...\n";
  don't_wait_for (test_handle_user_payload ());
  never_returns (Scheduler.go ()) *)


(* open InputOutputHandlers

let mock_message = Message { 
  message_id = 1;
  message_content = Some "Hello world";
  message_from = Server;
  message_to = Client;
  timestamp = Time_ns_unix.to_string (Time_ns_unix.now ());
}

(* let test_function () =
  let reader, writer = Pipe.create () in
  write_message writer mock_message;
  Pipe.close writer;

  (* Read from the pipe *)
  Pipe.read reader >>= function
  | `Ok message_read ->
      Printf.printf "Read from pipe: %s\n" message_read;
      Deferred.unit
  | `Eof ->
      Printf.printf "End of pipe reached.\n";
      Deferred.unit *)
  

let test4 () =
  let w = Lazy.force Writer.stdout in
  let piped_w = Writer.pipe w in
  write_message piped_w mock_message;
  Writer.flushed w

let () =
  (* Start the Async scheduler and run the test function *)
  Printf.printf "starting the test";
  (* don't_wait_for (test_function ());
  Printf.printf "starting test2";
  don't_wait_for (test2 ());
  Printf.printf "starting test3";
  don't_wait_for (test3 ()); *)
  Printf.printf "starting test4";
  don't_wait_for (test4 ());
  never_returns (Scheduler.go ())
 *)





(* to ensure total ordering of messages and
  that count of messages = count of acknowledgements *)


(* let send_maybe w msg =
  if Writer.is_open w
    then sexp_of_message msg |> Sexp.to_string_hum |> Writer.write_line w
  (* then yojson_of_msg msg |> Yojson.Safe.to_string |> Writer.write_line w *)
  else Out_channel.print_endline
    "[Warning: the writer is closed. The remote server/client may have disconnected.]" *)

(* let send_maybe w msg =
  if Writer.is_open w
    then sexp_of_message msg |> Sexp.to_string_hum |> Writer.write_line w
  (* then yojson_of_msg msg |> Yojson.Safe.to_string |> Writer.write_line w *)
  else Out_channel.print_endline
    "[Warning: the writer is closed. The remote server/client may have disconnected.]" *)

let test_function () =
  let reader, writer = Pipe.create () in
  let mock_message = Fail { error_message = "Test error" } in
  write_message writer mock_message;
  Pipe.close writer;

  (* Read from the pipe *)
  Pipe.read reader >>= function
  | `Ok message_read ->
      Printf.printf "Read from pipe: %s\n" message_read;
      Deferred.unit
  | `Eof ->
      Printf.printf "End of pipe reached.\n";
      Deferred.unit
  
let () =
  (* Start the Async scheduler and run the test function *)
  Printf.printf "starting the test";
  don't_wait_for (test_function ());
  never_returns (Scheduler.go ())

(* let create_mock_pipes () =
  let reader_pipe, _writer = Pipe.create () in  (* Dummy reader pipe *)
  let writer_pipe, _ = Pipe.create () in        (* Actual writer pipe *)
  (reader_pipe, writer_pipe)

let (mock_incoming_pipe, _) = create_mock_pipes ()

let mock_server_state = {
  curr_message_id = ref 0;
  curr_acknowledgement_id = ref 0;
  incoming_pipe = ref (Some mock_incoming_pipe);   (* Correctly using reader pipe *)
  writer = ref (Some (Lazy.force Writer.stdout));  (* Correctly using writer pipe *)
  socket_address = ref (Some "127.0.0.1:8475");
  client_nickname = ref (Some "anonymous alan");
  server_nickname = ref (Some "127.0.0.1");
}
let test_handle_input_payload () =
    let server_state = mock_server_state in
    let stdin_pipe = Reader.lines (Lazy.force Reader.stdin) in  (* Create a pipe from stdin *)
    InputOutputHandlers.handle_message_received ~server_state ~stdin_pipe

let () =
  don't_wait_for (test_handle_input_payload ());
  never_returns (Scheduler.go ()) *)
  
(* Global server state 
  defined during start up
*)
(* let server_state = {
  curr_message_id = ref 0;
  curr_acknowledgement_id = ref 0;
  client_nickname = ref None;
  server_nickname = ref None;
  socket_address = ref None;
  curr_writer_pipe = ref None;
  curr_reader_pipe = ref None;
} *)
