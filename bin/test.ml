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
