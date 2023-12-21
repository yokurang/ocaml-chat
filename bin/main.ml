open Async

(* to ensure total ordering of messages and
  that count of messages = count of acknowledgements *)
let uniqueMessageNumber: int ref = ref 0
let uniqueAcknowledgementNumber: int ref = ref 0

let () : unit =
  let open Command.Let_syntax in
  Command.group
    ~summary:"A simple one to one terminal chatting application"
    [
      ("server", 
        Command.async
          ~summary:"Start the server and wait for a client to connect."
          ~readme:(fun () -> "
            To start the chat application as a server, please provide
            the [port] of the server. Optionally, you can also
            provide a [nick] to nickname yourself in the chat. Note that
            [port] will default to 8765 if not provided.

            For example, you can run the following command to start the server:
            dune exec -- one_on_one_chat server -port 8765 -nick \"ServerNickname\"
          ")
          (let%map_open port = flag "-port" 
            (optional_with_default 8765 int) 
            ~doc:"PORT The port number of the server (default: 8765)"
            and nick = flag "-nick"
            (optional_with_default "anonymous alan" string) 
            ~doc:"NICK The nickname for the chat session (default: anonymous andy)"
            in
            fun () ->
              printf "Starting server to %d with nickname: %s\n" port nick;
              printf "The initial message number is %d\n" !uniqueMessageNumber;
              printf "The initial acknowledge number is %d\n" !uniqueAcknowledgementNumber;
              let stdin_pipe = (Reader.pipe (Lazy.force Reader.stdin)) in
              Server.start_server
                ~port
                ~nick
                ~uniqueMessageNumber
                ~uniqueAcknowledgementNumber
                ~stdin_pipe
          )
      );

      ("client", 
        Command.async
          ~summary:"Attempt to connect to a server to initiate chat."
          ~readme:(fun () -> "
            To start the chat application as a client, please provide
            the [host] and [port] of the server. Optionally, you can also
            provide a [nick] to nickname yourself in the chat. Note that
            [host] will default to 127.0.0.1 and [port] will default to
            8765 if not provided.

            For example, you can run the following command to start the client:
            dune exec -- one_on_one_chat client -host 127.0.0.1 -port 8765 -nick \"ClientNickname\"
          ")
          (let%map_open host = flag "-host"
            (optional_with_default "127.0.0.1" string) 
            ~doc:"HOSTNAME The hostname of the server (default: 127.0.0.1)"
            and port = flag "-port" 
            (optional_with_default 8765 int) 
            ~doc:"PORT The port number of the server (default: 8765)"
            and nick = flag "-nick"
            (optional_with_default "anonymous alan" string) 
            ~doc:"NICK The nickname for the chat session (default: anonymous alan)"
            in
            fun () ->
              printf "Connecting to %s:%d with nickname: %s\n" host port nick;
              let stdin_pipe = (Reader.pipe (Lazy.force Reader.stdin)) in
              Client.start_client
                ~host
                ~port
                ~nick
                ~uniqueMessageNumber
                ~uniqueAcknowledgementNumber
                ~stdin_pipe
          )
      );
    ]
  |> Command_unix.run