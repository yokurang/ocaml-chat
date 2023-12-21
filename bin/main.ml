open Async
open DataTypes

let global_state = {
  uniqueMessageNumber = ref 0;
  uniqueAcknowledgementNumber = ref 0;
  client_nickname = ref None;
  server_nickname = ref None;
  connection_address = ref None;
}

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
              let stdin_pipe = (Reader.pipe (Lazy.force Reader.stdin)) in
              Server.start_server
                ~port
                ~nick
                ~global_state
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
              let stdin_pipe = (Reader.pipe (Lazy.force Reader.stdin)) in
              Client.start_client
                ~host
                ~port
                ~nick
                ~global_state
                ~stdin_pipe
          )
      );
    ]
  |> Command_unix.run