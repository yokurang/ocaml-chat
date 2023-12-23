open Async

let () : unit =
  let open Command.Let_syntax in
  Command.group
    ~summary:"A simple one to one terminal chatting application"
    [
      ("server", 
        Command.async
          ~summary:"Start the server and wait for a client to connect."
          ~readme:(fun () -> "
            As an example, you can start the server with:
            dune exec -- one_on_one_chat server -port 8765
          ")
          (let%map_open port = flag "-port" 
            (optional_with_default 8765 int) 
            ~doc:"PORT The port number of the server (default: 8765)"
            in
            fun () ->
              let stdin_reader_pipe = (Reader.pipe (Lazy.force Reader.stdin)) in
              Server.start_server
                ~port
                ~stdin_reader_pipe
          )
      );

      ("client", 
        Command.async
          ~summary:"Attempt to connect to a server to initiate chat."
          ~readme:(fun () -> "
            As an example, you can start the client with:
            dune exec -- one_on_one_chat client -host 127.0.0.1 -port 8765
          ")
          (let%map_open host = flag "-host"
            (optional_with_default "127.0.0.1" string) 
            ~doc:"HOSTNAME The hostname of the server (default: 127.0.0.1)"
            and port = flag "-port" 
            (optional_with_default 8765 int) 
            ~doc:"PORT The port number of the server (default: 8765)"
            in
            fun () ->
              let stdin_reader_pipe = (Reader.pipe (Lazy.force Reader.stdin)) in
              Client.start_client
                ~host
                ~port
                ~stdin_reader_pipe
          )
      );
    ]
  |> Command_unix.run
