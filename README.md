# Ahrefs Coding Challenge: One on one chat application in OCaml

This README contains the following sections:

- [Installation](#installation)
- [Usage](#usage)
- [Project Structure](#project-structure)
- [System Design](#system-design)
- [Testing](#testing)

## Installation

To run the project locally, please follow the following steps:

First, you might want to update your opam packages. Note that this dune project requires dune 3.1.2 or higher.

```bash
opam update
```

or

```bash
opam update && opam upgrade
```

Then, install the dependencies via opam:

```bash
opam install core async sexplib ppx_jane
```

or

```bash
opam install . --deps-only
```

Finally, build the project:

```bash
dune clean && dune build
```

Please make sure you have your `_build` directory after running the above command so that you can run the project and follow the instructions in the next section.

## Usage

To run the server, run the following command:

```bash
dune exec -- one_on_one_chat server -port [port]
```

As an example, to start the server on port 8765, run the following command:

```bash
dune exec -- one_on_one_chat server -port 8765
```

To run the client, run the following command:

```bash
dune exec -- one_on_one_chat client -host [hostname] -port [port]
```

As an example, to start the client on host 127.0.0.1 and port 8765, run the following command:

```bash
dune exec -- one_on_one_chat client -host 127.0.0.1 -port 8765
```

## Project Structure

The project is structured as follows:

```bash
.
├── README.md
├── bin
│   ├── dune
│   └── main.ml
├── dune-project
├── lib
│   ├── Client.ml
│   ├── DataTypes.ml
│   ├── InputOutputHandlers.ml
│   ├── Server.ml
│   ├── Utils.ml
│   └── dune
├── one_on_one_chat.opam
└── test
    ├── dune
    └── test_one_on_one_chat.ml
```

### `main.ml`

The file `main.ml` contains the entry point for the application. It parses the command line arguments and calls the appropriate functions in `Server.ml` and `Client.ml`.

### `Server.ml`

The file `Server.ml` implementss the function to start the server.

### `Client.ml`

The file `Client.ml` implements the function to start the client.

### `DataTypes.ml`

The file `DataTypes.ml` defines the data types used in the application.

### `Utils.ml`

The file `Utils.ml` defines utility functions to produce pretty printing and logging.

### `InputOutputHandlers.ml`

The file `InputOutputHandlers.ml` defines the functions to handle input from the user and implements the message sending logic.

## System Design

### Implementing the Message Sending Logic

In building this one to one chat application, I found the most difficult challenge to be implementing the logic to correctly handle between reading from stdin and reading from the socket. Essentially, I need a mechanism to watch both stdin and the socket reader at the same time and react to either appropriately. I solved this problem using `Deferred.choose` and `Pipe.read_choice_single_consumer_exn socket_reader_pipe` from the Pipe interface provided by the Async library. This particular design choice is to ensure that, in a one on one chat application where a specific client or server instance is handling messages from one counterpart, i.e one is a producer and the other is a consumer of messages, at a time, we can choose to only read from stdin or only read from the socket so that messages are processed in the correct order and by the correct recipient, and that messages are not lost in the other pipe. Furthermore, this design choice also made it easier for me to handle the case when the `Eof` condition has been hit, i.e. when the socket is disconnected, and print the appropriate message to the user. This is implemented in `handle_connection` of `InputOutputHandlers.ml`.

### Continued Waiting for Messages to be Sent from Either Party

Another challenge I spent a lot of time on is figuring out a way to ensure that, once the server and client are connected and either party sends a message, we continue waiting for more messages to be sent from either party until one disconnects using `control-c`. I solved this problem by having my `handle_socket_payload` in `InputOutputHandlers.ml` return a `bool Deferred.t` that indicates whether the socket is still connected. If the socket is still connected, we continue to wait for more messages to be sent from either party. If the socket is disconnected, we stop waiting for more messages to be sent from either party and exit the program. This logic is implemented in this code snippet from `handle_connection` of `InputOutputHandlers.ml`.

```ocaml
let ping_pong = handle_socket_payload payload ~connection_address socket_writer_pipe in
    Deferred.bind ping_pong ~f:(fun result ->
      if result then
        handle_connection ~socket_reader_pipe ~socket_writer_pipe ~stdin_reader_pipe ~connection_address
      else
        return ()
    )
```

I spent a lot of time thinking about this, and it is one of my most fulfilling *a-ha* moments in building this chat application.

### Implementing the Data Types

Another important challenge was deciding the data types I needed for this chat application. I realised that I needed a mechanism to differentiate when I the `Eof` condition has been hit, or when we still have more input to read. Futhermore, I needed a way to distinguish whether I am reading from stdin or from the socket. As such, I defined a data type to represent the different types of input/payload I can receive, as well as a data type to represent the sources of the payload. This is implemented in `DataTypes.ml`. Finally, I needed a way to represent the different types of messages that need to be handled. Based on the task specifications, there are two types of messages, a "Message" sent from the sender and an "Acknowledgement". For me, I felt that a "Message" needs to carry the message content as well as the timestamp in which it was created (analogous to created_at in most databases or APIs). Furthermore, I felt that an "Acknowledgement" needs to carry the message timestamp it is acknowledging for round-trip time calculation. I also recognised that since I am using S-expressions to encode and decode my messages, there is an off-chance in which the parsing between messages and S-expressions may fail, and so I also have a "Fail" message to handle this exceptional case. Note that I chose to use S-expressions so that I can easily encode and decode my messages to and from strings.

Here is a code snippet showcasing how I implement my `message` data type:

```ocaml
type message =
    | Message of {
    message_content: string option;
    timestamp: string;
    } 
    | Acknowledgement of {
    message_timestamp: string;
    }
    | Fail of {
    error_message: string
    }
[@@deriving sexp]
```

Another reason I implemented my messages this way is that it makes it easier to implement the functions to handle the logic to respond to these messages, namely `handle_socket_message_list` from `InputOutputHandlers.ml`. Furthermore, it is arguably easier to write the code to add more message types one might want to add and handle as one would only need to add more branches in the match statement.

You may find the implementation of these data types and more details about them in `DataTypes.ml`.

### Handling Exceptions when Parsing Messages

Finally, I also spent a lot of time debugging the following bug:

```bash
[ERROR] [24/12/2023 10:29:48] - Failed to parse message: (Message(message_content("\n"))(timestamp"2023-12-24 10:29:48.397585000+07:00"))(Message(message_content("\n"))(timestamp"2023-12-24 10:29:48.397604000+07:00")) with error: (Failure
  "Sexplib.Sexp.of_string: got multiple S-expressions where only one was expected.")
```

Observing the error message, I realised that the error was triggered by `parse_string_to_message_sexp`. The reason is that `Sexp.of_string message` raises an exception if the string contains multiple S-expressions. This is because `Sexp.of_string` expects a single S-expression. As such, I implemented a function `parse_string_to_message_sexp_list` which parses a string to a list of S-expressions to handle this scenario. Furthermore, I also implemented a new function `handle_socket_message_list`, which is a recursive function that handles a list of S-expressions. This is implemented in `InputOutputHandlers.ml`. This trick of parsing a string to a list of S-expressions helped me solve this bug.

You may find the implementation of these functions and more details about them in `InputOutputHandlers.ml`.

## Testing

The following lists the edge cases I considered when testing this application:

### Edge Cases

- [x] Start the server on a port that is already in use
- [x] Start the client on a host that is not reachable
- [x] Start the client on a port that is not in use
- [x] Start the client on a port that is already in use
- [x] Start the server and client, send messages to each other, then disconnect the client and then disconnect the server and vice verse.
- [x] Start the server and client, send messages to each other, then disconnect the client, the server waits, and then a new client connects to the server and sends messages to each other.
- [x] Start the server, enter some messages, then disconnect the server
- [x] Start the server, enter some messages, connect the client, send messages to each other, disconnect the client, enter some more messages, then connect the client again, then disconnect either.
