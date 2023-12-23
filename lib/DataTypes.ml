open Sexplib.Std

(* 
  Throughout building this application, I must constantly ask myself
                Are my data types as simple as they can be?
*)

(** Payload:
    I need a mechanism to determine whether I have reached the end of the payload steam.
      InputEof indicates when the Eof condition is hit
      InputOk indicates that the payload is not empty and contains a string to read *)
type payload =
  | InputEof
  | InputOk of string

(** Participant:
    Since this is a one-on-one chat application, we either have
    1. A server sending a message to a client
    2. Or a client sending a message to a server *)

(** PayloadSource:
    The chat application has to simultaneously listen to the socket and stdin.
      1. If the payload is from stdin, it has to be sent over to the other participant
      2. If the payload is from the socket, an acknowledgement has to be sent
        to the original sender
      As such I need a mechanism to distinguish between the payload between
      the sources and perform the appropritate action. *)
type payloadSource =
  | Socket of payload
  | Stdin of payload

(** GlobalState:
    I need a global state to store some common, useful global variables.
    To keep the application simple but interactive, I want to add a nickname feature.
    As such, I will need to store the following:
      1. The client's nickname
      2. The server's nickname
      3. The client's connection address
      4. The server's connection address *)

(** Message:
    I need a message type to send messages between the server and client.
    The message type will contain the following:
      1. message_content, which is the actual message. 
        It is of type string option so we can handle the exception
        when a message with no content is sent.
      2. message_from, which is the author of the message, i.e the sender
        The type is participant since it can only be from either the server or client.
        This will be used for pretty log messages.
      3. message_to, which is the recipient of the message, i.e the receiver
        The type is participant since it can only be to either the server or client.
        This will be used for pretty log messages.
      4. The message timestamp, which is the timestamp in which the message was sent
        The type is string since it is easier to parse and compare.
        This will be used for RT calculation.
      Most of this information will be passed to the acknowledgement type.

  Acknowledgement:
  The acknowledgement type will contain the following:
    1. message_content, which is the actual message. 
      It is of type string option so we can handle the exception
      when a message with no content is sent.
    2. message_from, which is the author of the message, i.e the sender
      The type is participant since it can only be from either the server or client.
      This will be used for pretty log messages.
    3. message_to, which is the recipient of the message, i.e the receiver
      The type is participant since it can only be to either the server or client.
      This will be used for pretty log messages.
    4. The message timestamp, which is the timestamp in which the message was sent
      The type is string since it is easier to parse and compare.
      This will be used for RT calculation.

Fail
  I need a message type in case an error occurs when parsing from S-Expression

ClientConnection
  I need a message type to log when a new connection request has been made

ServerConnection
  I need a message type to log when a connection request has been accepted *)
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
