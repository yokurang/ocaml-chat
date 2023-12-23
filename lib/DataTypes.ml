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

type message =
(** Message:
    I need a message type to send messages between the server and client.
    The message type will contain the following:
      1. message_content, which is the actual message. 
        It is of type string option so we can handle the exception
        when a message with no content is sent.
      2. The message timestamp, which is the timestamp in which the message was sent
        The type is string since it is easier to parse and compare.
        This will be passed to the acknowledgement type. *)
| Message of {
  message_content: string option;
  timestamp: string;
} 
(** 
  Acknowledgement:
  The acknowledgement type will contain the following:
    1. The message timestamp, which is the timestamp in which the message was sent
      The type is string since it is easier to parse and compare.
      This will be used for RT calculation. *)
| Acknowledgement of {
  message_timestamp: string;
}
(** Fail:
    I need a message type in case an exception occurs while parsing an S-expression *)
| Fail of {
  error_message: string
}
[@@deriving sexp]
