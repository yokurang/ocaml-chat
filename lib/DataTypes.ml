open Sexplib.Std

type payload =
  | Eof
  | Ok of string

type participant =
  | Server
  | Client
  [@@deriving sexp]

type payloadSource =
  | Socket of payload
  | Stdin of payload

type global_state = {
  uniqueMessageNumber: int ref;
  uniqueAcknowledgementNumber: int ref;
  client_nickname: string option ref;
  server_nickname: string option ref;
  connection_address: string option ref;
}

type message =
| Message of { (* a message contains the payload and its timestamp as a string for round-trip (RT) calculation and the author and recipient of the message *)
  message_content: string option;
  message_id: int;
  message_from: participant;
  message_to: participant;
  timestamp: string;
} 
| Acknowledgement of { (* an acknowledgement contains the acknowledgement payload and its timestamp as a string for RT calculation and the author and recipient of the message *)
  acknowledgement_id: int;
  message_id: int;
  message_from: participant;
  message_to: participant;
  message_timestamp: string;
}
| Fail of {
  error_message: string (* in case something happens *)
}
[@@deriving sexp]

