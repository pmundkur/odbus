type error =
  | Internal_error of string
  | Unexpected_char of char
  | Unrecognized_server_protocol of string

exception Auth_error of error

type auth_mechanism =
  | External
  | Anonymous

type auth_state =
  | Auth_in_progress
  | Auth_failed
  | Auth_succeeded of (* server address *) string * (* number of consumed bytes *) int

type client_context

(* The caller needs to supply a function that can send a string to the peer. *)
type protocol_sender = string -> unit

val init_client_context : auth_mechanism -> protocol_sender -> client_context * auth_state
val parse_input : client_context -> protocol_sender -> string -> int -> int -> auth_state
