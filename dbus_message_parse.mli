type error =
  | Invalid_endian
  | Unknown_msg_type of int
  | Unexpected_header_arity of Dbus_message.header * (* received *) Dbus_type.t list
  | Unexpected_header_type of Dbus_message.header * (* received *) Dbus_type.t * (* expected *) Dbus_type.t
  | Missing_signature_header_for_payload
  | Missing_required_header of Dbus_message.msg_type * Dbus_message.header

exception Parse_error of error

type state

(* init_state is called with the current-offset in the bytestream
   being parsed.  this is needed to compute the current alignment for
   the parser. *)
val init_state : int -> state

type parse_result =
  | Parse_incomplete of state
  | Parse_result of Dbus_message.t * (* unconsumed bytes *) int

val parse_substring : state -> string -> int -> int -> parse_result
