type error =
  | Invalid_endian
  | Unknown_msg_type of int
exception Parse_error of error

type state
val init_state : unit -> state

type parse_result =
  | Parse_incomplete of state
  | Parse_result of Dbus_message.t * (* unconsumed bytes *) int

val parse_substring : state -> string -> int -> int -> parse_result
