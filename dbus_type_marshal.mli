type error =
  | Signature_too_long
  | Insufficient_space of Dbus_type.t

exception Marshal_error of error

type context

val init_context : Dbus_type.endian -> string -> offset:int -> length:int -> context

val compute_marshaled_size : offset:int -> Dbus_type.t -> Dbus_value.t -> int

val marshal_complete_type : context -> Dbus_type.t -> Dbus_value.t -> context
