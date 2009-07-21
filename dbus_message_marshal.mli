(* The first function computes the space the caller of the second
   needs to supply in the buffer.  Both functions take the current
   offset in the output stream for alignment purposes.
*)
val compute_marshaled_size : int -> Dbus_message.t -> int
val marshal_message : stream_offset:int -> Dbus_type.endian -> string -> offset:int -> length:int -> Dbus_message.t -> int
