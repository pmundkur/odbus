(* The first function computes the space the caller of the second
   needs to supply in the buffer. *)
val compute_marshaled_size : int -> Dbus_message.t -> int
val marshal_message : offset:int -> Dbus_type.endian -> string -> int -> Dbus_message.t -> int
