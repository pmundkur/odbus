type inv_reason =
  | Inv_non_boolean
  | Inv_embedded_nul
  | Inv_not_nul_terminated
  | Inv_objectpath_char
  | Inv_consecutive_slashes
  | Inv_non_slash_prefix
  | Inv_slash_terminated
  | Inv_signature of Dbus_type.sig_error
  | Inv_array_length

type error =
  | Insufficient_data of Dbus_type.t
  | Invalid_value of Dbus_type.t * inv_reason

exception Parse_error of error

type context =
    {
      endian : Dbus_type.endian;
      buffer : string;
      offset : int;
      length : int;
    }

val init_context : Dbus_type.endian -> string -> int -> int -> context

val append_bytes : context -> string -> int -> int -> context
val advance : context -> int -> context
val rewind : context -> int -> context

val take_byte : ?dtype:Dbus_type.t -> context -> char * context
val parse_byte : context -> Dbus_value.t * context

val take_int16 : context -> int * context
val take_uint16 : context -> int * context
val parse_int16 : context -> Dbus_value.t * context
val parse_uint16 : context -> Dbus_value.t * context

val take_uint32 : ?dtype:Dbus_type.t -> context -> int64 * context
val parse_int32 : context -> Dbus_value.t * context
val parse_uint32 : context -> Dbus_value.t * context

val parse_boolean : context -> Dbus_value.t * context

val take_uint64 : ?dtype:Dbus_type.t -> context -> int64 * context
val parse_int64 : context -> Dbus_value.t * context
val parse_uint64 : context -> Dbus_value.t * context

val take_string : ?dtype:Dbus_type.t -> context -> string * context
val parse_string : context -> Dbus_value.t * context
val parse_object_path : context -> Dbus_value.t * context

val parse_signature : context -> Dbus_value.t * context

val parse_double : context -> Dbus_value.t * context

val parse_complete_type : Dbus_type.t -> context -> Dbus_value.t * context
val parse_type_list : Dbus_type.t list -> context -> Dbus_value.t list * context
