type value =
  | V_byte of char
  | V_boolean of bool
  | V_int16 of int
  | V_uint16 of int
  | V_int32 of int32
  | V_uint32 of int64
  | V_int64 of int64
  | V_uint64 of int64  (* We risk signedness issues here. *)
  | V_double of float
  | V_string of string
  | V_object_path of string
  | V_signature of Dbus_type.t list
  | V_array of value array
  | V_struct of value list
  | V_variant of string * value
