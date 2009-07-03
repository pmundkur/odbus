(* We use a universal type representation.  This breaks type-safety by
   allowing heterogenous arrays, which violate the D-Bus type system.
   We need to regain safety by using automated converters instead of
   constructing values manually.
*)

type t =
  | V_byte of char
  | V_boolean of bool
  | V_int16 of int
  | V_uint16 of int
  | V_int32 of int32
  | V_uint32 of int64
  | V_int64 of int64
  | V_uint64 of int64
  | V_double of float
  | V_string of string
  | V_object_path of string
  | V_signature of Dbus_type.t list
  | V_array of t array
  | V_struct of t list
  | V_variant of Dbus_type.t list * t list

(* Kinds of invalid errors. *)

type object_path_error =
  | OP_with_invalid_char
  | OP_with_consecutive_slashes
  | OP_with_non_slash_prefix
  | OP_is_slash_terminated

type string_error =
  | String_with_embedded_nul
  | String_not_nul_terminated

type error =
  | Untyped_array
  | String_error of string_error
  | Object_path_error of object_path_error
  | Type_mismatch of Dbus_type.t * t

exception Invalid_value_error of error

val dtypes_of_signature : t -> Dbus_type.t list
val string_type_of : t -> string

val check_valid_string : string -> unit
val is_valid_string : string -> bool

val check_valid_object_path : string -> unit
val is_valid_object_path : string -> bool

val type_check : Dbus_type.t -> t -> unit
