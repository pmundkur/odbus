module V = Dbus_value
module P = Dbus_type_parse

type error =
  | Unexpected_dbus_type of (* rcvd *) string * (* expected *) string
  | Integer_out_of_range
  | Invalid_string of string
  | Invalid_object_path of string


exception Dbus_conv of error
let raise_error e =
  raise (Dbus_conv e)

let to_byte = function
  | V.V_byte c -> c
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "byte"))

let of_byte c = V.V_byte c

let to_boolean = function
  | V.V_boolean b -> b
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "boolean"))

let of_boolean b = V.V_boolean b

let to_int16 = function
  | V.V_int16 i -> i
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "int16"))

let of_int16 i =
  if i >= -32768 && i < 32768 then V.V_int16 i else raise_error Integer_out_of_range

let to_uint16 = function
  | V.V_uint16 i -> i
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "uint16"))

let of_uint16 i =
  if i land 0xffff = i then V.V_int16 i else raise_error Integer_out_of_range

let to_int32 = function
  | V.V_int32 i -> i
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "int32"))

let of_int32 i = V.V_int32 i

let to_uint32 = function
  | V.V_uint32 i -> i
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "uint32"))

let of_uint32 i =
  if Int64.logand i 0xffffffffL = i then V.V_uint32 i else raise_error Integer_out_of_range

let to_int64 = function
  | V.V_int64 i -> i
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "int64"))

let of_int64 i = V.V_int64 i

let to_uint64 = function
  | V.V_uint64 i -> i
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "uint64"))

(* TODO: fix signedness check *)
let of_uint64 i = V.V_uint64 i

let to_double = function
  | V.V_double d -> d
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "double"))

let of_double d = V.V_double d

let to_string = function
  | V.V_string s -> s
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "string"))

let of_string s =
  if P.is_valid_string s then V.V_string s else raise_error (Invalid_string s)

let to_object_path = function
  | V.V_object_path o -> o
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "object_path"))

let of_object_path o =
  if P.is_valid_object_path o then V.V_object_path o else raise_error (Invalid_object_path o)

let to_signature = function
  | V.V_signature s -> s
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "signature"))

let of_signature tl = V.V_signature tl

let to_array = function
  | V.V_array a -> a
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "array"))

let of_array a = V.V_array a

let to_struct = function
  | V.V_struct s -> s
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "struct"))

let of_struct s = V.V_struct s

let to_variant = function
  | V.V_variant (t, v) -> (t, v)
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "variant"))

let of_variant (t, v) = V.V_variant (t, v)
