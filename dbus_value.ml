module T = Dbus_type

type t =
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
  | V_signature of T.t list
  | V_array of t array
  | V_struct of t list
  | V_variant of T.t list * t list

let dtypes_of_signature v =
  match v with
    | V_signature tl -> tl
    | _ -> assert false

let rec string_type_of = function
  | V_byte _ -> "byte"
  | V_boolean _ -> "boolean"
  | V_int16 _ -> "int16"
  | V_uint16 _ -> "uint16"
  | V_int32 _ -> "int32"
  | V_uint32 _ -> "uint32"
  | V_int64 _ -> "int64"
  | V_uint64 _ -> "uint64"
  | V_double _ -> "double"
  | V_string _ -> "string"
  | V_object_path _ -> "object_path"
  | V_signature _ -> "signature"
  | V_array _ -> "array"
  | V_struct _ -> "struct"
  | V_variant _ -> "variant"
