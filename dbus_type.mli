type base =
  | B_byte
  | B_boolean
  | B_int16
  | B_uint16
  | B_int32
  | B_uint32
  | B_int64
  | B_uint64
  | B_double
  | B_string
  | B_object_path
  | B_signature

type t =
  | T_base of base
  | T_variant
  | T_array of t
  | T_struct of t list

type endian =
  | Little_endian
  | Big_endian

val to_string : t -> string
val to_code : t -> string

val is_basic_type : t -> bool
val is_container_type : t -> bool

val alignment_of : t -> int
val get_padding : int -> int -> int

type sig_error =
  | Sig_incomplete
  | Sig_invalid of string
  | Sig_invalid_char of char

exception Invalid_signature of sig_error

val signature_of_string : string -> t list
val signature_of_types : t list -> string
