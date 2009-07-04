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
  | Type_mismatch of T.t * t

exception Invalid_value_error of error
let raise_error e =
  raise (Invalid_value_error e)

let rec string_type_of = function
  | V_byte _        -> "byte"
  | V_boolean _     -> "boolean"
  | V_int16 _       -> "int16"
  | V_uint16 _      -> "uint16"
  | V_int32 _       -> "int32"
  | V_uint32 _      -> "uint32"
  | V_int64 _       -> "int64"
  | V_uint64 _      -> "uint64"
  | V_double _      -> "double"
  | V_string _      -> "string"
  | V_object_path _ -> "object_path"
  | V_signature _   -> "signature"
  | V_array _       -> "array"
  | V_struct _      -> "struct"
  | V_variant _     -> "variant"

(* Valid String:
   A UINT32 indicating the string's length in bytes excluding its
   terminating nul, followed by non-nul string data of the given
   length, followed by a terminating nul byte.
*)
let check_valid_string s =
  let len = String.length s in
    for i = 0 to len - 1 do
      if s.[i] = '\x00' then
        raise_error (String_error String_with_embedded_nul)
    done

let is_valid_string s =
  try check_valid_string s; true with _ -> false

(* Valid Object Paths:
   . The path must begin with an ASCII '/' (integer 47) character,
     and must consist of elements separated by slash characters.
   . Each element must only contain the ASCII characters
     "[A-Z][a-z][0-9]_"
   . No element may be the empty string.
   . Multiple '/' characters cannot occur in sequence.
   . A trailing '/' character is not allowed unless the path is
     the root path (a single '/' character).
*)
let is_valid_objectpath_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '/' -> true
  | _ -> false

let check_valid_object_path s =
  let slen = String.length s in
  let prev_was_slash = ref false in
    for i = 0 to slen do
      if not (is_valid_objectpath_char s.[i]) then
        raise_error (Object_path_error OP_with_invalid_char);
      if not !prev_was_slash && s.[i] = '/' then
        prev_was_slash := true
      else if !prev_was_slash then
        if s.[i] = '/'
        then raise_error (Object_path_error OP_with_consecutive_slashes)
        else prev_was_slash := false;
    done;
    if slen > 0 then begin
      if s.[0] <> '/' then
        raise_error (Object_path_error OP_with_non_slash_prefix);
      if slen > 1 && s.[slen - 1] = '/' then
        raise_error (Object_path_error OP_is_slash_terminated);
    end

let is_valid_object_path s =
  try check_valid_object_path s; true with _ -> false

let rec type_check t v =
  match t, v with
    | T.T_base T.B_byte, V_byte _
    | T.T_base T.B_boolean, V_boolean _
    | T.T_base T.B_int16, V_int16 _
    | T.T_base T.B_uint16, V_uint16 _
    | T.T_base T.B_int32, V_int32 _
    | T.T_base T.B_uint32, V_uint32 _
    | T.T_base T.B_int64, V_int64 _
    | T.T_base T.B_uint64, V_uint64 _
    | T.T_base T.B_double, V_double _
    | T.T_base T.B_string, V_string _
    | T.T_base T.B_object_path, V_object_path _
    | T.T_base T.B_signature, V_signature _
        -> ()
    | T.T_variant, V_variant (tl, vl) ->
        (* TODO: In what cases would we get mismatched lengths? *)
        List.iter2 type_check tl vl
    | T.T_array t, V_array va ->
        Array.iter (type_check t) va
    | T.T_struct tl, V_struct vl ->
        (* TODO: In what cases would we get mismatched lengths? *)
        List.iter2 type_check tl vl
    | t, v ->
        raise_error (Type_mismatch (t, v))

(* This function cannot really be used, since we can't type 0-length
   arrays.  Instead, we're limited to using string_type_of. *)
let rec type_of = function
  | V_byte _        -> T.T_base T.B_byte
  | V_boolean _     -> T.T_base T.B_boolean
  | V_int16 _       -> T.T_base T.B_int16
  | V_uint16 _      -> T.T_base T.B_uint16
  | V_int32 _       -> T.T_base T.B_int32
  | V_uint32 _      -> T.T_base T.B_uint32
  | V_int64 _       -> T.T_base T.B_int64
  | V_uint64 _      -> T.T_base T.B_uint64
  | V_double _      -> T.T_base T.B_double
  | V_string _      -> T.T_base T.B_string
  | V_object_path _ -> T.T_base T.B_object_path
  | V_signature _   -> T.T_base T.B_signature
  | V_struct vl     -> T.T_struct (List.map type_of vl)
  | V_variant _     -> T.T_variant
  | V_array va      ->
      (match Array.length va with
         | 0 -> raise_error (Untyped_array)
         | 1 -> T.T_array (type_of va.(0))
         | n ->
             let t = type_of va.(0) in
               for i = 1 to n - 1 do
                 type_check t va.(i)
               done;
               T.T_array t
      )
