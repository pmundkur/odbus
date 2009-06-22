module T = Dbus_type
module V = Dbus_value

type inv_reason =
  | Inv_non_boolean
  | Inv_embedded_nul
  | Inv_not_nul_terminated
  | Inv_objectpath_char
  | Inv_consecutive_slashes
  | Inv_non_slash_prefix
  | Inv_slash_terminated
  | Inv_signature of T.sig_error
  | Inv_array_length

type error =
  | Insufficient_data of T.t
  | Invalid_value of T.t * inv_reason

exception Parse_error of error
let raise_error e =
  raise (Parse_error e)

type endian =
  | Little_endian
  | Big_endian

type context =
    {
      endian : endian;
      buffer : string;
      offset : int;
      length : int;
    }

let advance ctxt nbytes =
  assert (ctxt.length >= nbytes);
  { ctxt with
      offset = ctxt.offset + nbytes;
      length = ctxt.length - nbytes }

let check_and_align_context ctxt alignment size dtype =
  let padding = T.get_padding ctxt.offset alignment in
    if ctxt.length < size + padding then
      raise_error (Insufficient_data dtype);
    advance ctxt padding

let take_byte dtype ctxt =
  let align = T.alignment_of (T.T_base T.B_byte) in
  let ctxt = check_and_align_context ctxt align 1 dtype in
  let b = ctxt.buffer.[ctxt.offset] in
    b, advance ctxt 1

let parse_byte ctxt =
  let b, ctxt = take_byte (T.T_base T.B_byte) ctxt in
    V.V_byte b, ctxt

let to_uint16 endian b0 b1 =
  match endian with
    | Little_endian -> b0 + (b1 lsl 8)
    | Big_endian -> b1 + (b0 lsl 8)

let to_int16 endian b0 b1 =
  let i, sign = match endian with
    | Little_endian -> b0 + (b1 lsl 8), b1 lsr 7
    | Big_endian -> b1 + (b0 lsl 8), b0 lsr 7
  in if sign = 0 then i else (i land 0x7fff) - 0x8000

let to_int32 endian q0 q1 =
  let module I = Int32 in
  let q0, q1 = I.of_int q0, I.of_int q1 in
    match endian with
      | Little_endian -> I.add q0 (I.shift_left q1 16)
      | Big_endian -> I.add q1 (I.shift_left q0 16)

let to_uint32 endian q0 q1 =
  let module I = Int64 in
  let q0, q1 = I.of_int q0, I.of_int q1 in
    match endian with
      | Little_endian -> I.add q0 (I.shift_left q1 16)
      | Big_endian -> I.add q1 (I.shift_left q0 16)

let to_uint64 endian u0 u1 =
  let module I = Int64 in
    match endian with
      | Little_endian -> I.add u0 (I.shift_left u1 32)
      | Big_endian -> I.add u1 (I.shift_left u0 32)

let take_i16 sign ctxt =
  let to_fn, dtype =
    if sign then to_int16, (T.T_base T.B_int16)
    else to_uint16, (T.T_base T.B_uint16) in
  let align = T.alignment_of dtype in
  let ctxt = check_and_align_context ctxt align 2 dtype in
  let b0 = Char.code ctxt.buffer.[ctxt.offset] in
  let b1 = Char.code ctxt.buffer.[ctxt.offset + 1] in
    to_fn ctxt.endian b0 b1, advance ctxt 2

let take_int16 = take_i16 true
let take_uint16 = take_i16 false

let parse_int16 ctxt =
  let i, ctxt = take_int16 ctxt in
    V.V_int16 i, ctxt

let parse_uint16 ctxt =
  let i, ctxt = take_uint16 ctxt in
    V.V_uint16 i, ctxt

let take_uint32 dtype ctxt =
  let align = T.alignment_of (T.T_base T.B_int32) in
  let ctxt = check_and_align_context ctxt align 4 dtype in
  let b0 = Char.code ctxt.buffer.[ctxt.offset] in
  let b1 = Char.code ctxt.buffer.[ctxt.offset + 1] in
  let b2 = Char.code ctxt.buffer.[ctxt.offset + 2] in
  let b3 = Char.code ctxt.buffer.[ctxt.offset + 3] in
  let q0 = to_uint16 ctxt.endian b0 b1 in
  let q1 = to_uint16 ctxt.endian b2 b3 in
    to_uint32 ctxt.endian q0 q1, advance ctxt 4

let parse_uint32 ctxt =
  let i, ctxt = take_uint32 (T.T_base T.B_uint32) ctxt in
    V.V_uint32 i, ctxt

let parse_int32 ctxt =
  let dtype = T.T_base T.B_int32 in
  let align = T.alignment_of dtype in
  let ctxt = check_and_align_context ctxt align 4 dtype in
  let b0 = Char.code ctxt.buffer.[ctxt.offset] in
  let b1 = Char.code ctxt.buffer.[ctxt.offset + 1] in
  let b2 = Char.code ctxt.buffer.[ctxt.offset + 2] in
  let b3 = Char.code ctxt.buffer.[ctxt.offset + 3] in
  let q0 = to_uint16 ctxt.endian b0 b1 in
  let q1 = to_uint16 ctxt.endian b2 b3 in
    V.V_int32 (to_int32 ctxt.endian q0 q1), advance ctxt 4

let parse_boolean ctxt =
  let dtype = T.T_base T.B_boolean in
  let i, ctxt = take_uint32 dtype ctxt in
  let b =
    if i <> 0L && i <> 1L
    then raise_error (Invalid_value (dtype, Inv_non_boolean))
    else (if i = 0L then false else true)
  in
    V.V_boolean b, ctxt

(* TODO: check int64 (and other!) sanity! *)
let take_uint64 dtype ctxt =
  let align = T.alignment_of (T.T_base T.B_int64) in
  let ctxt = check_and_align_context ctxt align 8 dtype in
  let b0 = Char.code ctxt.buffer.[ctxt.offset] in
  let b1 = Char.code ctxt.buffer.[ctxt.offset + 1] in
  let b2 = Char.code ctxt.buffer.[ctxt.offset + 2] in
  let b3 = Char.code ctxt.buffer.[ctxt.offset + 3] in
  let b4 = Char.code ctxt.buffer.[ctxt.offset + 4] in
  let b5 = Char.code ctxt.buffer.[ctxt.offset + 5] in
  let b6 = Char.code ctxt.buffer.[ctxt.offset + 6] in
  let b7 = Char.code ctxt.buffer.[ctxt.offset + 7] in
  let q0 = to_uint16 ctxt.endian b0 b1 in
  let q1 = to_uint16 ctxt.endian b2 b3 in
  let q2 = to_uint16 ctxt.endian b4 b5 in
  let q3 = to_uint16 ctxt.endian b6 b7 in
  let u0 = to_uint32 ctxt.endian q0 q1 in
  let u1 = to_uint32 ctxt.endian q2 q3 in
    to_uint64 ctxt.endian u0 u1, advance ctxt 8

let parse_uint64 ctxt =
  let u, ctxt = take_uint64 (T.T_base T.B_uint64) ctxt in
    V.V_uint64 u, ctxt

let parse_int64 ctxt =
  let i, ctxt = take_uint64 (T.T_base T.B_int64) ctxt in
    V.V_int64 i, ctxt

(* Valid String:
   A UINT32 indicating the string's length in bytes excluding its
   terminating nul, followed by non-nul string data of the given
   length, followed by a terminating nul byte.
*)
let take_string dtype ctxt =
  let len, ctxt = take_uint32 dtype ctxt in
  let len = Int64.to_int len in
  (* the below call is only to check the length, since we're already aligned. *)
  let ctxt = check_and_align_context ctxt 1 (len + 1) dtype in
  let s = String.sub ctxt.buffer ctxt.offset len in
    for i = 0 to len - 1 do
      if s.[i] = '\x00' then
        raise_error (Invalid_value (dtype, Inv_embedded_nul));
    done;
    if ctxt.buffer.[ctxt.offset + len] <> '\x00' then
      raise_error (Invalid_value (dtype, Inv_not_nul_terminated));
    s, (advance ctxt (len + 1))

let parse_string ctxt =
  let s, ctxt = take_string (T.T_base T.B_string) ctxt in
    V.V_string s, ctxt

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

let parse_object_path ctxt =
  let dtype = T.T_base T.B_object_path in
  let s, ctxt = take_string dtype ctxt in
  let slen = String.length s in
  let prev_was_slash = ref false in
    for i = 0 to slen do
      if not (is_valid_objectpath_char s.[i]) then
        raise_error (Invalid_value (dtype, Inv_objectpath_char));
      if not !prev_was_slash && s.[i] = '/' then
        prev_was_slash := true
      else if !prev_was_slash then
        if s.[i] = '/'
        then raise_error (Invalid_value (dtype, Inv_consecutive_slashes))
        else prev_was_slash := false;
    done;
    if slen > 0 then begin
      if s.[0] <> '/' then
        raise_error (Invalid_value (dtype, Inv_non_slash_prefix));
      if slen > 1 && s.[slen - 1] = '/' then
        raise_error (Invalid_value (dtype, Inv_slash_terminated));
    end;
    V.V_object_path s, ctxt

let parse_signature ctxt =
  let dtype = T.T_base T.B_signature in
  let b, ctxt = take_byte dtype ctxt in
  let slen = Char.code b in
  let s = String.sub ctxt.buffer ctxt.offset slen in
    try V.V_signature (T.signature_of_string s), advance ctxt slen
    with T.Invalid_signature se -> raise_error (Invalid_value (dtype, Inv_signature se))

let parse_double ctxt =
  let dtype = T.T_base T.B_double in
  let align = T.alignment_of dtype in
  let ctxt = check_and_align_context ctxt align 8 dtype in
    (* TODO: Some Oo.black magic, or better yet, do it in C. *)
    V.V_double 0.0, advance ctxt 8

let get_base_parser = function
  | T.B_byte ->         parse_byte
  | T.B_boolean ->      parse_boolean
  | T.B_int16 ->        parse_int16
  | T.B_uint16 ->       parse_uint16
  | T.B_int32 ->        parse_int32
  | T.B_uint32 ->       parse_uint32
  | T.B_int64 ->        parse_int64
  | T.B_uint64 ->       parse_uint64
  | T.B_double ->       parse_double
  | T.B_string ->       parse_string
  | T.B_object_path ->  parse_object_path
  | T.B_signature ->    parse_signature

let rec parse_complete_type dtype ctxt =
  match dtype with
    | T.T_base b ->
        (get_base_parser b) ctxt
    | T.T_variant ->
        let s, ctxt = parse_signature ctxt in
        let tl = V.dtypes_of_signature s in
        let vl, ctxt = parse_type_list tl ctxt in
          V.V_variant (tl, vl), ctxt
    | T.T_array t ->
        let len, ctxt = take_uint32 dtype ctxt in
        let len = Int64.to_int len in
        let align = T.alignment_of t in
        let ctxt = check_and_align_context ctxt align len dtype in
        let end_offset = ctxt.offset + len in
        let rec iter acc ctxt =
          if ctxt.offset < end_offset then
            let e, ctxt = parse_complete_type t ctxt in
              iter (e :: acc) ctxt
          else acc, ctxt in
        let alist, ctxt = iter [] ctxt in
          if ctxt.offset > end_offset then raise_error (Invalid_value (dtype, Inv_array_length))
          else V.V_array (Array.of_list (List.rev alist)), ctxt
    | T.T_struct tl ->
        let align = T.alignment_of dtype in
        (* the below call only performs alignment; the length check
           is performed during the loop. *)
        let ctxt = check_and_align_context ctxt align 0 dtype in
        let vl, ctxt = parse_type_list tl ctxt in
          V.V_array (Array.of_list (List.rev vl)), ctxt

and parse_type_list dtypes ctxt =
  let vl, ctxt =
    List.fold_left (fun (vl, ctxt) t ->
                      let v, ctxt = parse_complete_type t ctxt in
                        (v :: vl), ctxt
                   ) ([], ctxt) dtypes in
    List.rev vl, ctxt
