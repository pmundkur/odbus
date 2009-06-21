type error =
  | Insufficient_data of Dbus_type.base
  | Invalid_value of Dbus_type.base

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

let get_padding ctxt alignment =
  alignment - (ctxt.offset mod alignment)

let check_and_align_context ctxt alignment size dtype =
  let padding = get_padding ctxt alignment in
    if ctxt.length < size + padding then
      raise_error (Insufficient_data dtype);
    advance ctxt padding

let parse_byte ctxt =
  if ctxt.length < 1 then raise_error (Insufficient_data Dbus_type.Tbase_byte)
  else ctxt.buffer.[ctxt.offset], advance ctxt 1

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

let parse_i16 sign ctxt =
  let to_fn, dtype =
    if sign then to_int16, Dbus_type.Tbase_int16
    else to_uint16, Dbus_type.Tbase_uint16 in
  let ctxt = check_and_align_context ctxt 2 2 dtype in
  let b0 = Char.code ctxt.buffer.[ctxt.offset] in
  let b1 = Char.code ctxt.buffer.[ctxt.offset + 1] in
    to_fn ctxt.endian b0 b1, advance ctxt 2

let parse_int16 = parse_i16 true
let parse_uint16 = parse_i16 false

let parse_uint32 ?(dtype=Dbus_type.Tbase_uint32) ctxt =
  let ctxt = check_and_align_context ctxt 4 4 dtype in
  let b0 = Char.code ctxt.buffer.[ctxt.offset] in
  let b1 = Char.code ctxt.buffer.[ctxt.offset + 1] in
  let b2 = Char.code ctxt.buffer.[ctxt.offset + 2] in
  let b3 = Char.code ctxt.buffer.[ctxt.offset + 3] in
  let q0 = to_uint16 ctxt.endian b0 b1 in
  let q1 = to_uint16 ctxt.endian b2 b3 in
    to_uint32 ctxt.endian q0 q1, advance ctxt 4

let parse_int32 ctxt =
  let ctxt = check_and_align_context ctxt 4 4 Dbus_type.Tbase_int32 in
  let b0 = Char.code ctxt.buffer.[ctxt.offset] in
  let b1 = Char.code ctxt.buffer.[ctxt.offset + 1] in
  let b2 = Char.code ctxt.buffer.[ctxt.offset + 2] in
  let b3 = Char.code ctxt.buffer.[ctxt.offset + 3] in
  let q0 = to_uint16 ctxt.endian b0 b1 in
  let q1 = to_uint16 ctxt.endian b2 b3 in
    to_int32 ctxt.endian q0 q1, advance ctxt 4

let parse_boolean ctxt =
  let i, ctxt = parse_uint32 ~dtype:Dbus_type.Tbase_boolean ctxt in
    if i <> 0L && i <> 1L then raise_error (Invalid_value Dbus_type.Tbase_boolean)
    else (if i = 0L then false else true), ctxt

(* TODO: check int64 (and other!) sanity! *)
let parse_uint64 ?(dtype=Dbus_type.Tbase_uint64) ctxt =
  let ctxt = check_and_align_context ctxt 8 8 dtype in
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
    to_uint64 ctxt.endian u0 u1

let parse_int64 = parse_uint64

let parse_string ?(dtype=Dbus_type.Tbase_string) ctxt =
  let len, ctxt = parse_uint32 ~dtype ctxt in
  let len = Int64.to_int len in
  let ctxt = check_and_align_context ctxt 1 (len + 1) dtype in
  let s = String.sub ctxt.buffer ctxt.offset len in
    for i = 0 to len - 1 do
      if s.[i] = '\x00' then
        raise_error (Invalid_value dtype);
    done;
    if ctxt.buffer.[ctxt.offset + len] <> '\x00' then
      raise_error (Invalid_value dtype);
    s, (advance ctxt (len + 1))

let parse_object_path ctxt =
  let s, ctxt = parse_string ~dtype:Dbus_type.Tbase_object_path ctxt in
    (* TODO: validate the path:
       . The path must begin with an ASCII '/' (integer 47) character,
         and must consist of elements separated by slash characters.
       . Each element must only contain the ASCII characters
         "[A-Z][a-z][0-9]_"
       . No element may be the empty string.
       . Multiple '/' characters cannot occur in sequence.
       . A trailing '/' character is not allowed unless the path is
         the root path (a single '/' character).
    *)
    s, ctxt

