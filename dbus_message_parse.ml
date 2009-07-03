module T = Dbus_type
module P = Dbus_type_parse
module V = Dbus_value
module M = Dbus_message

type msg_type =
  | Msg_type_method_call
  | Msg_type_method_return
  | Msg_type_error
  | Msg_type_signal

type context = {
  mutable type_context : P.context;
  mutable msg_type : msg_type;
  mutable payload_length : int;
  mutable flags : M.flag list;
  mutable protocol_version : char;
  mutable reply_serial : int64;
  mutable bytes_remaining : int;
  mutable headers : (M.header * T.t list * V.t list) list;
  mutable payload_signature : T.t list;
}

type state =
  | In_fixed_header of string * (* offset *) int
  | In_headers of context
  | In_payload of context

type parse_result =
  | Parse_incomplete of state
  | Parse_result of M.t * (* number of unconsumed bytes *) int

type error =
  | Invalid_endian
  | Unknown_msg_type of int

exception Parse_error of error
let raise_error e =
  raise (Parse_error e)

(* Protocol constants *)
module Protocol = struct
  (* the minimum size of a message *)
  let fixed_header_length = 16

  let little_endian = 'l'
  let big_endian = 'B'

  let method_call_msg = 1
  let method_return_msg = 2
  let error_msg = 3
  let signal_msg = 4

  let no_reply_expected_flag = 0x1
  let no_auto_start_flag = 0x2

  let path_hdr = 1
  let path_hdr_type = T.T_base T.B_object_path

  let interface_hdr = 2
  let interface_hdr_type = T.T_base T.B_string

  let member_hdr = 3
  let member_hdr_type = T.T_base T.B_string

  let error_name_hdr = 4
  let error_name_hdr_type = T.T_base T.B_string

  let reply_serial_hdr = 5
  let reply_serial_hdr_type = T.T_base T.B_uint32

  let destination_hdr = 6
  let destination_hdr_type = T.T_base T.B_string

  let sender_hdr = 7
  let sender_hdr_type = T.T_base T.B_string

  let signature_hdr = 8
  let signature_hdr_type = T.T_base T.B_signature
end

let init_state () =
  let buffer = String.make Protocol.fixed_header_length '\x00' in
    In_fixed_header (buffer, 0)

let init_context type_context msg_type payload_length flags protocol_version
    reply_serial bytes_remaining =
  {
    type_context = type_context;
    msg_type = msg_type;
    payload_length = payload_length;
    flags = flags;
    protocol_version = protocol_version;
    reply_serial = reply_serial;
    bytes_remaining = bytes_remaining;
    headers = [];
    payload_signature = [];
  }

let parse_flags flags =
  let flags = Char.code flags in
  let with_no_reply flist =
    if (flags land Protocol.no_reply_expected_flag
        = Protocol.no_reply_expected_flag)
    then M.Msg_flag_no_reply_expected :: flist
    else flist in
  let with_no_auto_start flist =
    if (flags land Protocol.no_auto_start_flag
        = Protocol.no_auto_start_flag)
    then M.Msg_flag_no_auto_start :: flist
    else flist
  in
    with_no_reply (with_no_auto_start [])

let parse_msg_type mtype =
  let mtype = Char.code mtype in
  if mtype = Protocol.method_call_msg
  then Msg_type_method_call
  else if mtype = Protocol.method_return_msg
  then Msg_type_method_return
  else if mtype = Protocol.error_msg
  then Msg_type_error
  else if mtype = Protocol.signal_msg
  then Msg_type_signal
  else raise_error (Unknown_msg_type mtype)

let make_message ctxt =
  (* TODO *)
  M.Msg_method_return { M.method_return_flags = [];
                        M.method_return_serial = 0L;
                        M.method_return_destination = None;
                        M.method_return_sender = None;
                        M.method_return_signature = [];
                        M.method_return_payload = [] }

let process_fixed_header buffer =
  let endian =
    if buffer.[0] = Protocol.little_endian then T.Little_endian
    else if buffer.[0] = Protocol.big_endian then T.Big_endian
    else raise_error Invalid_endian in
  let tctxt = P.init_context endian buffer 1 (Protocol.fixed_header_length - 1) in
  let msg_type, tctxt = P.take_byte tctxt in
  let msg_type = parse_msg_type msg_type in
  let flags, tctxt = P.take_byte tctxt in
  let flags = parse_flags flags in
  let protocol_version, tctxt = P.take_byte tctxt in
  let payload_length, tctxt = P.take_uint32 tctxt in
  let reply_serial, tctxt = P.take_uint32 tctxt in
    (* To set the remaining bytes for the header, we first extract the
       header array length, and position the context just after it.
       There is no padding between the array length and the first
       array struct element, since it starts at byte 16, and hence is
       already 8-aligned. *)
  let bytes_remaining, tctxt = P.take_uint32 tctxt in
    (init_context tctxt msg_type (Int64.to_int payload_length) flags
       protocol_version reply_serial (Int64.to_int bytes_remaining))

let process_headers ctxt =
  (* At this point, the parsing context is set just past the array
     length, but the array parser starts by consuming the length.  So
     we need to rewind the context back to the beginning of the array
     length. *)
  let tctxt = P.rewind ctxt.type_context 4 in
  let hdr_type = T.T_array (T.T_struct [ T.T_base T.B_byte; T.T_variant ]) in
  let hdr_array, tctxt = P.parse_complete_type hdr_type tctxt in
    ctxt.type_context <- tctxt;
    ctxt.bytes_remaining <- ctxt.payload_length;
    ignore (hdr_array)

let rec parse_substring state str ofs len =
  match state with
    | In_fixed_header (buffer, offset) ->
        assert (Protocol.fixed_header_length > offset);
        let fh_bytes_remaining = Protocol.fixed_header_length - offset in
        let bytes_to_consume = min len fh_bytes_remaining in
          String.blit str ofs buffer offset bytes_to_consume;
          let offset = offset + bytes_to_consume in
            if bytes_to_consume < fh_bytes_remaining
            then Parse_incomplete (In_fixed_header (buffer, offset))
            else begin
              let ctxt = process_fixed_header buffer in
                if ctxt.bytes_remaining = 0
                then Parse_result (make_message ctxt, len - bytes_to_consume)
                else (parse_substring (In_headers ctxt)
                        str (ofs + bytes_to_consume) (len - bytes_to_consume))
            end
    | In_headers ctxt ->
        let bytes_to_consume = min len ctxt.bytes_remaining in
        let tctxt = P.append_bytes ctxt.type_context str ofs bytes_to_consume in
          ctxt.type_context <- tctxt;
          ctxt.bytes_remaining <- ctxt.bytes_remaining - bytes_to_consume;
          if ctxt.bytes_remaining = 0 then process_headers ctxt;
          (* We need to check again to see if we expect a payload. *)
          if ctxt.bytes_remaining = 0
          then Parse_result (make_message ctxt, len - bytes_to_consume)
          else (parse_substring (In_payload ctxt)
                  str (ofs + bytes_to_consume) (len - bytes_to_consume))
    | In_payload ctxt ->
        (* TODO *)
        failwith "failure"
