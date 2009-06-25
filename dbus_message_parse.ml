module T = Dbus_type
module P = Dbus_type_parse
module V = Dbus_value
module M = Dbus_message

type msg_type =
  | Msg_type_method_call
  | Msg_type_method_return
  | Msg_type_signal
  | Msg_type_error

type context =
    {
      mutable type_context : P.context;
      mutable msg_type : msg_type;
      mutable payload_length : int;
      mutable flags : M.flag list;
      mutable protocol_version : char;
      mutable reply_serial : int64;
      mutable hdr_bytes_remaining : int;
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

exception Parse_error of error
let raise_error e =
  raise (Parse_error e)

(* Protocol constants *)
module Protocol = struct
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
  let interface_hdr = 2
  let member_hdr = 3
  let error_name_hdr = 4
  let reply_serial_hdr = 5
  let destination_hdr = 6
  let sender_hdr = 7
  let signature_hdr = 8
end

let init_state () =
  let buffer = String.make Protocol.fixed_header_length '\x00' in
    In_fixed_header (buffer, 0)

let init_context type_context msg_type payload_length flags protocol_version
    reply_serial hdr_bytes_remaining =
  {
    type_context = type_context;
    msg_type = msg_type;
    payload_length = payload_length;
    flags = flags;
    protocol_version = protocol_version;
    reply_serial = reply_serial;
    hdr_bytes_remaining = hdr_bytes_remaining;
    headers = [];
    payload_signature = [];
  }

let parse_flags flags =
  (* TODO *)
  []

let parse_msg_type mtype =
  (* TODO *)
  Msg_type_method_call

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
  let hdr_bytes_remaining, tctxt = P.take_uint32 tctxt in
    (init_context tctxt msg_type (Int64.to_int payload_length) flags protocol_version
       reply_serial (Int64.to_int hdr_bytes_remaining))

let rec parse_substring state str ofs len =
  match state with
    | In_fixed_header (buffer, offset) ->
        let fh_bytes_remaining = Protocol.fixed_header_length - offset in
        let bytes_to_consume = min len fh_bytes_remaining in
          String.blit str ofs buffer offset bytes_to_consume;
          let offset = offset + bytes_to_consume in
            if bytes_to_consume < fh_bytes_remaining
            then Parse_incomplete (In_fixed_header (buffer, offset))
            else begin
              let ctxt = process_fixed_header buffer in
                if ctxt.hdr_bytes_remaining = 0 then
                  Parse_result ((make_message ctxt), (len - bytes_to_consume))
                else
                  (* We need to grow the context by hdrs_length, with
                     0 padding.  This is because the first struct element
                     of the headers array is on a 16-byte boundary, and
                     so there is no padding after the array length.
                  *)
                  let tctxt = (P.grow_context_by ctxt.type_context
                                 ctxt.hdr_bytes_remaining) in
                    ctxt.type_context <- tctxt;
                    (parse_substring (In_headers ctxt)
                       str (ofs + bytes_to_consume) (len - bytes_to_consume))
            end
    | _ -> failwith "failure"
