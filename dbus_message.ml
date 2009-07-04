(* The signature of the header is:
   BYTE, BYTE, BYTE, BYTE, UINT32, UINT32, ARRAY of STRUCT of (BYTE,VARIANT)
   where the array of headers has a UINT32 length followed immediately
   (i.e. without padding) by the headers since the struct is already
   aligned on a 8-byte boundary.
*)

module T = Dbus_type
module V = Dbus_value

type flag =
  | Msg_flag_no_reply_expected
  | Msg_flag_no_auto_start

type header =
  | Hdr_path
  | Hdr_interface
  | Hdr_member
  | Hdr_error_name
  | Hdr_reply_serial
  | Hdr_destination
  | Hdr_sender
  | Hdr_signature

type method_call = {
  method_call_flags : flag list;
  method_call_serial : int64;
  method_call_path : string;
  method_call_member : string;
  method_call_interface : string option;
  method_call_destination : string option;
  method_call_sender : string option;
  method_call_signature : T.t list;
  method_call_payload : V.t list;
}

type method_return = {
  method_return_flags : flag list;
  method_return_serial : int64;
  method_return_reply_serial : int64;
  method_return_destination : string option;
  method_return_sender : string option;
  method_return_signature : T.t list;
  method_return_payload : V.t list;
}

type error = {
  error_flags : flag list;
  error_serial : int64;
  error_name : string;
  error_reply_serial : int64;
  error_destination : string option;
  error_sender : string option;
  error_signature : T.t list;
  error_payload : V.t list;
}

type signal = {
  signal_flags : flag list;
  signal_serial : int64;
  signal_path : string;
  signal_interface : string;
  signal_member : string;
  signal_destination : string option;
  signal_sender : string option;
  signal_signature : T.t list;
  signal_payload : V.t list;
}

type t =
  | Msg_method_call of method_call
  | Msg_method_return of method_return
  | Msg_error of error
  | Msg_signal of signal
