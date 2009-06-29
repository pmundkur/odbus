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

type method_call =
    {
      method_call_flags : flag list;
      method_call_path : string;
      method_call_member : string;
      method_call_interface : string;
      method_call_destination : string option;
      method_call_sender : string option;
      method_call_signature : Dbus_type.t list;
      method_call_payload : Dbus_value.t list;
      method_call_headers : (header * Dbus_value.t list) list;
    }

type method_return =
    {
      method_return_flags : flag list;
      method_return_serial : int64;
      method_return_destination : string option;
      method_return_sender : string option;
      method_return_signature : Dbus_type.t list;
      method_return_payload : Dbus_value.t list;
    }

type error =
    {
      error_flags : flag list;
      error_serial : int64;
      error_name : string;
      error_destination : string option;
      error_sender : string option;
      error_signature : Dbus_type.t list;
      error_payload : Dbus_value.t list;
    }

type signal =
    {
      signal_flags : flag list;
      signal_path : string;
      signal_interface : string;
      signal_member : string;
      signal_destination : string option;
      signal_sender : string option;
      signal_signature : Dbus_type.t list;
      signal_payload : Dbus_value.t list;
    }

type t =
  | Msg_method_call of method_call
  | Msg_method_return of method_return
  | Msg_error of error
  | Msg_signal of signal
