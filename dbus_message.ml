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

let method_call ?(flags=[]) ~serial ?destination
    ?interface ~path ~member
    ~signature ~payload
    =
  V.type_check_args signature payload;
  Msg_method_call {
    method_call_flags = flags;
    method_call_serial = serial;
    method_call_path = path;
    method_call_member = member;
    method_call_interface = interface;
    method_call_destination = destination;
    method_call_sender = None;
    method_call_signature = signature;
    method_call_payload = payload;
  }

let method_return ?(flags=[]) ~serial ?destination
    ~reply_serial
    ~signature ~payload
    =
  V.type_check_args signature payload;
  Msg_method_return {
    method_return_flags = flags;
    method_return_serial = serial;
    method_return_reply_serial = reply_serial;
    method_return_destination = destination;
    method_return_sender = None;
    method_return_signature = signature;
    method_return_payload = payload;
  }

let error ?(flags=[]) ~serial ?destination
    ~name ~reply_serial
    ~signature ~payload
    =
  V.type_check_args signature payload;
  Msg_error {
    error_flags = flags;
    error_serial = serial;
    error_name = name;
    error_reply_serial = reply_serial;
    error_destination = destination;
    error_sender = None;
    error_signature = signature;
    error_payload = payload;
  }

let signal ?(flags=[]) ~serial ?destination
    ~interface ~path ~member
    ~signature ~payload
    =
  V.type_check_args signature payload;
  Msg_signal {
    signal_flags = flags;
    signal_serial = serial;
    signal_path = path;
    signal_interface = interface;
    signal_member = member;
    signal_destination = destination;
    signal_sender = None;
    signal_signature = signature;
    signal_payload = payload;
  }
