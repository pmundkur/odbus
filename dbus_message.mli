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
  method_call_signature : Dbus_type.t list;
  method_call_payload : Dbus_value.t list;
}

type method_return = {
  method_return_flags : flag list;
  method_return_serial : int64;
  method_return_reply_serial : int64;
  method_return_destination : string option;
  method_return_sender : string option;
  method_return_signature : Dbus_type.t list;
  method_return_payload : Dbus_value.t list;
}

type error = {
  error_flags : flag list;
  error_serial : int64;
  error_name : string;
  error_reply_serial : int64;
  error_destination : string option;
  error_sender : string option;
  error_signature : Dbus_type.t list;
  error_payload : Dbus_value.t list;
}

type signal = {
  signal_flags : flag list;
  signal_serial : int64;
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

val get_flags : t -> flag list
val get_serial : t -> int64
val get_destination : t -> string option
val get_sender : t -> string option
val get_signature : t -> Dbus_type.t list
val get_payload : t -> Dbus_value.t list

val method_call : ?flags:flag list -> serial:int64 -> ?destination:string
  -> ?interface:string -> ?path:string -> member:string
  -> signature: Dbus_type.t list -> payload:Dbus_value.t list
  -> t

val method_return : ?flags:flag list -> serial:int64 -> ?destination:string
  -> reply_serial:int64
  -> signature: Dbus_type.t list -> payload:Dbus_value.t list
  -> t

val error : ?flags:flag list -> serial:int64 -> ?destination:string
  -> name:string -> reply_serial:int64
  -> signature: Dbus_type.t list -> payload:Dbus_value.t list
  -> t

val signal : ?flags:flag list -> serial:int64 -> ?destination:string
  -> interface:string -> path:string -> member:string
  -> signature: Dbus_type.t list -> payload:Dbus_value.t list
  -> t
