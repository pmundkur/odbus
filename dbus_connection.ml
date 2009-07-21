module M = Dbus_message
module A = Dbus_auth
module C = Async_conn
module P = Dbus_message_parse
module MM = Dbus_message_marshal

type error =
  | Authentication_failed

exception Error of error
let raise_error e =
  raise (Error e)

type conn_state = {
  (* recv state *)
  read_offset : int;
  parse_state : P.state;

  (* send state *)
  write_offset : int;
}

type state =
  | Connecting
  | Authenticating of A.client_context
  | Connected of conn_state
  | Disconnected

type t = {
  conn : C.t;
  mutable callbacks : callbacks;
  mutable state : state;
  mutable server_address : string;
}

and callbacks = {
  authenticated_callback : t -> unit;
  msg_received_callback : t -> M.t -> unit;
  error_callback : t -> unit;
  send_done_callback : t -> unit;
}

module Conns = Connection_table.Make(struct type conn = t end)

let init_conn_state =
  {
    read_offset = 0;
    parse_state = P.init_state 0;
    write_offset = 0;
  }

let connect_callback aconn =
  let conn = Conns.get_conn (C.get_handle aconn) in
  let sender = C.send aconn in
  let auth_context, auth_state = A.init_client_context A.External sender in
    assert (conn.state = Connecting);
    sender "\x00";
    match auth_state with
      | A.Auth_in_progress ->
          conn.state <- Authenticating auth_context
      | A.Auth_failed ->
          raise_error Authentication_failed
      | A.Auth_succeeded _ ->
          conn.state <- Connected init_conn_state

let send_done_callback aconn =
  let conn = Conns.get_conn (C.get_handle aconn) in
    conn.callbacks.send_done_callback conn

let recv_callback aconn s ofs len =
  let conn = Conns.get_conn (C.get_handle aconn) in
    (* Since we invoke the msg_received_callback, we need to check for
       the disconnected state at the start of each recursion. *)
  let rec receiver s ofs len =
    match conn.state with
      | Connecting ->
          (* We should have transitioned out of this state on the
             connect_callback. *)
          assert false
      | Authenticating auth_ctxt ->
          let sender = C.send aconn in
            (match A.parse_input auth_ctxt sender s ofs len with
               | A.Auth_in_progress ->
                   ()
               | A.Auth_failed ->
                   raise_error Authentication_failed
               | A.Auth_succeeded (addr, consumed) ->
                   conn.server_address <- addr;
                   conn.state <- Connected init_conn_state;
                   receiver s (ofs + consumed) (len - consumed)
            )
      | Connected cs ->
          (match P.parse_substring cs.parse_state s ofs len with
             | P.Parse_incomplete s ->
                 let cs = { cs with
                              read_offset = cs.read_offset + len;
                              parse_state = s;
                          } in
                   conn.state <- Connected cs
             | P.Parse_result (m, remaining) ->
                 let read_offset = cs.read_offset + len - remaining in
                 let cs = { cs with
                              read_offset = read_offset;
                              parse_state = P.init_state read_offset;
                          } in
                   conn.state <- Connected cs;
                   conn.callbacks.msg_received_callback conn m;
                   receiver s (ofs + len - remaining) remaining
          )
      | Disconnected ->
          (* End the recursion. *)
          ()
  in
    receiver s ofs len

let disconnect conn =
  (* TODO *)
  conn.state <- Disconnected

(*
let send conn msg =
  let offset = conn.state.write_offset in
  let marshaled_size = MM.compute_marshaled_size offset m in
  let buf_len = offset + marshaled_size in
  let buffer = String.make buf_len '\000' in
  let state_update = { conn.state with
                         offset = (offset + marshaled_size) mod 8
                     } in
    MM.marshal_message ~offset T.Little_Endian buffer buf_len msg;
    conn.state <- state_update;
*)
