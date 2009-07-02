type error =
  | Internal_error of string
  | Unexpected_char of char
  | Unrecognized_server_protocol of string

exception Auth_error of error

let raise_error e =
  raise (Auth_error e)

(* Authentication mechanisms *)

type mech_output =
  | Mech_continue of string
  | Mech_ok of string
  | Mech_error

class type mechanism = object
  method init : mech_output
  method challenge : string -> mech_output
end

class external_mech =
  let euid = Printf.sprintf "%d" (Unix.geteuid ()) in
  let euid = Utils.string_to_hex euid in
object
  val mutable response_sent = false
  method init =
    response_sent <- true; Mech_ok euid
  method challenge (data : string) =
    if response_sent then Mech_error
    else (response_sent <- true;
          Mech_continue euid)
end

class anonymous_mech =
object
  val mutable response_sent = false
  method init =
    response_sent <- true; Mech_ok "odbus"
  method challenge (data : string) =
    if response_sent then Mech_error
    else (response_sent <- true;
          Mech_continue "odbus")
end

type supported_mechs =
  | External
  | Anonymous

(* Client and server protocols *)

type client_proto =
  | Client_auth of string * string option
  | Client_cancel
  | Client_begin
  | Client_data of string
  | Client_error of string

let client_proto_to_string = function
  | Client_auth (mech, None) -> Printf.sprintf "AUTH %s" mech
  | Client_auth (mech, Some init_resp) -> Printf.sprintf "AUTH %s %s" mech init_resp
  | Client_cancel -> "CANCEL"
  | Client_begin -> "BEGIN"
  | Client_data data -> Printf.sprintf "DATA %s" data
  | Client_error err -> Printf.sprintf "ERROR %s" err

type server_proto =
  | Server_rejected of string  (* TODO: make this a list of options *)
  | Server_ok of string
  | Server_data of string
  | Server_error
  | Server_other of string

let server_proto_of_string s =
  let slen = String.length s in
  let try_match_cmd cmd =
    let clen = String.length cmd in
      if slen >= clen && String.sub s 0 clen = cmd
      then Some (if slen = clen then "" else String.sub s (clen + 1) (slen - clen - 1))
      else None
  in
    match try_match_cmd "REJECTED" with | Some options -> Server_rejected options | None ->
       (match try_match_cmd "OK" with | Some o -> Server_ok o | None ->
          (match try_match_cmd "DATA" with | Some d -> Server_data d | None ->
             (match try_match_cmd "ERROR" with | Some _ -> Server_error | None ->
                Server_other s)))

(* Protocol context and states *)

type client_state =
  | Waiting_for_data
  | Waiting_for_ok
  | Waiting_for_reject

type cursor =
  | Line of char list
  | Line_CR of char list
  | Line_done of string

type parse_result =
  | Line_incomplete
  | Line_complete of string * (* number of consumed bytes *) int

type auth_state =
  | Auth_in_progress of (* protocol reply *) string
  | Auth_failed
  | Auth_succeeded of (* server address (guid) *) string * (* protocol reply *) string

type client_context = {
  mutable state : client_state;
  mutable cursor : cursor;
  mechanism : mechanism;
}

let init_client_context mech_type =
  let mech_str, mech =
    match mech_type with
      | External -> "EXTERNAL", (new external_mech : mechanism)
      | Anonymous -> "ANONYMOUS", (new anonymous_mech : mechanism) in
  let make_context init_state =
    { state = init_state;
      cursor = Line [];
      mechanism = mech } in
  let make_init_proto init_resp =
    if init_resp = "" then Client_auth (mech_str, None)
    else Client_auth (mech_str, Some init_resp)
  in match mech#init with
    | Mech_continue init_resp ->
        make_init_proto init_resp, make_context Waiting_for_data
    | Mech_ok init_resp ->
        make_init_proto init_resp, make_context Waiting_for_ok
    | Mech_error ->
        (* Mechanisms shouldn't really have errors on initialization! *)
        assert false

let rev_string_of_chars cl =
  let len = List.length cl in
  let s = String.create len in
    ignore (List.fold_left (fun idx c -> s.[idx] <- c; idx - 1) (len - 1) cl);
    s

let parse_char ctxt c =
  match ctxt.cursor with
    | Line cl ->
        (match c with
           | '\r' -> ctxt.cursor <- Line_CR cl
           | _    -> ctxt.cursor <- Line (c :: cl)
        )
    | Line_CR cl ->
        (match c with
           | '\r' -> ctxt.cursor <- Line_CR (c :: cl)
           | '\n' -> ctxt.cursor <- Line_done (rev_string_of_chars cl)
           | _    -> ctxt.cursor <- Line (c :: '\r' :: cl)
        )
    | Line_done _ ->
        raise_error (Internal_error "parse_char called on Line_done!")

let is_done ctxt =
  match ctxt.cursor with
    | Line_done _ -> true
    | _ -> false

let parse_line ctxt s ofs len =
  let i = ref ofs in
  let iend = ofs + len in
    while not (is_done ctxt) && !i < iend do
      parse_char ctxt s.[!i];
      incr i;
    done;
    match ctxt.cursor with
      | Line_done s -> Line_complete (s, !i - ofs)
      | _ -> Line_incomplete

(* Client protocol state machine *)

let send client_proto =
  Printf.sprintf "%s\r\n" (client_proto_to_string client_proto)

let process_server_cmd ctxt s =
  let server_proto = server_proto_of_string s in
  match ctxt.state with
    | Waiting_for_data ->
        (match server_proto with
           | Server_data data ->
               (match ctxt.mechanism#challenge data with
                  | Mech_continue resp ->
                      ctxt.state <- Waiting_for_data;
                      Auth_in_progress (send (Client_data resp))
                  | Mech_ok resp ->
                      ctxt.state <- Waiting_for_ok;
                      Auth_in_progress (send (Client_data resp))
                  | Mech_error ->
                      Auth_in_progress (send (Client_error "Auth mechanism failure"))
               )
           | Server_rejected _ ->
               (* TODO: attempt via a hitherto untried mechanism, before failing *)
               Auth_failed
           | Server_error ->
               ctxt.state <- Waiting_for_reject;
               Auth_in_progress (send Client_cancel)
           | Server_ok server_addr ->
               Auth_succeeded (server_addr, (send Client_begin))
           | Server_other _ ->
               Auth_in_progress (send (Client_error "unrecognized protocol"))
        )
    | Waiting_for_ok ->
        (match server_proto with
           | Server_ok server_addr ->
               Auth_succeeded (server_addr, (send Client_begin))
           | Server_rejected _ ->
               (* TODO: attempt via a hitherto untried mechanism, before failing *)
               Auth_failed
           | Server_error
           | Server_data _ ->
               ctxt.state <- Waiting_for_reject;
               Auth_in_progress (send Client_cancel)
           | Server_other _ ->
               Auth_in_progress (send (Client_error "unrecognized protocol"))
        )
    | Waiting_for_reject ->
        (match server_proto with
           | Server_rejected _ ->
               (* TODO: attempt via a hitherto untried mechanism, before failing *)
               Auth_failed
           | Server_ok _
           | Server_data _
           | Server_error
           | Server_other _ ->
               Auth_failed
        )
