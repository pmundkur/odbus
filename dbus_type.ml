type base =
  | B_byte          (* 'y' *)
  | B_boolean       (* 'b' *)
  | B_int16         (* 'n' *)
  | B_uint16        (* 'q' *)
  | B_int32         (* 'i' *)
  | B_uint32        (* 'u' *)
  | B_int64         (* 'x' *)
  | B_uint64        (* 't' *)
  | B_double        (* 'd' *)
  | B_string        (* 's' *)
  | B_object_path   (* 'o' *)
  | B_signature     (* 'g' *)

type t =
  | T_base of base
  | T_array of t        (* 'a' *)
  | T_struct of t list  (* 'r', '(' .. ')' *)
  | T_variant           (* 'v' *)

type sig_error =
  | Sig_incomplete
  | Sig_invalid of string
  | Sig_invalid_char of char

let is_basic_type = function
  | T_base _ -> true
  | _        -> false

let is_container_type t = not (is_basic_type t)

exception Signature of sig_error
let raise_sig_error se = raise (Signature se)

let is_valid_dict_entry = function
  | T_struct tlist -> (List.length tlist = 2) && (is_basic_type (List.hd tlist))
  | _ -> false

let rec get_complete_type clist in_array =
  match clist with
    | [] ->
        raise_sig_error Sig_incomplete
    | 'y' :: rem ->
        (T_base B_byte), rem
    | 'b' :: rem ->
        (T_base B_boolean), rem
    | 'n' :: rem ->
        (T_base B_int16), rem
    | 'q' :: rem ->
        (T_base B_uint16), rem
    | 'i' :: rem ->
        (T_base B_int32), rem
    | 'u' :: rem ->
        (T_base B_uint32), rem
    | 'x' :: rem ->
        (T_base B_int64), rem
    | 't' :: rem ->
        (T_base B_uint64), rem
    | 'd' :: rem ->
        (T_base B_double), rem
    | 's' :: rem ->
        (T_base B_string), rem
    | 'o' :: rem ->
        (T_base B_object_path), rem
    | 'g' :: rem ->
        (T_base B_signature), rem
    | 'v' :: rem ->
	T_variant, rem
    | 'a' :: rem ->
	let t, rem = get_complete_type rem true in
	  (T_array t), rem
    | '(' :: rem ->
        get_struct_type rem
    | ('{' as c) :: rem ->
        if not in_array then
          raise_sig_error (Sig_invalid_char c)
        else
          let t, rem = get_struct_type rem in
            if is_valid_dict_entry t
            then (T_array t), rem
            else raise_sig_error (Sig_invalid "dict_entry not in array context")
    | c :: _ ->
        raise_sig_error (Sig_invalid_char c)

and get_struct_type clist =
  let rec helper acc = function
    | [] ->
	raise_sig_error Sig_incomplete
    | ')' :: rem ->
        acc, rem
    | clist ->
        let t, rem = get_complete_type clist false in
          helper (t :: acc) rem in
  let tl, rem = helper [] clist in
    if List.length tl = 0 then raise_sig_error (Sig_invalid "empty struct")
    else (T_struct (List.rev tl)), rem

let string_to_char_list s =
  let rec accum_list l idx =
    if idx = 0 then s.[0] :: l
    else accum_list (s.[idx] :: l) (idx - 1)
  in
    accum_list [] ((String.length s) - 1)

let parse_signature s =
  let rec helper acc = function
    | [] -> acc
    | clist ->
        let t, rem = get_complete_type clist false in
          helper (t :: acc) rem
  in
    if String.length s > 255 then
      raise_sig_error (Sig_invalid "signature exceeds 255 bytes")
    else
      List.rev (helper [] (string_to_char_list s))
