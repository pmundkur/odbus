type base =
  | Tbase_byte          (* 'y' *)
  | Tbase_boolean       (* 'b' *)
  | Tbase_int16         (* 'n' *)
  | Tbase_uint16        (* 'q' *)
  | Tbase_int32         (* 'i' *)
  | Tbase_uint32        (* 'u' *)
  | Tbase_int64         (* 'x' *)
  | Tbase_uint64        (* 't' *)
  | Tbase_double        (* 'd' *)
  | Tbase_string        (* 's' *)
  | Tbase_object_path   (* 'o' *)
  | Tbase_signature     (* 'g' *)

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
        (T_base Tbase_byte), rem
    | 'b' :: rem ->
        (T_base Tbase_boolean), rem
    | 'n' :: rem ->
        (T_base Tbase_int16), rem
    | 'q' :: rem ->
        (T_base Tbase_uint16), rem
    | 'i' :: rem ->
        (T_base Tbase_int32), rem
    | 'u' :: rem ->
        (T_base Tbase_uint32), rem
    | 'x' :: rem ->
        (T_base Tbase_int64), rem
    | 't' :: rem ->
        (T_base Tbase_uint64), rem
    | 'd' :: rem ->
        (T_base Tbase_double), rem
    | 's' :: rem ->
        (T_base Tbase_string), rem
    | 'o' :: rem ->
        (T_base Tbase_object_path), rem
    | 'g' :: rem ->
        (T_base Tbase_signature), rem
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
