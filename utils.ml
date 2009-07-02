let string_as_list s =
  let len = String.length s in
  let rec loop acc i =
    if i < 0 then acc else loop (s.[i] :: acc) (i - 1)
  in loop [] (len - 1)

let string_to_hex s =
  let clist = string_as_list s in
  let hexlist = List.map (fun c ->  Printf.sprintf "%0X" (Char.code c)) clist
  in String.concat "" hexlist
