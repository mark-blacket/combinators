include Stdlib.Bytes
type elt = char

let take n s = sub s 0 n

let drop n s =
    try sub s n (length s - n) with
        | Invalid_argument _ -> empty

let drop_match prefix s =
    if starts_with ~prefix s
    then
        let len = length prefix in
        try Some (sub s len (length s - len)) with
            | Invalid_argument _ -> None
    else None

let join cs = 
    let buf = Buffer.create 16 in
    let rec inner = function
        | x :: xs -> Buffer.add_char buf x; inner xs
        | [] -> Buffer.to_bytes buf
    in inner cs
