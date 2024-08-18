open OUnit2

module Parser = Combinators.Make(Combinators.String)
open Parser.O

let input = "aabbcc"
let test printer p exp _ = assert_equal ~printer exp (p >>| input)
let returns_char = test [%show: string * (char, string) result]
let returns_string = test [%show: string * (string, string) result]
let returns_string_pair = test [%show: string * ((string * string), string) result]
let returns_string_list = test [%show: string * (string list, string) result]

let () = let open Parser in "tests" >:::
    [ "return" >:: returns_string
        (return "a") 
        (input, Ok "a")
    ; "fail" >:: returns_string
        (fail "err")
        (input, Error "err")
    ; "elt_if" >:: returns_char
        (elt_if (fun c -> c != 'b'))
        ("abbcc", Ok 'a')
    ; "elt_if_err" >:: returns_char
        (elt_if (fun c -> c = 'b'))
        (input, Error "Element not matched")
    ; "elt" >:: returns_char
        (elt 'a')
        ("abbcc", Ok 'a')
    ; "elt_err" >:: returns_char
        (elt 'b')
        (input, Error "Element not matched")
    ; "exact" >:: returns_string
        (exact "aa")
        ("bbcc", Ok "aa")
    ; "exact_err" >:: returns_string
        (exact "ab")
        (input, Error "Input not matched")
    ; "bind" >:: returns_char
        (bind (elt 'a') (fun x -> elt x))
        ("bbcc", Ok 'a') 
    ; "bind_err" >:: returns_char
        (bind (elt 'b') (fun x -> elt x))
        (input, Error "Element not matched")
    ; "map" >:: returns_char
        (map (elt 'a') (fun x -> Char.uppercase_ascii x))
        ("abbcc", Ok 'A')
    ; "map_err" >:: returns_char
        (map (elt 'b') (fun x -> Char.uppercase_ascii x))
        (input, Error "Element not matched")
    ; "forward" >:: returns_char
        (forward (exact "aa") (elt 'a'))
        ("bbcc", Ok 'a')
    ; "forward_err_l" >:: returns_char
        (forward (exact "ab") (elt 'a'))
        (input, Error "Input not matched")
    ; "forward_err_r" >:: returns_char
        (forward (exact "aa") (elt 'b'))
        ("bbcc", Error "Element not matched")
    ; "left" >:: returns_string
        (left (exact "aa") (exact "bb"))
        ("cc", Ok "aa")
    ; "left_err_l" >:: returns_string
        (left (exact "ab") (exact "bb"))
        (input, Error "Input not matched")
    ; "left_err_r" >:: returns_string
        (left (exact "aa") (exact "ab"))
        (input, Error "Input not matched")
    ; "right" >:: returns_string
        (right (exact "aa") (exact "bb"))
        ("cc", Ok "bb")
    ; "right_err_l" >:: returns_string
        (right (exact "ab") (exact "bb"))
        (input, Error "Input not matched")
    ; "right_err_r" >:: returns_string
        (right (exact "aa") (exact "ab"))
        (input, Error "Input not matched")
    ; "both" >:: returns_string_pair
        (both (exact "aa") (exact ""))
        ("bbcc", Ok ("aa", ""))
    ; "both_err_l" >:: returns_string_pair
        (both (exact "ab") (exact "bb"))
        (input, Error "Input not matched")
    ; "both_err_r" >:: returns_string_pair
        (both (exact "aa") (exact "ab"))
        (input, Error "Input not matched")
    ; "any_ok_l" >:: returns_string
        (any (exact "aa") (exact "bb"))
        ("bbcc", Ok "aa")
    ; "any_ok_r" >:: returns_string
        (any (exact "bb") (exact "aa"))
        ("bbcc", Ok "aa")
    ; "any_err" >:: returns_string
        (any (exact "bb") (exact "cc"))
        (input, Error "Input not matched")
    ; "opt" >:: returns_string
        (opt (exact "bb") "bb")
        (input, Ok "bb")
    ; "map2" >:: returns_string
        (map2 (exact "aa") (exact "bb") ~f:String.cat)
        ("cc", Ok "aabb")
    ; "map3" >:: returns_string
        (map3 (exact "aa") (exact "") (exact "bb") ~f:(fun x y z -> x ^ y ^ z))
        ("cc", Ok "aabb")
    ; "repeat0" >:: returns_string_list
        (repeat0 (exact "a"))
        ("bbcc", Ok ["a"; "a"])
    ; "repeat0_empty" >:: returns_string_list
        (repeat0 (exact "b"))
        (input, Ok [])
    ; "repeat1" >:: returns_string_list
        (repeat1 (exact "a"))
        ("bbcc", Ok ["a"; "a"])
    ; "repeat1_empty" >:: returns_string_list
        (repeat1 (exact "b"))
        (input, Error "No matches")
    ; "foreach" >:: returns_string_list
        (foreach (repeat1 (exact "a")) (map (exact "a") (String.cat "b")))
        ("bbcc", Ok ["ba"; "ba"])
    ; "foreach_err_l" >:: returns_string_list
        (foreach (repeat1 (exact "b")) (map (exact "a") (String.cat "b")))
        (input, Error "No matches")
    ; "foreach_err_r" >:: returns_string_list
        (foreach (repeat1 (exact "a")) (map (exact "b") (String.cat "b")))
        (input, Error "Input not matched")
    ; "take_all" >:: returns_string
        (take_all)
        ("", Ok input)
    ; "take_while" >:: returns_string
        (take_while (fun c -> c = 'a'))
        ("bbcc", Ok "aa")
    ; "take_while_err" >:: returns_string
        (take_while (fun c -> c = 'b'))
        (input, Error "No matches")
    ; "take_until" >:: returns_string
        (take_until (fun c -> c = 'b'))
        ("bbcc", Ok "aa")
    ; "take_until_err" >:: returns_string
        (take_until (fun c -> c = 'a'))
        (input, Error "No matches")
    ] |> run_test_tt_main
