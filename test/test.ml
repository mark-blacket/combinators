open OUnit2

module Parser = Combinators.Make(Combinators.String)
open Parser.O

let input = "aa|bb|cc"
let str_test p exp = 
    let printer = [%show: string * (string, string) result] in
    (fun _ -> assert_equal ~printer exp (p >>| input))
let char_test p exp = 
    let printer = [%show: string * (char, string) result] in
    (fun _ -> assert_equal ~printer exp (p >>| input))
let str_pair_test p exp = 
    let printer = [%show: string * ((string * string), string) result] in
    (fun _ -> assert_equal ~printer exp (p >>| input))

let () = let open Parser in "tests" >:::
    [ "return" >:: str_test
        (return "a") 
        (input, Ok "a")
    ; "fail" >:: str_test
        (fail "err")
        (input, Error "err")
    ; "elt_if" >:: char_test
        (elt_if (fun c -> c != 'b'))
        ("a|bb|cc", Ok 'a')
    ; "elt_if_err" >:: char_test
        (elt_if (fun c -> c = 'b'))
        (input, Error "Element not matched")
    ; "elt" >:: char_test
        (elt 'a')
        ("a|bb|cc", Ok 'a')
    ; "elt_err" >:: char_test
        (elt 'b')
        (input, Error "Element not matched")
    ; "exact" >:: str_test
        (exact "aa")
        ("|bb|cc", Ok "aa")
    ; "exact_err" >:: str_test
        (exact "ab")
        (input, Error "Input not matched")
    ; "bind" >:: char_test
        (bind (elt 'a') (fun x -> elt x))
        ("|bb|cc", Ok 'a') 
    ; "bind_err" >:: char_test
        (bind (elt 'b') (fun x -> elt x))
        (input, Error "Element not matched")
    ; "map" >:: char_test
        (map (elt 'a') (fun x -> Char.uppercase_ascii x))
        ("a|bb|cc", Ok 'A')
    ; "map_err" >:: char_test
        (map (elt 'b') (fun x -> Char.uppercase_ascii x))
        (input, Error "Element not matched")
    ; "forward" >:: char_test
        (forward (exact "aa") (elt 'a'))
        ("|bb|cc", Ok 'a')
    ; "forward_err_l" >:: char_test
        (forward (exact "ab") (elt 'a'))
        (input, Error "Input not matched")
    ; "forward_err_r" >:: char_test
        (forward (exact "aa") (elt 'b'))
        ("|bb|cc", Error "Element not matched")
    ; "left" >:: str_test
        (left (exact "aa") (exact "|bb|"))
        ("cc", Ok "aa")
    ; "left_err_l" >:: str_test
        (left (exact "ab") (exact "|bb|"))
        (input, Error "Input not matched")
    ; "left_err_r" >:: str_test
        (left (exact "aa") (exact "|ab|"))
        (input, Error "Input not matched")
    ; "right" >:: str_test
        (right (exact "aa") (exact "|bb|"))
        ("cc", Ok "|bb|")
    ; "right_err_l" >:: str_test
        (right (exact "ab") (exact "|bb|"))
        (input, Error "Input not matched")
    ; "right_err_r" >:: str_test
        (right (exact "aa") (exact "|ab|"))
        (input, Error "Input not matched")
    ; "both" >:: str_pair_test
        (both (exact "aa") (exact "|"))
        ("bb|cc", Ok ("aa", "|"))
    ; "both_err_l" >:: str_pair_test
        (both (exact "ab") (exact "|bb|"))
        (input, Error "Input not matched")
    ; "both_err_r" >:: str_pair_test
        (both (exact "aa") (exact "|ab|"))
        (input, Error "Input not matched")
    ; "any_ok_l" >:: str_test
        (any (exact "aa") (exact "bb"))
        ("|bb|cc", Ok "aa")
    ; "any_ok_r" >:: str_test
        (any (exact "bb") (exact "aa"))
        ("|bb|cc", Ok "aa")
    ; "any_err" >:: str_test
        (any (exact "bb") (exact "cc"))
        (input, Error "Input not matched")
    ; "opt" >:: str_test
        (opt (exact "bb") "bb")
        (input, Ok "bb")
    ] |> run_test_tt_main
