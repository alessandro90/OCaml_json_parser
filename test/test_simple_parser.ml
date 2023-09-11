open OUnit2
open Json_parser.Parser

let str_from_char = String.make 1
let success = assert_bool "" true

let print_list (printer : 'a -> string) =
  List.fold_left (fun acc item -> acc ^ printer item ^ ", ") ""

let skip_first_char s =
  if String.length s = 1 then "" else String.sub s 1 (String.length s - 1)

let print_ch_str (ch, str) = str_from_char ch ^ "; " ^ str

let check_char s =
  match run char_ s with
  | Error _ -> assert_failure "Invalid char"
  | Ok (c', s') ->
      assert_equal ~printer:print_ch_str (c', s') (s.[0], skip_first_char s)

let check_char_empty =
  match run char_ "" with
  | Error _ -> success
  | Ok (c', s') ->
      assert_failure @@ "Expected failure, got" ^ str_from_char c' ^ ", " ^ s'

let check_satisfy_ok c s =
  match run (satisfy (( = ) c) @@ not_found @@ String.make 1 c) s with
  | Error _ -> assert_failure @@ Printf.sprintf "Could not find the char: %c" c
  | Ok (c', s') ->
      assert_equal ~printer:print_ch_str (c', s') (s.[0], skip_first_char s)

let check_satisfy_fail c s =
  match run (satisfy (( = ) c) @@ not_found @@ String.make 1 c) s with
  | Error _ -> success
  | Ok (c', s') ->
      assert_failure @@ "Expected failure, got" ^ str_from_char c' ^ ", " ^ s'

let check_newline s expected_str =
  match run newline s with
  | Error _ -> assert_failure "Could not find newline"
  | Ok (_, s') -> assert_equal s' expected_str

let check_string s expected_str =
  match run string_ s with
  | Error _ -> assert_failure "Could not match expected string"
  | Ok (s', _) -> assert_equal ~printer:Fun.id s' expected_str

let check_exact_string s expected_str =
  match run (exact_str expected_str) s with
  | Error _ -> assert_failure @@ "Could not match string: " ^ expected_str
  | Ok (s', _) -> assert_equal ~printer:Fun.id s' expected_str

let check_exact_string_fail s expected_str =
  match run (exact_str expected_str) s with
  | Error _ -> success
  | Ok (s', _) -> assert_failure @@ "Invalid match: " ^ s'

let check_sequence_list_bool (s : string) (expected_seq : bool list) =
  match run (sequence (module Bool) (true_ <|> false_)) s with
  | Error _ -> assert_failure "Could not match sequence"
  | Ok (ls, _) ->
      assert_equal ~printer:(print_list string_of_bool) expected_seq ls

let check_sequence_list_bool_fail (s : string) =
  match run (sequence (module Bool) (true_ <|> false_)) s with
  | Error _ -> success
  | Ok (ls, _) ->
      assert_failure @@ "Matched an invalid list: "
      ^ print_list string_of_bool ls

let check_number (s : string) (expected_n : float) =
  match run number s with
  | Error _ -> assert_failure "Could not match number"
  | Ok (n, _) ->
      assert_equal ~printer:string_of_float ~cmp:cmp_float expected_n n

let check_number_fail (s : string) =
  match run number s with
  | Error _ -> success
  | Ok (n, _) -> assert_failure @@ "Invalid number parsed: " ^ string_of_float n

let parser_tests =
  "Parser-tests"
  >::: [
         ("char-find" >:: fun _ -> check_char "something");
         ("char-empty" >:: fun _ -> check_char_empty);
         ("satisfy-ok-ch:a" >:: fun _ -> check_satisfy_ok 'a' "also");
         ("satisfy-ok-ch:quote" >:: fun _ -> check_satisfy_ok '\"' "\"also");
         ("satisfy-fail-ch:a" >:: fun _ -> check_satisfy_fail 'a' "lso");
         ("newline-lf" >:: fun _ -> check_newline "\nsomething" "something");
         ("newline-crlf" >:: fun _ -> check_newline "\r\nsomething" "something");
         ("check-string" >:: fun _ -> check_string "\"this\" absd" "this");
         ( "check-string" >:: fun _ ->
           check_string "\"this\nis\nescaped\" and something else"
             "this\nis\nescaped" );
         ( "check-exact-string" >:: fun _ ->
           check_exact_string "nullplus something" "null" );
         ( "check-exact-string-fail" >:: fun _ ->
           check_exact_string_fail ".nullplus something" "null" );
         ( "check_sequence" >:: fun _ ->
           check_sequence_list_bool "  true, false    ,true,true"
             [ true; false; true; true ] );
         ( "check_sequence-fail" >:: fun _ ->
           check_sequence_list_bool_fail ", false    ,true,true" );
         ("check-int" >:: fun _ -> check_number "125" 125.);
         ("check-float" >:: fun _ -> check_number "125.2" 125.2);
         ("check-neg" >:: fun _ -> check_number "-125.2" (-125.2));
         ("check-exp" >:: fun _ -> check_number "1e2" 1.0e2);
         ("check-Exp" >:: fun _ -> check_number "1E2" 1.0e2);
         ("check-num-fail" >:: fun _ -> check_number_fail "a1");
         ("check-num-fail" >:: fun _ -> check_number_fail "");
         ("check-num-fail" >:: fun _ -> check_number_fail "1ee3");
       ]
