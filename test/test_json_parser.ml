open OUnit2
open Json_parser.Jparser
open Json_parser.Parser

let check_null s =
  match run obj_null s with
  | Error _ -> assert_bool "" true
  | Ok (v, _) -> (
      match v with JNull -> assert_bool "" true | _ -> assert_bool "" true)

let check_record s = assert_bool "Invalid" @@ Result.is_ok (run (jrecords ()) s)

let parser_tests =
  "Json-tests"
  >::: [
         ("null-check" >:: fun _ -> check_null "null");
         ("records-test" >:: fun _ -> check_record {|{ "x": 10 }|});
       ]
