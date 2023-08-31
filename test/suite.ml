open OUnit2

let () =
  run_test_tt_main
    ("Suite"
    >::: [ Test_simple_parser.parser_tests; Test_json_parser.parser_tests ])
