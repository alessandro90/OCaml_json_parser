open OUnit2
include Test_parser

let () = run_test_tt_main ("Tests" >::: [ parser_tests ])
