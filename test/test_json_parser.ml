open OUnit2
open Json_parser.Jparser

let check_json (s : string) (expected : jvalue) =
  match parse s with
  | Error _ -> assert_failure "Could not parse object"
  | Ok (obj, s') ->
      assert_equal expected obj;
      assert_string s'

let parser_tests =
  "Json-tests"
  >::: [
         ( "number-test" >:: fun _ ->
           check_json {|{ "x": 10 }  |} @@ JObject [ ("x", JNumber 10.) ] );
         ( "string-test" >:: fun _ ->
           check_json {|{ "key1": "this \"is" }  |}
           @@ JObject [ ("key1", JString "this \"is") ] );
         ( "bool-test" >:: fun _ ->
           check_json {|{ "a b": true }  |} @@ JObject [ ("a b", JBool true) ]
         );
         ( "null-test" >:: fun _ ->
           check_json {|{ "z": null }  |} @@ JObject [ ("z", JNull) ] );
         ( "array-test" >:: fun _ ->
           check_json {|{ "x": [1, 2, 3] }  |}
           @@ JObject [ ("x", JArray [ JNumber 1.; JNumber 2.; JNumber 3. ]) ]
         );
         ( "multiple-objects-test" >:: fun _ ->
           check_json {|{ "x": 1, "y": "hello" }  |}
           @@ JObject [ ("x", JNumber 1.); ("y", JString "hello") ] );
         ( "nested-objects-test" >:: fun _ ->
           check_json
             {|{ "x": { "z": 1, "x": [1, {"k": "wow"}] }, "y": "hello"}|}
           @@ JObject
                [
                  ( "x",
                    JObject
                      [
                        ("z", JNumber 1.);
                        ( "x",
                          JArray
                            [ JNumber 1.; JObject [ ("k", JString "wow") ] ] );
                      ] );
                  ("y", JString "hello");
                ] );
       ]
