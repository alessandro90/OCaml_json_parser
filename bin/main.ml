open Json_parser.Jparser

let () =
  {|{ "x": { "z": 1, "x": [1, {"k": "wow"}] }, "y": "hello"}|} |> parse
  |> Result.fold
       ~ok:(fun t -> to_string @@ fst t)
       ~error:(Fun.const "Could not parse")
  |> print_endline
