open Json_parser.Jparser

let () =
  let text = {|{ "x": { "z": 1, "x": [1, {"k": "wow"}] }, "y": "hello"}|} in
  let parsed = parse text in
  let as_string = Result.map (fun (jv, s) -> (to_string jv, s)) parsed in
  print_endline
  @@ match as_string with Error _ -> "Could not parse" | Ok (s, _) -> s
