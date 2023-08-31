open Json_parser.Parser

let () =
  let s = {|"A
st\"ring"|} in
  match run string_ s with
  | Error _ -> print_endline "Error found"
  | Ok (s', _) -> print_endline s'
