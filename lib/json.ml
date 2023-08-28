(* TODO: support for escaped and unicode characters *)
open Parser

type 'a json_array = 'a list

module JsonMap = Map.Make (String)

type 'a obj = (string * 'a) list

and value =
  | Null
  | Bool of bool
  | Str of string
  | Number of float
  | Array of value json_array
  | Object of value obj

let trim =
  let* _ = space in
  let* _ = comma in
  pure ()

let obj_null = (fun _ -> Null) <$> null <* trim
let obj_bool = (fun b -> Bool b) <$> (true_ <|> false_) <* trim
let obj_string = (fun s -> Str s) <$> string_ <* trim
let obj_number = (fun n -> Number n) <$> number <* trim

let rec obj_entry (_ : unit) =
  let* _ = space in
  let* k = string_ in
  let* _ = space in
  let* _ = colon in
  let* _ = space in
  let* v = obj_value () in
  Parser (fun s -> Ok ((k, v), s))

and obj_value (_ : unit) =
  space
  *> (obj_null <|> obj_records () <|> obj_string <|> obj_number <|> obj_bool
    <|> obj_array ())

and obj_array (_ : unit) =
  let* _ = space in
  let* _ = lsquare in
  let* _ = space in
  let* values = sequence (obj_value ()) (pure []) in
  let* _ = space in
  let* _ = rsquare in
  pure (Array values)

and obj_records (_ : unit) =
  let* _ = space in
  let* _ = lbrace in
  let* _ = space in
  let* records = sequence (obj_entry ()) (pure []) in
  let* _ = space in
  let* _ = rbrace in
  pure (Object records)

let parse = obj_records ()
