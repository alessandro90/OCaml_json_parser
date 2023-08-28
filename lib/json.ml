open Parser

type 'a json_array = 'a list

module JsonMap = Map.Make (String)

let merge_maps m1 m2 = JsonMap.union (fun _ _ v2 -> Some v2) m1 m2

type 'a obj = 'a JsonMap.t

and value =
  | Null
  | Bool of bool
  | Str of string
  | Number of float
  | Array of value json_array
  | Entry of { k : string; v : value }
  | Object of value obj

let obj_null = (fun _ -> Null) <$> null
let obj_bool = (fun b -> Bool b) <$> (true_ <|> false_)
let obj_string = (fun s -> Str s) <$> string_
let obj_number = (fun n -> Number n) <$> number
let obj_pure (v : value) = Parser (fun s -> Ok (v, s))
let empty_pure = obj_pure (Object JsonMap.empty)

let rec obj_entry _ =
  let* _ = lbrace in
  let* key = string_ in
  let* _ = colon in
  let* v = obj_value () in
  let* _ = rbrace in
  let* _ = comma in
  obj_pure (Entry { k = key; v })

and obj_value (_ : unit) =
  obj_null <|> obj_records () <|> obj_string <|> obj_number <|> obj_bool
  <|> obj_array () <* comma

and obj_records (_ : unit) =
  some empty_pure (obj_entry ()) (fun entry map ->
      match (entry, map) with
      | Entry e, Object m -> Object (JsonMap.add e.k e.v m)
      | _, _ -> raise (Invalid_argument "Not map entry"))

and obj_array (_ : unit) =
  let* _ = lsquare in
  let* values =
    many (obj_pure (Array []))
      (obj_value () <* comma)
      (fun v arr ->
        match arr with
        | Array a -> Array (a @ [ v ])
        | _ -> raise (Invalid_argument "Not an array"))
  in
  let* _ = rsquare in
  let* _ = comma in
  obj_pure values
