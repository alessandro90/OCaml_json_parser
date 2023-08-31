(*
    TODO:
     * support for unicode characters
     * stringify
     * lookup and insertion
     * Track line number and column number in errors
     * error if json does not end with last }
*)
open Parser

type 'a jarray = 'a list

type 'a jobj = (string * 'a) list

and jvalue =
  | JNull
  | JBool of bool
  | JString of string
  | JNumber of float
  | JArray of jvalue jarray
  | JObject of jvalue jobj

let obj_null = (fun _ -> JNull) <$> null
let obj_bool = (fun b -> JBool b) <$> (true_ <|> false_)
let obj_string = (fun s -> JString s) <$> string_
let obj_number = (fun n -> JNumber n) <$> number

let rec jentry (_ : unit) =
  (fun k v -> (k, v))
  <$> string_
  <*> space *> colon *> space *> defer (fun _ -> jvalue ())

and jvalue (_ : unit) =
  obj_null
  <|> defer (fun _ -> jrecords ())
  <|> obj_string <|> obj_number <|> obj_bool
  <|> defer (fun _ -> jarray ())

and jarray (_ : unit) =
  (fun x -> JArray x)
  <$> lsquare *> space *> sequence (defer (fun _ -> jvalue ())) (pure [])
  <* space <* rsquare

and jrecords (_ : unit) =
  (fun x -> JObject x)
  <$> (space *> lbrace *> space
       *> sequence (defer (fun _ -> jentry ())) (pure [])
      <* space <* rbrace)

let parse = jrecords
