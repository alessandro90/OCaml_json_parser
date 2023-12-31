(*
    TODO:
     * support for unicode characters
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

let defer_ (f : unit -> 'a parser) = defer (fun _ -> f ())
let obj_null = Fun.const JNull <$> null
let obj_bool = (fun b -> JBool b) <$> (true_ <|> false_)
let obj_string = (fun s -> JString s) <$> string_
let obj_number = (fun n -> JNumber n) <$> number

module JValue = struct
  type t = jvalue
end

module JEntry = struct
  type t = string * jvalue
end

let rec jentry (_ : unit) =
  (fun k v -> (k, v)) <$> string_ <*> space *> colon *> space *> defer_ jvalue

and jvalue (_ : unit) =
  obj_null <|> defer_ jrecords <|> obj_string <|> obj_number <|> obj_bool
  <|> defer_ jarray

and jarray (_ : unit) =
  (fun x -> JArray x)
  <$> lsquare *> space *> sequence (module JValue) (defer_ jvalue)
  <* space <* rsquare

and jrecords (_ : unit) =
  (fun x -> JObject x)
  <$> (space *> lbrace *> space *> sequence (module JEntry) (defer_ jentry)
      <* space <* rbrace)

let parse = run (jrecords () <* space)

let rec to_string (jv : jvalue) : string =
  match jv with
  | JNull -> "null"
  | JString s -> s
  | JNumber n -> string_of_float n
  | JBool b -> if b then "true" else "false"
  | JObject obj -> jobject_to_string obj
  | JArray a -> jarray_to_string a

and jobject_to_string (obj : jvalue jobj) : string =
  "{ "
  ^ List.fold_left
      (fun acc (k, v) ->
        acc ^ (if acc <> "" then ", \"" else "\"") ^ k ^ "\": " ^ to_string v)
      "" obj
  ^ " }"

and jarray_to_string (a : jvalue jarray) : string =
  "[ "
  ^ List.fold_left
      (fun acc v ->
        match acc with "" -> to_string v | _ -> acc ^ ", " ^ to_string v)
      "" a
  ^ " ]"
