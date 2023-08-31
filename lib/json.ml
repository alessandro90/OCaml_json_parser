(*
    TODO:
     * support for unicode characters
     * stringify
     * lookup and insertion
     * Track line number and column number in errors
     * tests
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

(* let rec jentry (_ : unit) = *)
(*   let* k = string_ in *)
(*   let* _ = space in *)
(*   let* _ = colon in *)
(*   let* _ = space in *)
(*   let* v = jvalue () in *)
(*   Parser (fun s -> Ok ((k, v), s)) *)

let rec jentry (_ : unit) =
  (fun k v -> (k, v)) <$> string_ <*> space *> colon *> space *> jvalue ()

and jvalue (_ : unit) =
  obj_null <|> jrecords () <|> obj_string <|> obj_number <|> obj_bool
  <|> jarray ()

(* and jarray (_ : unit) = *)
(*   let* _ = lsquare in *)
(*   let* _ = space in *)
(*   let* values = sequence (jvalue ()) (pure []) in *)
(*   let* _ = space in *)
(*   let* _ = rsquare in *)
(*   pure (JArray values) *)
and jarray (_ : unit) =
  (fun x -> JArray x)
  <$> lsquare *> space *> sequence (jvalue ()) (pure [])
  <* space <* rsquare

and jrecords (_ : unit) =
  (fun x -> JObject x)
  <$> (space *> lbrace *> space *> sequence (jentry ()) (pure [])
      <* space <* rbrace)
(* and jrecords (_ : unit) = *)
(*   let* _ = space in *)
(*   let* _ = lbrace in *)
(*   let* _ = space in *)
(*   let* records = sequence (jentry ()) (pure []) in *)
(*   let* _ = space in *)
(*   let* _ = rbrace in *)
(*   pure (JObject records) *)

let parse = run @@ jrecords ()
