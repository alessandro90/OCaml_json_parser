type error =
  | SyntaxError
  | NotFoundError of char
  | EOSError
  | CustomError of string
  | CharMismatchError of char
  | NotANumberError

type 'a parser = Parser of (string -> ('a * string, error) result)

module type Pure = sig
  type t

  val pure : t parser
end

let run (Parser p) = p
let pure x = Parser (fun s -> Ok (x, s))
let fail x = Parser (fun _ -> Error x)

let alt (p : 'a parser) (p1 : 'b parser) =
  Parser
    (fun (s : string) ->
      let res = run p s in
      match res with Error _ -> run p1 s | _ -> res)

let ( <|> ) = alt
let mtuple f (v, str) = (f v, str)
let fmap f p = Parser (fun s -> Result.map (mtuple f) (run p s))
let ( <$> ) = fmap
let ( let+ ) = fmap

let bind (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
  Parser
    (fun s ->
      match run p s with Error e -> Error e | Ok (v, str) -> run (f v) str)

let ( >>= ) = bind
let ( let* ) = bind

let apply (fp : ('a -> 'b) parser) (p : 'a parser) =
  let* f = fp in
  f <$> p

let ( <*> ) = apply
let skip (p : 'a parser) (p1 : 'b parser) = (fun _ x -> x) <$> p <*> p1
let rskip (p : 'a parser) (p1 : 'b parser) = (fun x _ -> x) <$> p <*> p1
let ( *> ) = skip
let ( <* ) = rskip
let id = Parser (fun s -> Ok ((), s))
let optional p = ignore <$> p <|> id
let defer (f : unit -> 'a parser) = Parser (fun s -> run (f ()) s)
let append_char c s = s ^ String.make 1 c

let rec some (null_case : 'b parser) (p : 'a parser) (cons : 'a -> 'b -> 'b) :
    'b parser =
  cons <$> p <*> defer (fun _ -> many null_case p cons)

and many (null_case : 'b parser) (p : 'a parser) (cons : 'a -> 'b -> 'b) =
  some null_case p cons <|> null_case

(* Utility parsers *)
let char_ =
  Parser
    (fun s ->
      let len = String.length s in
      if len = 0 then Error EOSError else Ok (s.[0], String.sub s 1 (len - 1)))

let satisfy (pred : char -> bool) : char parser =
  char_ >>= fun c -> if pred c then pure c else fail (CharMismatchError c)

let rec satisfy_many_aux (preds : (char -> bool) list) (acc : string)
    (s : string) =
  match preds with
  | [] -> Ok (acc, s)
  | h :: t -> (
      match run (satisfy h) s with
      | Error e -> Error e
      | Ok (c, str) -> satisfy_many_aux t (append_char c acc) str)

let satisfy_many (preds : (char -> bool) list) =
  Parser (fun s -> satisfy_many_aux preds "" s)

let sequence s =
  let str = s |> String.to_seq |> List.of_seq in
  satisfy_many (List.map (fun c -> ( = ) c) str)

let is_digit c = match c with '0' .. '9' -> true | _ -> false
let comma = satisfy (( = ) ',')
let newline = (satisfy (( = ) '\r') |> optional) *> satisfy (( = ) '\n')
let lbrace = satisfy (( = ) '{')
let rbrace = satisfy (( = ) '}')
let lsquare = satisfy (( = ) '[')
let rsquare = satisfy (( = ) ']')
let colon = satisfy (( = ) ':')
let space = satisfy (( = ) ' ')
let true_ = (fun _ -> true) <$> sequence "true"
let false_ = (fun _ -> false) <$> sequence "false"
let null = sequence "null"
let quote = satisfy (( = ) '"')
let dot = satisfy (( = ) '.')
let digit = satisfy is_digit
let string_ = quote *> many (pure "") char_ append_char <* quote

let number =
  let* num =
    (fun s -> float_of_string_opt s |> Option.to_result ~none:NotANumberError)
    <$> some (pure "") (digit <|> dot) append_char
  in
  match num with Error e -> fail e | Ok v -> pure v
