type error =
  | ParseErrorEof
  | ParseErrorCharNotFound of string
  | ParseErrorNotANumber of string

type 'a parser = Parser of (string -> ('a * string, error) result)

let run (Parser p) = p
let pure x = Parser (fun s -> Ok (x, s))
let fail x = Parser (fun _ -> Error x)
let not_found s = "Could not find " ^ s

let alt (p : 'a parser) (p1 : 'b parser) =
  Parser
    (fun (s : string) ->
      let res = run p s in
      Result.fold ~ok:(Fun.const res) ~error:(fun _ -> run p1 s) res)

let ( <|> ) = alt
let mtuple f (v, str) = (f v, str)
let fmap f p = Parser (fun s -> Result.map (mtuple f) (run p s))
let ( <$> ) = fmap
let ( let+ ) = fmap

let bind (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
  Parser
    (fun s ->
      Result.join @@ Result.map (fun (v, str) -> run (f v) str) @@ run p s)

let ( >>= ) = bind
let ( let* ) = bind
let apply (fp : ('a -> 'b) parser) (p : 'a parser) = fp >>= fun x -> x <$> p
let ( <*> ) = apply
let skip (p : 'a parser) (p1 : 'b parser) = (fun _ x -> x) <$> p <*> p1
let rskip (p : 'a parser) (p1 : 'b parser) = (fun x _ -> x) <$> p <*> p1
let ( *> ) = skip
let ( <* ) = rskip
let optional p = ignore <$> p <|> pure ()
let defer (f : unit -> 'a parser) = Parser (fun s -> run (f ()) s)
let prepend_char c s = String.make 1 c ^ s
let append_char c s = s ^ String.make 1 c

module type Empty = sig
  type t

  val empty : t
end

module Any =
functor
  (S : Empty)
  ->
  struct
    let rec some (p : 'a parser) (cons : 'a -> S.t -> S.t) : S.t parser =
      cons <$> p <*> defer (fun _ -> many p cons)

    and many (p : 'a parser) (cons : 'a -> S.t -> S.t) : S.t parser =
      some p cons <|> pure S.empty
  end

module AnyString = Any (struct
  type t = string

  let empty = ""
end)

module AnyUnit = Any (struct
  type t = unit

  let empty = ()
end)

module EmptyList (S : sig
  type t
end) : Empty with type t = S.t list = struct
  type t = S.t list

  let empty = []
end

let char_ =
  Parser
    (fun s ->
      let len = String.length s in
      if len = 0 then Error ParseErrorEof
      else Ok (s.[0], String.sub s 1 (len - 1)))

let satisfy (pred : char -> bool) (msg_err : string) : char parser =
  char_ >>= fun c ->
  if pred c then pure c else fail (ParseErrorCharNotFound msg_err)

let rec satisfy_many_aux (preds : (char -> bool) list) (acc : string)
    (s : string) (target : string) =
  match preds with
  | [] -> Ok (acc, s)
  | h :: t ->
      Result.join
      @@ Result.map (fun (c, str) ->
             satisfy_many_aux t (append_char c acc) str target)
      @@ run (satisfy h @@ not_found target) s

let satisfy_many (preds : (char -> bool) list) (target : string) =
  Parser (fun s -> satisfy_many_aux preds "" s target)

let exact_str s =
  let str = s |> String.to_seq |> List.of_seq in
  satisfy_many (List.map (fun c -> ( = ) c) str) s

let is_digit c = match c with '0' .. '9' -> true | _ -> false

let normal_char =
  satisfy (fun c -> c <> '"' && c <> '\\') @@ not_found "quote or backslash"

let comma = satisfy (( = ) ',') @@ not_found ","

let newline =
  (optional @@ satisfy (( = ) '\r') @@ not_found "\"r")
  *> (satisfy (( = ) '\n') @@ not_found "\"n")

let lbrace = satisfy (( = ) '{') @@ not_found "{"
let rbrace = satisfy (( = ) '}') @@ not_found "}"
let lsquare = satisfy (( = ) '[') @@ not_found "["
let rsquare = satisfy (( = ) ']') @@ not_found "]"
let colon = satisfy (( = ) ':') @@ not_found ":"

let space =
  AnyUnit.many
    (pure ()
    <* (satisfy (( = ) ' ') @@ not_found "white space"
       <|> newline
       <|> satisfy (( = ) '\t') @@ not_found "tab"))
    (fun _ _ -> ())

let true_ = Fun.const true <$> exact_str "true"
let false_ = Fun.const false <$> exact_str "false"
let null = exact_str "null"
let quote = satisfy (( = ) '"') @@ not_found "\""
let dot = satisfy (( = ) '.') @@ not_found "."
let digit = satisfy is_digit @@ not_found "digit"

let escape =
  exact_str {|\"|} *> pure '"'
  <|> exact_str {|\n|} *> pure '\n'
  <|> exact_str {|\t|} *> pure '\t'
  <|> exact_str {|\r|} *> pure '\r'
  <|> exact_str {|\b|} *> pure '\b'
  <|> exact_str {|\\|} *> pure '\\'

let string_ =
  quote *> AnyString.many (normal_char <|> escape) prepend_char <* quote

module type Type = sig
  type t
end

let sequence (type a) (module M : Type with type t = a) (p : a parser) :
    a list parser =
  let module EmptyM = EmptyList (M) in
  let module AsAny = Any (EmptyM) in
  let sep = space *> comma *> space in
  List.cons <$> space *> p
  <*> (AsAny.many (sep *> p) List.cons <|> pure EmptyM.empty)

let number =
  let* num =
    (fun s ->
      float_of_string_opt s |> Option.to_result ~none:(ParseErrorNotANumber s))
    <$> AnyString.some
          (digit <|> dot
          <|> satisfy (fun x -> x = '-' || x = 'e' || x = 'E')
              @@ not_found "'-' or 'e' or 'E'")
          prepend_char
  in
  match num with Error e -> fail e | Ok v -> pure v
