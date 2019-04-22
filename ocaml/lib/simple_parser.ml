open Common
open Common_module_types
open Parse_combinators



module Position:
sig
  type t
  val line: t -> int
  val column: t -> int
  val start: t
  val next: char -> t -> t
end =
  struct
    type t = {line:int; column:int}
    let line (p:t): int = p.line
    let column (p:t): int = p.column
    let start: t = {line = 0; column = 0}
    let next_column (p:t): t = {p with column = p.column + 1}
    let next_line   (p:t): t = {line = p.line + 1; column = 0}
    let next (c:char) (p:t): t =
      if c = '\n' then
        next_line p
      else
        next_column p
  end





module Simple_parser (F:ANY):
sig
  type final = F.t
  type token = char option
  type error = string list
  type parser
  type 'a res = ('a,error) result

  include Monad.MONAD

  val succeed: 'a -> 'a t
  val fail: error -> 'a t
  val consumer: 'a t -> 'a t
  val (>>-): 'a t -> ('a -> 'b t) -> 'b t
  val backtrackable: 'a t -> 'a t
  val commit: 'a -> 'a t
  val (>>|): 'a t -> (error -> 'a t) -> 'a t
  val expect: (char -> 'a res) -> string -> 'a t

  val expect_end: final -> final t
  val char: char -> unit t
  val one_of_chars: string -> unit t
  val space: unit t
  val string: string -> unit t
  val letter: char t
  val optional: 'a t -> 'a option t
  val one_or_more: 'a t -> 'a list t
  val zero_or_more: 'a t -> 'a list t
  val skip_one_or_more: 'a t -> int t
  val skip_zero_or_more: 'a t -> int t
  val zero_or_more_separated: 'a t -> _ t -> 'a list t
  val one_or_more_separated: 'a t -> _ t -> 'a list t
  val one_of: 'a t list -> 'a t
  val (<|>): 'a t -> 'a t -> 'a t

  val run: final t -> string -> parser

  val needs_more: parser -> bool
  val has_ended:  parser -> bool
  val result: parser -> final res
  val line:   parser -> int
  val column: parser -> int
  val lookahead: parser -> token list
end =
  struct
    module Token =
      struct
        type t = char option
      end

    module Error =
      struct
        type t = string list
      end

    module P = Basic_parser.Basic (Token) (Position) (Error) (F)
    include  P

    module C = Combinators (P)
    include C

    let position =
      state
    let expect_base =
      expect
    let line (p:parser): int =
      Position.line (position p)
    let column (p:parser): int =
      Position.column (position p)

    let expect (f:char -> 'a res) (e:string): 'a t =
      expect_base
        (fun st t ->
          match t with
          | None ->
             Error [e], st
          | Some c ->
             let r = f c in
             match r with
              | Ok _ ->
                 r, Position.next c st
              | Error _ ->
                 r, st)

    let expect_end (a:final): final t =
      expect_base
        (fun st t ->
           match t with
           | None ->
              Ok a, st
           | Some _ ->
              Error ["end"], st)

    let char (c:char): unit t =
      let estr () = "'" ^ String.one c ^ "'"
      in
      expect
        (fun d ->
          if c = d then
            Ok ()
          else
            Error [estr ()])
        (estr ())

    let one_of_chars (str:string): unit t =
      let errstr = "one of \"" ^ str ^ "\"" in
      expect
        (fun c ->
          let idx = String.find (fun d -> c = d) 0 str
          and len = String.length str in
          if idx < len then
            Ok ()
          else
            Error [errstr])
        errstr


    let space: unit t =
      char ' '

    let string (str:string): unit t =
      let len = String.length str in
      let rec parse i =
        if i = len then
          succeed ()
        else
          char str.[i] >>= fun _ -> parse (i+1)
      in
      parse 0

    let letter: char t =
      expect
        (fun c ->
          if Char.is_letter c then
            Ok c
          else
            Error ["letter"])
        "letter"

    let one_of (ps:'a t list): 'a t =
      one_of ps List.concat

    let (<|>) (p:'a t) (q:'a t): 'a t =
      one_of [p; q]

    let run (p:final t) (s:string): parser =
      let p = ref (parser Position.start p) in
      let i = ref 0
      and len = String.length s in
      while !i <> len && needs_more !p do
        p := put_token !p (Some s.[!i]);
        i := !i + 1
      done;
      if needs_more !p then
        p := put_token !p None;
      !p
  end


(* ********** *)
(* Unit Tests *)
(* ********** *)


module CP = Simple_parser (Char)
module UP = Simple_parser (Unit)
module IP = Simple_parser (Int)

let%test _ =
  let module P = Simple_parser (Char) in
  let p = P.(run letter "a") in
  P.(has_ended p
     && result p = Ok 'a'
     && column p = 1
     && lookahead p = [])



let%test _ =
  let module P = Simple_parser (Char) in
  let p = P.(run letter "1") in
  P.(has_ended p
     && result p = Error ["letter"]
     && column p = 0
     && lookahead p = [Some '1'])


let%test _ =
  let module P = Simple_parser (Unit) in
  let p = P.(run (char 'a') "z") in
  P.(has_ended p
     && result p = Error ["'a'"]
     && column p = 0
     && lookahead p = [Some 'z'])



let%test _ =
  let module P = Simple_parser (Unit) in
  let p = P.(run (char 'a') "a") in
  P.(has_ended p
     && result p = Ok ()
     && column p = 1
     && lookahead p = [])




(* Test the [>>-] combinator *)
(* ************************* *)
let%test _ =
  let open UP in
  let p = run (char 'a' >>= fun _ -> char 'b') "ab" in
  has_ended p
  && result p = Ok ()
  && column p = 2
  && lookahead p = []



(* Test the [>>-] combinator *)
(* ************************* *)

let%test _ =
  let open UP in
  let p =
    run (char 'a' >>- fun _ -> char 'b') "ab"
  in
  has_ended p
  && result p = Ok ()
  && column p = 2
  && lookahead p = []


let%test _ =
  let open UP in
  let p =
    run (char 'a' >>- fun _ -> char 'b') "ac"
  in
  has_ended p
  && column p = 0
  && lookahead p = [Some 'a'; Some 'c']



(* Test [optional] *)
(* *************** *)
let%test _ =
  let open UP in
  let p =
    run (map (fun _ -> ()) (char 'a' |> optional)) "a"
  in
  has_ended p
  && column p = 1
  && lookahead p = []


let%test _ =
  let open UP in
  let p =
    run (map (fun _ -> ()) (char 'a' |> optional)) "b"
  in
  has_ended p
  && column p = 0
  && lookahead p = [Some 'b']






(* Test nested parenthesis *)
(* *********************** *)

let parens: unit UP.t =
  let open UP in
  let rec pars (): unit t =
    (consumer (char '(') >>= pars
     >>= fun _ ->
     char ')' >>= pars)
    <|> succeed ()
  in
  pars ()

let nesting: int IP.t =
  let open IP in
  let rec pars (): int t =
    (consumer (char '(') >>= pars
     >>= fun n ->
     char ')' >>= pars >>= fun m -> succeed (max (n+1) m))
    <|> succeed 0
  in
  pars ()

let%test _ =
  let open UP in
  let p = run parens "(())()"
  in
  has_ended p
  && column p = 6
  && lookahead p = [None]

let%test _ =
  let open UP in
  let p = run parens "(())("
  in
  has_ended p
  && column p = 5
  && result p = Error ["')'"]
  && lookahead p = [None]

let%test _ =
  let open UP in
  let p = run parens ")"
  in
  has_ended p
  && column p = 0
  && result p = Ok ()
  && lookahead p = [Some ')']


let%test _ =
  let open IP in
  let p = run nesting "(())()"
  in
  has_ended p
  && result p = Ok 2
  && lookahead p = [None]


let%test _ =
  let open IP in
  let p = run nesting "(()(()))"
  in
  has_ended p
  && result p = Ok 3
  && lookahead p = [None]


(* String parser *)
(* ************* *)

let%test _ =
  let open UP in
  let p = run (string "abcd") "abcd"
  in
  has_ended p
  && column p = 4
  && result p = Ok ()
  && lookahead p = []

let%test _ =
  let open UP in
  let p = run (string "(a)" <|> string "(b)") "(b)"
  in
  has_ended p
  && column p = 1
  && result p = Error ["'a'"]
  && lookahead p = [Some 'b']




(* Backtrackable *)
(* ************* *)

let%test _ =
  let open UP in
  let p =
    run
      (backtrackable (string "(a)"))
      "(a"
  in
  has_ended p
  && column p = 0
  && result p = Error ["')'"]
  && lookahead p = [Some '('; Some 'a'; None]


let%test _ =
  let open UP in
  let p =
    run
      (backtrackable (string "(a)")
       <|> string "(b)")
      "(b)"
  in
  has_ended p
  && column p = 3
  && result p = Ok ()
  && lookahead p = []


let%test _ =
  let open UP in
  let p =
    run
      (backtrackable (string "(a)")
       <|> string "(b)")
      "(a)"
  in
  has_ended p
  && column p = 3
  && result p = Ok ()
  && lookahead p = []





(* Sentences and Error Messages *)
(* **************************** *)
module String_list =
  struct
    type t = string list
  end

module SLP = Simple_parser (String_list)

let word: string SLP.t =
  let open SLP in
  one_or_more letter >>= fun l -> succeed (String.of_list l)

let separator: int SLP.t =
  let open SLP in
  skip_one_or_more (space <|> char ',')

let sentence: string list SLP.t =
  let open SLP in
  one_or_more_separated word separator >>= fun lst ->
  one_of_chars ".?!" >>= fun _ ->
  succeed lst


let%test _ =
  let open SLP in
  let p = run sentence "hi,di,hi." in
  result p = Ok ["hi"; "di"; "hi"]

let%test _ =
  let open SLP in
  let p = run sentence "hi,,di hi!" in
  result p = Ok ["hi"; "di"; "hi"]

let%test _ =
  let open SLP in
  let p = run sentence "hi       di        hi?" in
  result p = Ok ["hi"; "di"; "hi"]


let%test _ =
  let open SLP in
  let p = run sentence "hi,123" in
  has_ended p
  && result p = Error ["letter"]
  && column p = 3
