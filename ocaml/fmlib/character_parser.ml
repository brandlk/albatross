open Common
open Common_module_types

module Position:
sig
  type t
  val line: t -> int
  val column: t -> int
  val start: t
  val next: char -> t -> t
  val next_line: t -> t
  val next_column: t -> t
end =
  struct
    type t = {line:int; column:int}

    let line (p:t): int = p.line

    let column (p:t): int = p.column

    let start: t =
      {line = 0; column = 0}

    let next_column (p:t): t =
      {p with column = p.column + 1}

    let next_line (p:t): t =
      {line = p.line + 1; column = 0;}

    let next (c:char) (p:t): t =
      if c = '\n' then
        next_line p
      else
        next_column p
  end


module type CONTEXT =
  sig
    type t
    type msg
    val message: t -> msg
    val position: t -> Position.t
    val line: t -> int
    val column: t -> int
  end


module Located =
  struct
    type 'a t =
      {start:Position.t; value:'a; end_:Position.t}

    let make start value end_ =
      {start;value;end_}

    let map (f:'a -> 'b) (l:'a t): 'b t =
      {l with value = f l.value}

    let use (l:'a t) (f: Position.t -> 'a -> Position.t -> 'b): 'b =
      f l.start l.value l.end_

    let value (l:'a t): 'a =
      l.value

    let start (l:'a t): Position.t = l.start

    let end_ (l:'a t) = l.end_

    let range (l:'a t): Position.t * Position.t =
      l.start, l.end_
  end

module Indent =
  struct

    type t = {
        lb: int;          (* lower bound of the indentation set *)
        ub: int option;   (* upper bound of the indentation set *)
        abs: bool;        (* absolute alignment *)
        strict: bool;     (* default token indentation
                               true:  token indentation >  parent
                               false: token indentation >= parent *)
      }

    let initial: t = {lb = 0;
                      ub = None;
                      abs = false;
                      strict = false}


    let bounds (ind:t): int * int option =
      ind.lb, ind.ub


    let is_allowed_token_position (pos:int) (ind:t): bool =
      if ind.abs then
        (* The token position must be in the set of the allowed indentations
           of the parent. *)
        ind.lb <= pos
        && match ind.ub with
           | None ->
              true
           | Some ub ->
              pos <= ub
      else
        (* The token must be strictly or nonstrictly indented relative to the
           parent. *)
        let incr = if ind.strict then 1 else 0 in
        ind.lb + incr <= pos


    let is_offside (col:int) (ind:t): bool =
      not (is_allowed_token_position col ind)


    let token (pos:int) (ind:t): t =
      if ind.abs then
        (* It is the first token of an absolutely aligned parent. *)
        {ind with lb = pos; ub = Some pos; abs = false}
      else
        (* Indentation of the parent is at most the indentation of the token
           (strict = false) or the indentation of the token - 1 (strict =
           true). *)
        let pos = if ind.strict then pos - 1 else pos
        in
        match ind.ub with
        | Some ub when ub <= pos ->
           ind
        | _ ->
           {ind with ub = Some pos}

    let absolute (ind:t): t =
      {ind with abs = true}

    let start_indented (strict:bool) (ind:t): t =
      if ind.abs then
        ind
      else
        let incr = if strict then 1 else 0
        in
        {ind with lb = ind.lb + incr; ub = None}

    let end_indented (strict:bool) (ind0:t) (ind:t): t =
      if ind0.abs then
        ind
      else
        match ind.ub with
        | None ->
           ind0
        | Some ub ->
           let incr = if strict then 1 else 0
           in
           assert (incr <= ub);
           {ind0 with
             ub =
               match ind0.ub with
               | None ->
                  Some (ub - 1)
               | Some ub0 ->
                  Some (min ub0 (ub - 1))}
  end




module Context (Msg:ANY) =
  struct
    type msg = Msg.t
    type t = {
        pos: Position.t;
        msg: Msg.t;
      }

    let make pos msg: t = {pos; msg}

    let message (c:t) = c.msg

    let position (c:t) = c.pos

    let line (c:t) = Position.line c.pos

    let column (c:t) = Position.column c.pos

  end




module State (User:ANY) (Context_msg:ANY) =
  struct
    module Context = Context (Context_msg)

    type context = Context.t

    type t = {
        pos: Position.t;
        indent: Indent.t;
        user: User.t;
        contexts: context list
      }

    let make pos user = {pos;user; indent = Indent.initial; contexts = []}

    let position (s:t): Position.t = s.pos
    let line (s:t): int = Position.line s.pos
    let column (s:t): int = Position.column s.pos

    let next (c:char) (s:t): t =
      {s with
        pos = Position.next c s.pos;
        indent = Indent.token (Position.column s.pos) s.indent}

    let bounds (s:t): int * int option =
      Indent.bounds s.indent

    let is_offside (s:t): bool =
      Indent.is_offside (Position.column s.pos) s.indent


    let absolute (s:t): t =
      {s with indent = Indent.absolute s.indent}

    let start_detached (s:t): t =
      {s with indent = Indent.initial}

    let end_detached (s0:t) (s:t): t =
      {s with indent = s0.indent}

    let start_indented (strict:bool) (s:t): t =
      {s with indent = Indent.start_indented strict s.indent}

    let end_indented (strict:bool) (s0:t) (s:t): t =
      {s with indent = Indent.end_indented strict s0.indent s.indent}

    let user (s:t): User.t = s.user
    let update (f:User.t->User.t) (s:t) =
      {s with user = f s.user}

    let push_context (msg:Context_msg.t) (s:t): t =
      {s with contexts = Context.make s.pos msg :: s.contexts}

    let pop_context (s:t): t =
      match s.contexts with
      | [] ->
         assert false (* Illegal call *)
      | _ :: contexts ->
         {s with contexts}
  end






module type PARSER =
  sig
    type parser
    val needs_more: parser -> bool
    val has_ended:  parser -> bool
    val position:   parser -> Position.t
    val line:   parser -> int
    val column: parser -> int
    val put_char: parser -> char -> parser
    val put_end: parser -> parser
  end





module type COMBINATORS =
  sig
    include Generic_parser.COMBINATORS

    val get_position: (Position.t) t
    val located: 'a t -> 'a Located.t t

    type state
    val get_state: state t
    val update: (state -> state) -> unit t

    val absolute: 'a t -> 'a t
    val indented: bool -> 'a t -> 'a t
    val detached: 'a t -> 'a t
    val get_bounds: (int * int option) t

    val one_or_more_aligned:  'a t -> 'a list t
    val zero_or_more_aligned: 'a t -> 'a list t
    val skip_one_or_more_aligned:  'a t -> int t
    val skip_zero_or_more_aligned: 'a t -> int t

    type context
    val in_context: context -> 'a t -> 'a t

  end







module Advanced
         (User:ANY)
         (Final:ANY)
         (Expect:ANY)
         (Semantic:ANY)
         (Context_msg:ANY) =
  struct
    type state   = User.t
    type context = Context_msg.t

    module Token =
      struct
        type t = char option
      end

    module Context = Context (Context_msg)

    module State = State (User) (Context_msg)

    module Basic =
      Generic_parser.Make (Token) (State) (Expect) (Semantic) (Final)
    include  Basic

    let state (p:parser): User.t =
      State.user (Basic.state p)

    let position (p:parser): Position.t =
      State.position (Basic.state p)

    let line (p:parser): int =
      State.line (Basic.state p)

    let column (p:parser): int =
      State.column (Basic.state p)

    let get_state: User.t t =
      Basic.get >>= fun st ->
      return (State.user st)

    let update (f:User.t -> User.t): unit t =
      Basic.update (State.update f)

    let get_position: (Position.t) t =
      Basic.get >>= fun st ->
      return (State.position st)

    let fail (e: Semantic.t): 'a t =
      Basic.fail e

    let token
          (f: State.t -> char -> ('a,Expect.t) result)
          (e: State.t -> Expect.t) (* generate expectation error in case there
                                      is no character or offside *)
        : 'a t =
      Basic.token
        (fun st t ->
          match t with
          | None ->
             Error (e st)
          | Some c ->
             if State.is_offside st then
               Error (e st)
             else
               match f st c with
               | Ok a ->
                  Ok (a, State.next c st)
               | Error e ->
                  Error e)


    let backtrackable (p: 'a t) (e: Expect.t): 'a t =
      Basic.backtrackable p e



    (* Character Combinators *)

    let expect (p:char -> bool) (e: Expect.t): char t =
      token
        (fun _ c ->
          if p c then
            Ok c
          else
            Error e)
        (fun _ -> e)


    let expect_end (e: Expect.t): unit t =
      Basic.token
        (fun st t ->
          match t with
          | None ->
             Ok ((), st)
          | Some _ ->
             Error e)


    let char (c:char) (e: Expect.t): unit t =
      token
        (fun _ d ->
          if c = d then
            Ok ()
          else
            Error e)
        (fun _ -> e)


    let one_of_chars (str:string) (e: Expect.t): unit t =
      token
        (fun _ c ->
          if String.find (fun d -> c = d) 0 str = String.length str then
            Error e
          else
            Ok ())
        (fun _ -> e)


    let space (e: Expect.t): unit t =
      char ' ' e

    let string (str:string) (msg:int -> Expect.t): unit t =
      let len = String.length str in
      let rec parse i =
        if i = len then
          return ()
        else
          char str.[i] (msg i) >>= fun _ -> parse (i+1)
      in
      parse 0


    let word
          (start: char -> bool)
          (inner: char -> bool)
          (e: Expect.t)
        : string t
      =
      let module Arr = Segmented_array in
      let rec rest arr =
        (expect inner e >>= fun c ->
         rest (Arr.push c arr))
        <|> return arr
      in
      expect start e >>= fun c ->
      map Arr.to_string (rest (Arr.singleton c))


    let whitespace_char (e: Expect.t): char t =
      expect (fun c -> c = ' ' || c = '\n' || c = '\t') e

    let whitespace (e: Expect.t): int t =
      skip_zero_or_more (map (fun _ -> () ) (whitespace_char e))


    let letter (e: Expect.t): char t =
      expect Char.is_letter e

    let digit (e: Expect.t): char t =
      expect Char.is_digit e


    (* Context *)

    let in_context (msg:Context_msg.t) (p:'a t): 'a t =
      Basic.update (State.push_context msg) >>= fun _ ->
      p >>= fun a ->
      Basic.update State.pop_context >>= fun _ ->
      return a

    (* Located *)
    let located (p:'a t): 'a Located.t t =
      Basic.get >>= fun st1 ->
      p >>= fun a ->
      Basic.get >>= fun st2 ->
      return @@ Located.make (State.position st1) a (State.position st2)




    (* Indentation combinators *)


    let absolute (p:'a t): 'a t =
      Basic.update State.absolute >>= fun _ ->
      p

    let indented (strict:bool) (p:'a t): 'a t =
      Basic.get_and_update (State.start_indented strict) >>= fun st ->
      p >>= fun a ->
      Basic.update (State.end_indented strict st) >>= fun _ ->
      return a

    let detached (p:'a t): 'a t =
      Basic.get_and_update State.start_detached >>= fun st ->
      p >>= fun a ->
      Basic.update (State.end_detached st) >>= fun _ ->
      return a

    let get_bounds: (int * int option) t =
      map State.bounds Basic.get


    let one_or_more_aligned (p:'a t): 'a list t =
      absolute (one_or_more (absolute p))

    let zero_or_more_aligned (p:'a t): 'a list t =
      absolute (zero_or_more (absolute p))

    let skip_one_or_more_aligned (p:'a t): int t =
      absolute (skip_one_or_more (absolute p))

    let skip_zero_or_more_aligned (p:'a t): int t =
      absolute (skip_zero_or_more (absolute p))


    (* General functions *)

    let put_char (p:parser) (c:char): parser =
      assert (needs_more p);
      Basic.put_token p (Some c)

    let put_end (p:parser): parser =
      assert (needs_more p);
      Basic.put_token p None

    let make (p:final t) (user:User.t): parser =
      Basic.make_parser (State.make Position.start user) p

    let run (pc:final t) (user:User.t) (s:string): parser =
      let p = ref (make pc user) in
      let i = ref 0
      and len = String.length s in
      while !i <> len && needs_more !p do
        p := put_char !p s.[!i];
        i := !i + 1
      done;
      if needs_more !p then
        p := put_end !p;
      !p

    let lookahead_string (p:parser): string =
      assert (has_ended p);
      "["
      ^ String.concat
          "; "
          (List.map
             (fun o ->
               match o with
               | None ->
                  "None"
               | Some c ->
                  "Some " ^ "'" ^ String.one c ^ "'")
             (lookahead p))
      ^
        "]"

    let result_string
          (p:parser) (f:final -> string): string =
      assert (has_ended p);
      match result p with
      | Some a ->
         "Some " ^ f a
      | None ->
         "None"
  end







module Simple (Final:ANY) =
  struct
    module Advanced = Advanced (Unit) (Final) (String) (String) (String)
    include Advanced

    let expect_end: unit t =
      Advanced.expect_end "end"

    let char (c:char): unit t =
      Advanced.char c @@ "'" ^ String.one c ^ "'"

    let one_of_chars (str:string) (msg:string) : unit t =
      Advanced.one_of_chars str msg

    let space: unit t =
      Advanced.space "space"

    let string (str:string): unit t =
      Advanced.string str (fun i -> "'" ^ String.one str.[i] ^ "'")

    let whitespace_char: char t =
      Advanced.whitespace_char "whitespace"

    let whitespace: int t =
      Advanced.whitespace "whitespace"

    let letter: char t =
      Advanced.letter "letter"

    let digit: char t =
      Advanced.digit "digit"

    let make (p:final t): parser =
      Advanced.make p ()

    let run (pc:final t) (s:string): parser =
      Advanced.run pc () s

    let result_string (p:parser) (f:final->string): string =
      Advanced.result_string p f
  end



(* ********** *)
(* Unit Tests *)
(* ********** *)
module Simple_test (F:ANY) =
  struct
    include Simple (F)
    let one_expect (str:string): Error.t =
      Error.make_expectations [str]
  end
module CP = Simple_test (Char)
module UP = Simple_test (Unit)
module IP = Simple_test (Int)
module SP = Simple_test (String)

let%test _ =
  let open CP in
  let p = run letter "a" in
  has_ended p
  && result p = Some 'a'
  && column p = 1
  && lookahead p = []


let%test _ =
  let open CP in
  let p = run (return identity
               |= letter
               |. expect_end)
            "a"
  in
  has_ended p
  && result p = Some 'a'
  && column p = 1
  && lookahead p = []

module Ctx = Context (String)


let%test _ =
  let open CP in
  let p = run letter "1" in
  has_ended p
  && result p = None
  && error p = Error.make_expectations ["letter"]
  && column p = 0
  && lookahead p = [Some '1']

let%test _ =
  let open UP in
  let p = run (char 'a') "z" in
  has_ended p
  && result p = None
  && error p = one_expect "'a'"
  && column p = 0
  && lookahead p = [Some 'z']

let%test _ =
  let open UP in
  let p = run (char 'a' |. expect_end) "ab"
  in
  has_ended p
  && result p = None
  && error p = one_expect "end"
  && column p = 1
  && lookahead p = [Some 'b']

let%test _ =
  let open UP in
  let p = run (char 'a') "a" in
  has_ended p
  && result p = Some ()
  && column p = 1
  && lookahead p = []

let%test _ =
  let open UP in
  let p = run (char 'a' |. char 'b' |. expect_end) "ab"
  in
  has_ended p
  && result p = Some ()
  && column p = 2
  && lookahead p = []

let%test _ =
  let open UP in
  let p = run (char 'a' |. char 'b')
            "a"
  in
  has_ended p
  && result p = None
  && error p = one_expect "'b'"
  && column p = 1
  && lookahead p = [None]



let%test _ =
  let open UP in
  let p = run (char 'a' >>= fun _ -> char 'b') "ab" in
  has_ended p
  && result p = Some ()
  && column p = 2
  && lookahead p = []



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
    <|> return ()
  in
  pars ()

let nesting: int IP.t =
  let open IP in
  let rec pars (): int t =
    (consumer (char '(')
     >>= pars
     >>= fun n ->
     char ')'
     >>= pars
     >>= fun m -> return (max (n+1) m))
    <|> return 0
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
  && result p = None
  && error p = Error.make_expectations ["'('"; "')'"]
  && lookahead p = [None]

let%test _ =
  let open UP in
  let p = run parens ")"
  in
  has_ended p
  && column p = 0
  && result p = Some ()
  && lookahead p = [Some ')']


let%test _ =
  let open IP in
  let p = run nesting "(())()"
  in
  has_ended p
  && result p = Some 2
  && lookahead p = [None]


let%test _ =
  let open IP in
  let p = run nesting "(()(()))"
  in
  has_ended p
  && result p = Some 3
  && lookahead p = [None]


(* String parser *)
(* ************* *)

let%test _ =
  let open UP in
  let p = run (string "abcd") "abcd"
  in
  has_ended p
  && column p = 4
  && result p = Some ()
  && lookahead p = []

let%test _ =
  let open UP in
  let p = run (string "(a)" <|> string "(b)") "(b)"
  in
  has_ended p
  && column p = 1
  && result p = None
  && error p = one_expect "'a'"
  && lookahead p = [Some 'b']



(* Backtrackable *)
(* ************* *)

let%test _ =
  let open UP in
  let str = "(a)" in
  let p =
    run
      (backtrackable (string str) str)
      "(a"
  in
  has_ended p
  && line   p = 0
  && column p = 0
  && result p = None
  && error p = one_expect str
  && lookahead p = [Some '('; Some 'a'; None]


let%test _ =
  let open UP in
  let p =
    run
      (backtrackable (string "(a)") "(a)"
       <|> string "(b)")
      "(b)"
  in
  has_ended p
  && column p = 3
  && result p = Some ()
  && lookahead p = []


let%test _ =
  let open UP in
  let p =
    run
      ((backtrackable (string "(a)") "(a)"
        <|> string "(b)")
       |. expect_end)
      "(b)"
  in
  has_ended p
  && column p = 3
  && result p = Some ()
  && lookahead p = []





(* Parser Pipelines *)
(* **************** *)
let%test _ =
  let module SP = Simple (String) in
  let open SP in
  let p =
    run
      (return (fun c1 c2 c3 -> String.one c1 ^ String.one c2 ^ String.one c3)
       |= letter
       |. letter
       |= digit
       |= letter
       |. digit)
      "ab1d0"
  in
  has_ended p
  && result p = Some "a1d"
  && column p = 5
  && lookahead p = []





(* Indentation sensitivity *)
(* *********************** *)
module Indent_parser =
  struct
    module P = Simple (Unit)
    include P

    let white_space: int t =
      detached P.whitespace

    let letter_ws: char t =
      return identity
      |= letter
      |. white_space

    let result_string (p:parser): string =
      result_string p (fun _ -> "()")

    let print (p:parser) (str:string): unit =
      let open Printf in
      printf "string <%s>\n" (String.escaped str);
      printf "line %d, column %d\n" (line p) (column p);
      printf "%s\n"  (result_string p);
      printf "lookahead %s\n\n" (lookahead_string p)

    let _ = print (* to avoid warning of unused 'print' *)
 end

let%test _ =
  let open Indent_parser in
  let str = "a\nb" in
  let p = run
            (return ()
             |. letter_ws
             |. (indented true letter_ws)
             |. expect_end)
            str
  in
  (*print p str;*)
  has_ended p
  && has_failed p
  && lookahead p = [Some 'b']
  && line p = 1
  && column p = 0



let%test _ =
  let open Indent_parser in
  let str = "a\n b\nc" in
  let p = run
            (return ()
             |. letter_ws
             |. (indented true letter_ws)
             |. letter_ws
             |. expect_end)
            str
  in
  (*print p str;*)
  has_ended p
  && has_succeeded p
  && lookahead p = []
  && line p = 2
  && column p = 1



let%test _ =
  let open Indent_parser in
  let str = "a\n  b c\n d\n     e\nz" in
  let p = run
            (return ()
             |. letter_ws
             |. (indented true (skip_one_or_more letter_ws))
             |. letter_ws
             |. expect_end)
            str
  in
  (*print p str;*)
  has_ended p
  && has_succeeded p
  && lookahead p = []
  && line p = 4
  && column p = 1



let%test _ =
  let open Indent_parser in
  let str = " a\n b\n  " in
  let p = run
            (return ()
             |. white_space
             |. absolute
                  (return ()
                   |. absolute (letter_ws)
                   |. absolute (letter_ws))
             |. expect_end)
            str
  in
  (*print p str;*)
  has_ended p
  && has_succeeded p
  && lookahead p = []
  && line p = 2
  && column p = 2



let%test _ =
  let open Indent_parser in
  let str = " a\n  b" in
  let p = run
            (return ()
             |. white_space
             |. absolute
                  (return ()
                   |. absolute (letter_ws)
                   |. absolute (letter_ws))
             |. expect_end)
            str
  in
  (*print p str;*)
  has_ended p
  && has_failed p
  && lookahead p = [Some 'b']
  && line p = 1
  && column p = 2



let%test _ =
  let open Indent_parser in
  let str = "a\nb\n c\n d" in
  let p = run
            (return ()
             |. white_space
             |. absolute
                  (return ()
                   |. absolute (letter_ws)
                   |. absolute (letter_ws)
                   |. indented
                        true
                        (absolute
                           ( return ()
                             |. absolute (letter_ws)
                             |. absolute (letter_ws)))
                  )
             |. expect_end)
            str
  in
  (*print p str;*)
  has_ended p
  && has_succeeded p
  && lookahead p = []
  && line p = 3
  && column p = 2



let%test _ =
  let open Indent_parser in
  let str = "a\nb\n c\nd" in
  let p = run
            (return ()
             |. white_space
             |. absolute
                  (return ()
                   |. absolute (letter_ws)
                   |. absolute (letter_ws)
                   |. indented
                        true
                        (absolute
                           ( return ()
                             |. absolute (letter_ws)
                             |. absolute (letter_ws)))
                  )
             |. expect_end)
            str
  in
  (*print p str;*)
  has_ended p
  && has_failed p
  && lookahead p = [Some 'd']
  && line p = 3
  && column p = 0



let%test _ =
  let open Indent_parser in
  let str = "a\nb\n c\n  d" in
  let p = run
            (return ()
             |. white_space
             |. absolute
                  (return ()
                   |. absolute (letter_ws)
                   |. absolute (letter_ws)
                   |. indented
                        true
                        (absolute
                           ( return ()
                             |. absolute (letter_ws)
                             |. absolute (letter_ws)))
                  )
             |. expect_end)
            str
  in
  (*print p str;*)
  has_ended p
  && has_failed p
  && lookahead p = [Some 'd']
  && line p = 3
  && column p = 2


let%test _ =
  let open Indent_parser in
  let str = "a\n x\n y\nb\nc" in
  let p = run
            (return ()
             |. skip_one_or_more_aligned
                  (letter_ws
                   |. indented true (skip_zero_or_more_aligned letter_ws)
                  )
             |. expect_end)
            str
  in
  (*print p str;*)
  has_ended p
  && lookahead p = []
  && line p = 4
  && column p = 1
