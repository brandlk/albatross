open Common


module type SCANNER =
  sig
    type t
    val can_receive: t -> bool
    val receive: char -> t -> t
    val end_buffer: t -> t
    val end_stream: t -> t
  end


module type S0 =
  sig
    type in_file
    type out_file

    val stdin:  in_file
    val stdout: out_file
    val stderr: out_file

    include Monad.MONAD

    val exit: int -> 'a t
    val execute: unit t -> unit

    val command_line: string array t

    val open_for_read:  string -> in_file  option t
    val open_for_write: string -> out_file option t
    val create: string -> out_file option t
    val close_in:  in_file  -> unit t
    val close_out: out_file -> unit t
    val flush: out_file -> unit t
    val flush_all: unit t

    val getc: in_file -> char option t
    val putc: out_file -> char ->  unit t
    val get_line: in_file -> string option t
    val scan: in_file -> (char,'a) Scan.t -> 'a t
    val put_substring: out_file -> int -> int -> string -> unit t

    module Scan: functor (S:SCANNER) ->
                 sig
                   val buffer: in_file -> S.t -> S.t t
                   val stream: in_file -> S.t -> S.t t
                 end
  end


module type S =
  sig
    include S0

    val read_file:   string -> 'a t -> (in_file  -> 'a t) -> 'a t
    val write_file:  string -> 'a t -> (out_file -> 'a t) -> 'a t
    val create_file: string -> 'a t -> (out_file -> 'a t) -> 'a t

    val put_string: out_file -> string -> unit t
    val put_line:   out_file -> string -> unit t
    val put_newline:   out_file -> unit t
    val fill: out_file -> char -> int -> unit t

    val getc_in: char option t
    val get_line_in: string option t

    val putc_out: char -> unit t
    val put_string_out: string -> unit t
    val put_line_out: string -> unit t
    val put_newline_out: unit t

    val putc_err: char -> unit t
    val put_string_err: string -> unit t
    val put_line_err: string -> unit t
    val put_newline_err: unit t
  end


module Make (M:S0): S =
  struct
    include M

    let read_file
          (path:string) (cannot_open:'a t) (read:in_file -> 'a t)
        : 'a t =
      open_for_read path >>= fun fd ->
      match fd with
      | None ->
         cannot_open
      | Some fd ->
         read fd >>= fun a ->
         close_in fd >>= fun _ ->
         make a

    let write_file
          (path:string) (cannot_open:'a t) (write:out_file -> 'a t)
        : 'a t =
      open_for_write path >>= fun fd ->
      match fd with
      | None ->
         cannot_open
      | Some fd ->
         write fd >>= fun a ->
         close_out fd >>= fun _ ->
         make a

    let create_file
          (path:string) (cannot_create:'a t) (write:out_file -> 'a t): 'a t =
      create path >>= fun fd ->
      match fd with
      | None ->
         cannot_create
      | Some fd ->
         write fd >>= fun a ->
         close_out fd >>= fun _ ->
         make a

    let put_string (fd:out_file) (s:string): unit t =
      put_substring fd 0 (String.length s) s

    let put_newline (fd:out_file): unit t =
      putc fd '\n'

    let put_line (fd:out_file) (s:string): unit t =
      put_string fd s >>= fun _ ->
      put_newline fd


    let fill (fd:out_file) (c:char) (n:int): unit t =
      let rec put i =
        if i = n then
          make ()
        else
          putc fd c >>= fun _ -> put (i+1)
      in
      put 0


    let getc_in: char option t =
      getc stdin

    let get_line_in: string option t =
      get_line stdin

    let putc_out (c:char): unit t =
      putc stdout c

    let put_string_out (s:string): unit t =
      put_string stdout s

    let put_line_out (s:string): unit t =
      put_line stdout s

    let put_newline_out: unit t =
      put_newline stdout

    let putc_err (c:char): unit t =
      putc stderr c

    let put_string_err (s:string): unit t =
      put_string stderr s

    let put_line_err (s:string): unit t =
      put_line stderr s

    let put_newline_err: unit t =
      put_newline stderr
  end


module Output (Io:S) =
  struct
    type out_file = Io.out_file

    include
      Monad.Make (
          struct
            type 'a t = out_file -> 'a Io.t
            let make (a:'a) (fd:out_file): 'a Io.t =
              Io.make a
            let bind (m:'a t) (f:'a -> 'b t) (fd:out_file): 'b Io.t =
              Io.(m fd >>= fun a -> f a fd)
          end
        )

    let putc (c:char) (fd:out_file): unit Io.t =
      Io.putc fd c

    let put_substring
          (start:int) (len:int) (s:string) (fd:out_file)
        : unit Io.t =
      Io.put_substring fd start len s

    let put_string (s:string): unit t =
      put_substring 0 (String.length s) s

    let put_newline: unit t =
      putc '\n'

    let put_line (s:string): unit t =
      put_string s >>= fun _ -> put_newline

    let fill (c:char) (n:int) (fd:out_file): unit Io.t =
      Io.fill fd c n

    let run (fd:out_file) (m:'a t): 'a Io.t =
      m fd
 end
