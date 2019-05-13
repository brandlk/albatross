open Common_module_types


module Parse_buffer (S:ANY) (T:ANY) (E:ANY):
sig
  type state = S.t
  type token = T.t
  type error = E.t
  type consumed = bool
  type saved_errors
  type back
  type t

  val has_lookahead: t -> bool
  val has_consumed: t -> bool
  val state:        t -> state
  val lookahead:    t -> token list
  val errors:       t -> error list

  val init: state -> t
  val pop_one_lookahead: t -> token

  val add_error: error -> t -> unit

  val consume: token -> state -> t -> unit
  val reject:  token -> error -> t -> unit
  val reset_consumed: t -> consumed
  val set_consumed: consumed -> t -> unit

  val reset_errors: t -> saved_errors
  val set_errors: error list -> t -> unit
  val update_errors: saved_errors -> t -> unit

  val start_backtrack: t -> back
  val end_backtrack_success: back -> t -> unit
  val end_backtrack_fail:    back -> t -> unit
  val commit: t -> unit
end =
  struct
    type state = S.t
    type token = T.t
    type error = E.t
    type consumed = bool
    type is_buffering = bool
    type consumption_length = int

    type commit = Not | Committing | Committed
    type back = state * consumed * consumption_length
                * error list
                * is_buffering * is_buffering  * commit

    type saved_errors = consumed * error list

    type t = {mutable state: state;
              mutable consumed:      consumed;
              mutable consumption:   token list;
              mutable lookahead:     token list;
              mutable n_consumption: consumption_length;
              mutable errors:        error list;
              mutable isbuf:         is_buffering;
              mutable isbuf_prev:    is_buffering;
              mutable commit:        commit}

    let has_lookahead(b:t): bool = b.lookahead <> []
    let lookahead    (b:t): token list = b.lookahead
    let errors       (b:t): error list = List.rev b.errors
    let has_consumed (b:t): bool = b.consumed
    let state        (b:t): state = b.state

    let init (s:state): t =
      {state = s;
       consumed = false; consumption = []; n_consumption = 0;
       lookahead = [];
       errors = [];
       isbuf = false; isbuf_prev = false; commit = Not}

    let pop_one_lookahead (b:t): token =
      match b.lookahead with
      | [] -> assert false (* Illegal call *)
      | t :: rest ->
         b.lookahead <- rest;
         t

    let add_error (e:error) (b:t): unit =
      b.errors <- e :: b.errors

    let consume (t:token) (s:state) (b:t): unit =
      b.consumed <- true;
      b.state    <- s;
      b.errors   <- [];
      if b.isbuf then
        (* A parser is buffering if it is part of a backtrackable parser which
           has not yet committed. *)
        ((* Put consumed token into the consumption buffer. *)
         b.consumption   <- t :: b.consumption;         (* 2 *)
         b.n_consumption <- b.n_consumption + 1;

         if b.commit = Committing then
           (b.commit <- Committed; (* If a commit has been issued
                                      (i.e. [Committing]), the next consumed
                                      token sets the [commit] to
                                      [Committed]. *)
            if not b.isbuf_prev then  (* A committed parser does no longer
                                         buffer consumed tokens unless an
                                         outer parser is buffering. *)
              b.isbuf <- false)
        )

    let reject (t:token) (e:error) (b:t): unit =
      b.errors <- e :: b.errors;
      b.lookahead <- t :: b.lookahead

    let reset_consumed (b:t): bool =
      let c = b.consumed in
      b.consumed <- false; c

    let set_consumed (c:bool) (b:t): unit =
      b.consumed <- c

    let reset_errors (b:t): saved_errors =
      let saved = b.consumed, b.errors in
      b.consumed <- false;  (* to be able to check if the next parser has
                               consumed anything *)
      b.errors <- [];       (* in order to collect the error messages
                               generated by the next parser *)
      saved

    let set_errors (errs:error list) (b:t): unit =
      b.errors <- errs

    let update_errors ((c,errs):saved_errors) (b:t): unit =
      if b.consumed then
        () (* parser has advanced, old errors are obsolete *)
      else
        (b.consumed <- c;            (* restore consumption flag *)
         b.errors <- b.errors @ errs  (* parser has not consumed, old error
                                        messages are still valid *)
        )

    let move_buffered (n:int) (la:bool) (b:t): unit =
      (* Remove the buffered consumed tokens so that the consumption buffer
         has only n tokens and put them into the lookahead buffer if the la
         flag is set. *)
      assert (n <= b.n_consumption);
      while n < b.n_consumption do
        match b.consumption with
        | [] -> assert false (* cannot happen *)
        | t :: rest ->
           b.consumption   <- rest;
           b.n_consumption <- b.n_consumption - 1;
           if la then
             b.lookahead <- t :: b.lookahead
      done

    let start_backtrack (b:t): back =
      let saved = b.state, b.consumed, b.n_consumption,
                  b.errors, b.isbuf, b.isbuf_prev, b.commit in
      b.consumed <- false;
      b.isbuf_prev <- b.isbuf;
      b.isbuf <- true;
      b.errors <- [];
      b.commit <- Not;
      saved

    let end_backtrack_success ((_,c,n,es,ib,ibp,comm):back) (b:t): unit =
      if not ib then (* no buffering previously, therefore remove all newly
                        buffered tokens. *)
        move_buffered n false b;
      if not b.consumed then (* backtrackable parser has not consumed tokens,
                                errors must be restored *)
        b.errors <- es;
      b.consumed   <- c && b.consumed;
      b.isbuf      <- ib;
      b.isbuf_prev <- ibp;
      b.commit     <- comm

    let end_backtrack_fail    ((s,c,n,es,ib,ibp,comm):back) (b:t): unit =
      if b.commit <> Committed then (* undo consumption *)
        (move_buffered n true b;
         b.consumed <- c;
         b.state <- s;
         b.errors <- b.errors @ es)
      else
        b.consumed   <- c && b.consumed;
      b.isbuf      <- ib;
      b.isbuf_prev <- ibp;
      b.commit     <- comm

    let commit (b:t): unit =
      if b.isbuf && b.commit = Not then
        b.commit <- Committing

  end






module Make (T:ANY) (S:ANY) (E:ANY) (F:ANY) =
  struct
    type token = T.t
    type error = E.t
    type state = S.t
    type final = F.t

    module B = Parse_buffer (S) (T) (E)

    type parser =
      | More  of state * (state -> token -> parser)
      | Final of state * (final, error list) result * token list

    let needs_more (p:parser): bool =
      match p with
      | More _ -> true | Final _ -> false

    let has_ended (p:parser): bool = not (needs_more p)

    let put_token (p:parser) (t:token): parser =
      assert (needs_more p);
      match p with
      | More (st,f) ->
         f st t
      | _ ->
         assert false (* Illegal call *)

    let state (p:parser): state =
      match p with
      | More (st,_) | Final (st,_,_) -> st

    let result (p:parser): (final,error list) result =
      match p with
      | Final (_,r,_) -> r
      | _ -> assert false (* Illegal call! *)

    let lookahead (p:parser): token list =
      match p with
      | Final (_,_,la) -> la
      | _ -> assert false (* Illegal call! *)



    type 'a t = ('a option -> parser) -> B.t -> parser

    let make_parser (s:state) (p:final t): parser =
      let b = B.init s in
      p (fun o ->  (* continuation function for p *)
          Final (B.state b,
                 (match o with
                  | Some x -> Ok x
                  | None   -> Error (B.errors b)),
                 B.lookahead b))
        b

    let succeed (a:'a) (k:'a option -> parser) (_:B.t): parser =
      k (Some a)

    let fail (e:error) (k:'a option -> parser) (b:B.t): parser =
      B.add_error e b;
      k None


    let token
          (f:state -> token -> ('a*state, error) result)
          (k:'a option -> parser)
          (b:B.t): parser =
      More (B.state b,
            fun s0 t ->
            match f s0 t with
            | Ok (a, s1) ->
               B.consume t s1 b;
               k (Some a)
            | Error e ->
               B.reject t e b;
               k None)

    let map (f:'a -> 'b) (p:'a t) (k:'b option -> parser) (b:B.t): parser =
      p (fun o ->
          match o with
          | None -> k None
          | Some a -> k (Some (f a)))
        b

    let apply_lookahead (b:B.t) (p:parser): parser =
      if B.has_lookahead b && needs_more p  then
        let t = B.pop_one_lookahead b in
        put_token p t
      else
        p


    let consumer (p:'a t) (k:'a option -> parser) (b:B.t): parser =
      let c0 = B.reset_consumed b in
      p (fun o ->
          let c1 = B.has_consumed b in
          assert (o = None || c1);
          B.set_consumed (c0 || c1) b;
          k o)
        b

    let (>>=) (p:'a t) (f:'a -> 'b t) (k:'b option -> parser) (b:B.t): parser =
      p (fun o ->  (* continuation function for p *)
          match o with
          | Some a ->  (* p might have left over some lookahead *)
             apply_lookahead b (f a k b)
          | None ->
             k None)
        b

    let (<|>) (p:'a t) (q:'a t) (k:'a option -> parser) (b:B.t): parser =
      let c0 = B.reset_consumed b in
      p (fun o ->
          let c1 = B.has_consumed b in
          B.set_consumed (c0 || c1) b;
          match o with
          | None when not c1 -> (* p did not consume tokens *)
             apply_lookahead b (q k b)
          | _ ->
             (* p either succeeded or failed and consumed tokens *)
             k o)
        b

    let (<?>) (p:'a t) (es:error list) (k:'a option -> parser) (b:B.t): parser =
      let e0 = B.reset_errors b in
      p (fun o ->
          (match o with
           | None -> B.set_errors es b
           | _ ->    ());
          B.update_errors e0 b;
          k o)
        b

    let backtrackable (p:'a t) (k:'a option -> parser) (b:B.t): parser =
      let back = B.start_backtrack b in
      p (fun o ->
          (match o with
           | Some _ -> B.end_backtrack_success back b
           | None ->   B.end_backtrack_fail    back b);
          k o)
        b

    let commit (a:'a) (k:'a option -> parser) (b:B.t): parser =
      B.commit b;
      k (Some a)
  end (* Make *)
