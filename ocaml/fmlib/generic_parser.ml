open Common_module_types

module type BASIC =
  sig
    type 'a t
    val return: 'a -> 'a t
    val succeed: 'a -> 'a t
    val consumer: 'a t -> 'a t
    val map:     ('a -> 'b) -> 'a t -> 'b t
    val (>>=):   'a t -> ('a -> 'b t) -> 'b t
    val (<|>):   'a t -> 'a t -> 'a t
    val backtrackable: 'a t -> 'a t
    val commit: 'a -> 'a t
  end


module Buffer (S:ANY) (T:ANY) (E:ANY) =
  struct
    type state = S.t
    type token = T.t
    type error = E.t

    type commit = Not | Committing | Committed

    type backtrack_state = {
        state: state;
        has_consumed: bool;
        consumption_length: int;
        errors: error list;
        is_buffering: bool;
        is_previous_buffering: bool;
        commit: commit
      }
    type t = {
        bs: backtrack_state;
        consumption: token list;
        lookahead:   token list;
      }

    let init (st:state): t =
      {bs = {state = st;
             has_consumed = false;
             consumption_length = 0;
             errors = [];
             is_buffering = false;
             is_previous_buffering = false;
             commit = Not};
       consumption = [];
       lookahead = []}

    let state (b:t): state =
      b.bs.state

    let errors (b:t): error list =
      List.rev b.bs.errors

    let has_consumed (b:t): bool =
      b.bs.has_consumed

    let has_lookahead (b:t): bool =
      b.lookahead <> []

    let lookahead (b:t): token list =
      b.lookahead

    let update (f:state->state) (b:t): t =
      {b with bs = {b.bs with state = f b.bs.state}}

    let pop_one_lookahead (b:t): token * t =
      match b.lookahead with
      | [] -> assert false (* Illegal call *)
      | t :: lookahead ->
         t, {b with lookahead}

    let add_error (e:error) (b:t): t =
      {b with bs = {b.bs with errors = e :: b.bs.errors}}

    let clear_errors  (b:t): t =
      {b with bs = {b.bs with errors = []}}


    let consume (t:token) (state:state) (b:t): t =
      let isbuf = b.bs.is_buffering
      and committing = b.bs.commit = Committing
      in
      {b with
        bs = {b.bs with
               has_consumed = true;
               state;
               errors = [];
               consumption_length =
                 if isbuf then
                   b.bs.consumption_length + 1
                 else
                   b.bs.consumption_length;
               commit =
                 if isbuf && committing then
                   Committed
                 else
                   b.bs.commit;
               is_buffering =
                 if committing then
                   b.bs.is_previous_buffering
                 else
                   b.bs.is_buffering
             };
        consumption =
          if isbuf then
            t :: b.consumption
          else
            b.consumption}

    let reject (t:token) (e:error) (b:t): t =
      {b with bs = {b.bs with errors = e :: b.bs.errors};
              lookahead = t :: b.lookahead}


    let reset_consumed (b:t): bool * t =
      b.bs.has_consumed,
      {b with bs = {b.bs with has_consumed = false}}

    let set_consumed (c:bool) (b:t): t =
      {b with bs = {b.bs with has_consumed = c}}


    let move_buffered (n:int) (flg:bool) (cons:token list) (la:token list)
        : token list * token list =
      (* Remove n buffered consumed tokens and put them into the lookahead
         buffer if the flag is set. *)
      let rec move n cons la =
        if n = 0 then
          cons,la
        else
          match cons with
          | [] ->
             assert false (* Illegal call *)
          | t :: rest ->
             move
               (n-1) rest
               (if flg then
                  t :: la
                else
                  la)
      in
      move n cons la


    let start_backtrack (b:t): backtrack_state * t =
      let bs = b.bs in
      bs,
      {b with bs = {bs with
                     has_consumed = false;
                     is_previous_buffering = bs.is_buffering;
                     is_buffering = true;
                     errors = [];
                     commit = Not}}


    let end_backtrack_success (bs:backtrack_state) (b:t): t =
      let consumption_length, consumption, lookahead =
        if bs.is_buffering then
          b.bs.consumption_length, b.consumption, b.lookahead
        else
          (* not buffering previously, therefore remove all newly buffered
             tokens. *)
          let n = b.bs.consumption_length - bs.consumption_length in
          let cons,la =
            move_buffered n false b.consumption b.lookahead
          in
          bs.consumption_length, cons, la
      in
      {bs = {b.bs with
              has_consumed = bs.has_consumed || b.bs.has_consumed (*????*);
              consumption_length;
              is_buffering = bs.is_buffering;
              is_previous_buffering = bs.is_previous_buffering;
              errors =
                if not b.bs.has_consumed then
                  bs.errors (* backtrackable parser has not consumed tokens,
                               errors must be restored *)
                else
                  b.bs.errors;
              commit = bs.commit};
       consumption;
       lookahead}

    let end_backtrack_fail (bs:backtrack_state) (b:t): t =
      let committed = b.bs.commit = Committed
      and consumption_length =
        if bs.is_buffering then
          b.bs.consumption_length
        else
          bs.consumption_length
      in
      let consumption, lookahead =
        move_buffered
          (b.bs.consumption_length - consumption_length)
          (not committed)
          b.consumption
          b.lookahead
      in
      {bs = {state =  if committed then
                        b.bs.state
                      else
                        bs.state;
             has_consumed =
               if committed then
                 bs.has_consumed || b.bs.has_consumed
               else
                 bs.has_consumed;
             consumption_length;
             errors = if committed then
                        b.bs.errors
                      else
                        b.bs.errors @  bs.errors;
             is_buffering = bs.is_buffering;
             is_previous_buffering = bs.is_previous_buffering;
             commit = bs.commit};
       consumption;
       lookahead}


    let commit (b:t): t =
      if b.bs.is_buffering && b.bs.commit = Not then
        {b with bs = {b.bs with commit = Committing}}
      else
        b
  end






module Make (T:ANY) (S:ANY) (E:ANY) (F:ANY) =
  struct
    type token = T.t
    type error = E.t
    type state = S.t
    type final = F.t

    module B = Buffer (S) (T) (E)

    type parser =
      | More  of B.t * (B.t -> token -> parser)
      | Final of B.t * (final, error list) result * token list

    let needs_more (p:parser): bool =
      match p with
      | More _ -> true | Final _ -> false

    let has_ended (p:parser): bool = not (needs_more p)


    let put_token (p:parser) (t:token): parser =
      assert (needs_more p);
      let rec put_lookahead p =
        match p with
        | More (b,f) when B.has_lookahead b ->
           let t,b = B.pop_one_lookahead b in
           put_lookahead @@ f b t
        | _ ->
           p
      in
      match p with
      | More (b,f) ->
         assert (not (B.has_lookahead b));
         put_lookahead @@ f b t
      | _ ->
         assert false (* Illegal call *)


    let state (p:parser): state =
      match p with
      | More (b,_) | Final (b,_,_) -> B.state b

    let result (p:parser): (final,error list) result =
      match p with
      | Final (_,r,_) -> r
      | _ -> assert false (* Illegal call! *)

    let lookahead (p:parser): token list =
      match p with
      | Final (_,_,la) -> la
      | _ -> assert false (* Illegal call! *)



    type 'a cont = 'a option -> B.t -> parser
    type 'a t = B.t -> 'a cont -> parser

    let make_parser (s:state) (p:final t): parser =
      p (B.init s)
        (fun o b ->
          Final (b,
                 (match o with
                  | Some x -> Ok x
                  | None   -> Error (B.errors b)),
                 B.lookahead b))

    let update (f:state->state) (b:B.t) (k:unit cont): parser =
      k (Some ()) (B.update f b)

    let get (b:B.t) (k:state cont): parser =
      k (Some (B.state b)) b

    let get_and_update
          (f:state->state) (b:B.t) (k:state cont): parser =
      let st = B.state b in
      k (Some st) (B.update f b)

    let return (a:'a) (b:B.t) (k:'a cont): parser =
      k (Some a) b

    let succeed (a:'a) (b:B.t) (k:'a cont): parser =
      k (Some a) (B.clear_errors b)

    let fail (e:error) (b:B.t) (k:'a cont): parser =
      k None (B.add_error e b)


    let token
          (f:state -> token -> ('a*state, error) result)
          (b:B.t)
          (k:'a cont)
        : parser
      =
      More (b,
            fun b0 t ->
            match f (B.state b0) t with
            | Ok (a, s1) ->
               k (Some a) (B.consume t s1 b0)
            | Error e ->
               k None (B.reject t e b0))

    let map (f:'a -> 'b) (p:'a t) (b:B.t) (k:'b cont): parser =
      p b
        (fun o b ->
          match o with
          | None -> k None b
          | Some a -> k (Some (f a)) b)


    let consumer (p:'a t) (b:B.t) (k:'a cont): parser =
      let c0,b = B.reset_consumed b in
      p b
        (fun o b ->
          let c1 = B.has_consumed b in
          assert (o = None || c1);
          k o (B.set_consumed (c0 || c1) b))


    let (>>=) (p:'a t) (f:'a -> 'b t) (b:B.t) (k:'b cont): parser =
      p
        b
        (fun o b ->
          match o with
          | Some a ->
             f a b k
          | None ->
             k None b)

    let (<|>) (p:'a t) (q:'a t) (b:B.t) (k:'a cont): parser =
      let c0,b = B.reset_consumed b in
      p b
        (fun o b ->
          let c1 = B.has_consumed b in
          let b = B.set_consumed (c0 || c1) b in
          match o with
          | None when not c1 ->
             (* p failed and did not consume tokens *)
             q b k
          |  _ ->
             k o b)


    let backtrackable (p:'a t) (b:B.t) (k:'a cont): parser =
      let back,b = B.start_backtrack b in
      p b
        (fun o b ->
          k o
            (match o with
            | None -> B.end_backtrack_fail back b
            | Some _ -> B.end_backtrack_success back b))


    let commit (a:'a) (b:B.t) (k:'a cont): parser =
      k (Some a) (B.commit b)
  end (* Make *)