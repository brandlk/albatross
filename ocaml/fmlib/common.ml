let identity (a:'a): 'a = a

module Void:
sig
  type t
end =
  struct
    type t = int
  end


module Unit:
sig
  type t = unit
end =
  struct
    type t = unit
  end


module Int =
  struct
    type t = int
    let compare = Pervasives.compare
  end

module Int_set = Set.Make (Int)



module Float =
  struct
    type t = float
    let compare = Pervasives.compare
  end



module Either =
  struct
    type ('a,'b) t =
      | Left of 'a
      | Right of 'b
    let left a = Left a
    let right b = Right b
  end


module Char =
  struct
    include Char
    let is_lower (c:char): bool =
      'a' <= c && c <= 'z'
    let is_upper (c:char): bool =
      'A' <= c && c <= 'Z'
    let is_letter (c:char): bool =
      is_lower c || is_upper c
    let is_digit (c:char): bool =
      '0' <= c && c <= '9'
  end


module String =
  struct
    include String

    let one (c:char): string =
      String.make 1 c

    let is_prefix (a: string) (b:string): bool =
      let len_a = length a in
      len_a <= length b && a = sub b 0 len_a

    let is_suffix (a: string) (b:string): bool =
      let len_a = length a
      and len_b = length b
      in
      len_a <= len_b
      && a = sub b  (len_b - len_a) len_a

    let find (f:char -> bool) (start:int) (s:string): int =
      let len = String.length s in
      let rec find i =
        if i = len || f s.[i] then
          i
        else
          find (i+1)
      in
      find start

    let find_bwd (f:char -> bool) (beyond:int) (s:string): int =
      assert (beyond <= String.length s);
      let rec find i =
        if i = 0 || f s.[i-1] then
          i-1
        else
          find (i-1)
      in
      find beyond

    let list (s:string): char list =
      let rec list cs i =
        if i = 0 then
          cs
        else
          let j = i - 1 in
          list (s.[j]::cs) j
      in
      list [] (length s)

    let of_list (cs:char list): string =
      let rec str cs i =
        match cs with
        | [] ->
           Bytes.create i
        | c::cs ->
           let bs = str cs (i+1) in
           Bytes.set bs i c;
           bs
      in
      let bs = str cs 0 in
      Bytes.unsafe_to_string bs
  end


module String_set = Set.Make(String)
module String_map = Finite_map.Make(String)


module Interval =
  struct
    let find (p:int -> bool) (start:int) (beyond:int): int =
      let rec fnd i =
        if i = beyond || p i then
          i
        else
          fnd (i+1)
      in
      fnd start


    let fold (a:'a) (f:int -> 'a -> 'a) (start:int) (beyond:int): 'a =
      assert (start <= beyond);
      let rec fold i a =
        if i = beyond then
          a

        else
          fold (i + 1) (f i a)
      in
      fold start a


    module Monadic (M:Common_module_types.MONAD) =
      struct
        let fold
              (a:'a)
              (f: int -> 'a -> 'a M.t)
              (start:int)
              (beyond:int)
            : 'a M.t
          =
          assert (start <= beyond);
          let rec fold i a =
            if i = beyond then
              M.return a

            else
              M.(f i a >>= fold (i + 1))
          in

          fold start a
      end
  end

module Loop_state =
  struct
    type ('a,'b) t =
      | More of 'a
      | Exit of 'b

    let fold (f1:'a -> 'c) (f2:'b -> 'c) = function
      | More a -> f1 a
      | Exit b -> f2 b

    let more a: ('a,'b) t = More a

    let exit b: ('a,'b) t = Exit b
  end



module String_reader =
  struct
    type t = {s: string; pos:int; beyond:int}

    let of_substring (s:string) (start:int) (len:int): t =
      assert (0 <= start);
      assert (start + len <= String.length s);
      {s; pos = start; beyond = start + len}

    let of_string (s:string): t =
      of_substring s 0 (String.length s)

    let has_more (r:t): bool =
      r.pos < r.beyond

    let peek (r:t): char =
      assert (has_more r);
      r.s.[r.pos]

    let advance (r:t): t =
      assert (has_more r);
      {r with pos = r.pos + 1}
  end

module Fill_reader =
  struct
    type t = {n:int; c:char}
    let has_more (r:t): bool =
      r.n > 0
    let peek (r:t): char =
      r.c
    let advance (r:t): t =
      {r with n = r.n - 1}
    let make (n:int) (c:char): t =
      {n;c}
  end

module Char_reader =
  struct
    type t = char option
    let make (c:char): t =
      Some c
    let has_more (cr:t): bool =
      cr <> None
    let peek (cr:t): char =
      match cr with
      | None -> assert false (* Illegal call! *)
      | Some c -> c
    let advance (_:t): t =
      None
  end




module type SEXP =
  sig
    type t =
      | Atom of string
      | Seq of t array
    val string: t -> string
  end


module Sexp =
  struct
    type t =
      | Atom of string
      | Seq of t array
    let string(s:t): string =
      let rec string0 i s =
        match s with
        | Atom str ->
           str
        | Seq arr ->
           let s0 =
             String.concat
               ""
               (List.map (string0 (i+1)) (Array.to_list arr))
           in
           if i = 0 then
             s0
           else
             "(" ^ s0 ^ ")"
      in
      string0 0 s
  end
