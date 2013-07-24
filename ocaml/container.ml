module type Search = sig
  val binsearch_max: 'a -> 'a array -> int
end

module Search: Search = struct
  let binsearch_max (el:'a) (arr: 'a array) =
    (* returns the maximal index where el can to be inserted *)
    let len = Array.length arr
    in
    (* all k: 0<=k<=i => arr.(k)<=el   *)
    (*        j<=k<len => el < arr.(k) *)
    let rec search i j =
      assert (0<=i); assert (i<=j); assert (j<=len);
      if (j-i) <= 1 then
        if i<j && el<arr.(i) then i else j
      else
        let m = i + (j-i)/2
        in
        if arr.(m)<=el then search m j
        else                search i m
    in
    let idx = search 0 len
    in
    assert (0<=idx && idx<=Array.length arr);
    idx
end



module Seq: sig
  type 'a sequence
  val empty: unit -> 'a sequence
  val singleton: 'a -> 'a sequence
  val count: 'a sequence -> int
  val elem:  'a sequence -> int -> 'a
  val push:  'a sequence -> 'a -> unit
end = struct
  type 'a sequence = {mutable cnt:int;
                      mutable arr: 'a array}
  let empty () = {cnt=0; arr=Array.init 0 (function _ -> assert false)}
  let singleton e = {cnt=1; arr=Array.make 1 e}
  let count seq  = seq.cnt
  let elem seq i =
    assert (i<seq.cnt);
    seq.arr.(i)
  let push seq elem =
    let cnt = seq.cnt
    in
    let _ =
      if cnt = Array.length seq.arr then
        let narr =
          Array.make (1+2*cnt) elem
        in
        Array.blit seq.arr 0 narr 0 cnt;
        seq.arr <- narr
      else
        ()
    in
    assert (cnt < Array.length seq.arr);
    seq.arr.(cnt) <- elem;
    seq.cnt <- cnt+1
end

type 'a seq = 'a Seq.sequence


module type Set = sig
  type 'a set
  val empty:      unit -> 'a set
  val mem:        'a -> 'a set -> bool
  val plus_elem:  'a -> 'a set -> 'a set
  val plus_set:   'a set -> 'a set -> 'a set
  val test:       unit -> unit
end


module ArrayedSet: Set = struct
  type 'a set = 'a array

  let empty () = Array.init 0 (fun _ -> assert false)

  let mem (el:'a) (set:'a set) =
    let idx = Search.binsearch_max el set
    in
    0<idx && set.(idx-1)=el

  let plus_elem (el:'a) (set:'a set) =
    let i = Search.binsearch_max el set;
    and len = Array.length set
    in
    if 0<i  && set.(i-1)=el then
      set
    else
      Array.init (len+1) 
        (fun j -> 
          if j<i then set.(j)
          else if j=i then el
          else set.(j-1))

  let plus_set s1 s2 =
    let rec plus i =
      if i=0 then s1
      else plus_elem s2.(i-1) s1
    in
    plus (Array.length s2)

  let test () =
    let len = 10
    in
    let rec ins n =
      if n=0 then empty ()
      else plus_elem (len-n) (plus_elem (len-n) (ins (n-1)))
    in
    let set = ins len
    in
    let rec check n =
      if n=0 then true
      else ((n-1)=set.(n-1)) && mem (n-1) set && check (n-1)
    in
    (*Printf.printf "arr = [";
    Array.iter (fun e -> Printf.printf "%d " e) set;
    Printf.printf "]\n";*)
    assert (check len);
    assert (not (mem len set))
end

type 'a arrayed_set = 'a ArrayedSet.set
