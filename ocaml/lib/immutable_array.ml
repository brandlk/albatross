(* Let b be the bitsize of the branching factor and h be the height of the
   node (a leaf node has height 0).

   The maximum capacity of a tree is 2^((h+1)*b)

      h    capacity
      0    2^5      32
      1    2^10     1024      (32*32)
      2    2^15     32768     (32*32*32)
      3    2^20     1048576   (32*32*32)

   Capacity of a slot: 2^(h*b). Capacity of m slots: m * 2^(h*b).

   Find an element i: slot = i / 2^(h*b)   (i lsr h*b)
                      relative index = i mod 2^(h*b)
                                     = i - slot lsl h*b

   Append an element:
       a) tree is full: size = capacity
          new root with two nodes, the first one is the original,
          the second is a new node with one element at all levels below

       b) tree is not full: size < capacity

          b1) all slots are full:
                  create a new slot
          b2) last slot has still capacity
 *)

open Printf

let bitsize = 5 (* or the branching factor, i.e. branching factor = 32 *)
let node_size = 1 lsl bitsize




type 'a t =
  | Leaf of 'a array
  | Node of int * int * 'a t array (* size, level, nodes *)


let empty = Leaf [||]

let length (t:'a t): int =
  match t with
  | Leaf arr ->
     Array.length arr
  | Node (size,_,_) ->
     size

let height (t:'a t): int =
  match t with
  | Leaf _ ->
     0
  | Node (_,h,_) ->
     h

let index (i:int) (h:int): int * int =
  let slot = i lsr (h*bitsize) in
  let rel  = i - slot lsl (h*bitsize) in
  slot, rel


let rec elem (i:int) (t:'a t): 'a =
  assert (i < length t);
  match t with
  | Leaf arr ->
     arr.(i)
  | Node (size,h,arr) ->
     let slot,rel = index i h in
     assert (slot < Array.length arr);
     elem rel arr.(slot)


let rec singleton (e:'a) (h:int): 'a t =
  assert (0 <= h);
  if h = 0 then
    Leaf [|e|]
  else
    Node (1,h, [|singleton e (h-1)|])



let push_array (e:'a) (arr:'a array): 'a array =
  let len = Array.length arr in
  let arr2 = Array.make (len+1) e in
  Array.blit arr 0 arr2 0 len;
  arr2

let put_array (i:int) (e:'a) (arr:'a array): 'a array =
  let arr2 = Array.copy arr in
  arr2.(i) <- e;
  arr2


let rec put (i:int) (e:'a) (t:'a t): 'a t =
  assert (i < length t);
  match t with
  | Leaf arr ->
     Leaf (put_array i e arr)
  | Node (s,h,arr) ->
     let slot, rel = index i h in
     let arr = put_array slot (put rel e arr.(slot)) arr in
     Node (s,h,arr)


let push (e:'a) (t:'a t): 'a t =
  let n = length t in
  let rec push0 e t =
    match t with
    | Leaf arr ->
       let len = Array.length arr in
       if len < node_size then
         Leaf (push_array e arr), false
     else
       Node (len+1,1,[|Leaf arr; Leaf [|e|]|]), true
    | Node (size,h,arr) ->
       assert (0 < h);
       let len = Array.length arr in
       assert (0 < len);
       if size = len * (1 lsl (h*bitsize)) then
         if len = node_size then
           (* introduce a new level *)
           Node (size+1, h+1, [|t; singleton e h|]),
           true
         else
           (* introduce a new slot *)
           Node (size+1, h, push_array (singleton e (h-1)) arr),
           false
       else
         (* push into last slot *)
         let t0, incr = push0 e arr.(len-1) in
         assert (not incr);
         let arr2 = Array.copy arr in
         arr2.(len-1) <- t0;
         Node (size+1,h,arr2), false
  in
  let t,_ = push0 e t in
  assert (length t = n + 1);
  t


let test ()  =
  let n = 32768 in
  let fill (n:int): int t =
    assert (0 <= n);
    let rec fill0 (i:int) (t:int t): int t =
      if i = n then
        t
      else
        fill0 (i+1) (push i t) in
    fill0 0 empty
  in
  let overwrite (n:int) (t:int t): int t =
    let rec over i t =
      if i = n then
        t
      else
        over (i+1) (put i (i+1) t)
    in
    over 0 t
  in
  let t = fill n in
  printf "length t %d\n" (length t);
  printf "height t %d\n" (height t);
  for i = 0 to n - 1 do
    assert (elem i t = i)
  done;
  let t = overwrite n t in
  for i = 0 to n - 1 do
    assert (elem i t = i + 1)
  done;
  printf "all checks done\n"
