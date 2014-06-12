open Container
open Term

type type_term   = term
type constraints = type_term array




module Term_sub_arr: sig

  type t
  val make: int -> t
  val count: t -> int
  val get:   int -> t -> term
  val flags: t -> bool array
  val args:  t -> term array
  val has:   int -> t -> bool
  val add:   int -> term -> t -> unit
  val extend:int -> t -> t
  val extend_bottom: int -> t -> t
  val remove_bottom: int -> t -> t

end = struct

  type t = {args: term array; flags: bool array}

  let flags (s:t): bool array = s.flags
  let args  (s:t): term array = s.args


  let make (n:int): t =
    {args  = Array.init n (fun i -> Variable i);
     flags = Array.make n false}

  let count (s:t): int = Array.length s.args

  let has (i:int) (s:t): bool =
    assert (i < count s);
    s.flags.(i)

  let get (i:int) (s:t): term =
    assert (i < (count s));
    s.args.(i)

  let add (i:int) (t:term) (s:t): unit =
    (** Add the substitution [i ~~> t] to the substitution [s] i.e.
        apply to [t] all already available substitutions and check for
        circularity and apply [i ~~> t] to all available substitutions.
     *)
    let n = count s in
    assert (i <= n);
    assert (not (Term.is_variable_i t i));
    let t = Term.sub t s.args n in
    if IntSet.mem i (Term.bound_variables t n) then
      raise Not_found (* circular substitution *)
    else begin
      Array.iteri
        (fun j e ->
          if s.flags.(j) then s.args.(j) <- Term.sub_var i e t
          else ())
        s.args;
      if not s.flags.(i) then
        (s.args.(i)<-t; s.flags.(i)<-true)
      else if t = s.args.(i) then
        ()
      else
        raise Not_found
    end

  let extend (n:int) (s:t): t =
    (** Introduce [n] new variables at the top, i.e. all substitutions term
        above [count s] are shifted up by [n] and just copied into the new
        larger substitution.
     *)
    let sn   = count s in
    let args = Array.map (fun t -> Term.upbound n sn t) s.args in
    let snew = make (n+sn) in
    Array.blit args    0  snew.args  0  sn;
    Array.blit s.flags 0  snew.flags 0  sn;
    snew


  let extend_bottom (n:int) (s:t): t =
    (** Introduce [n] new variables at the bottom, i.e. shift all
        terms up by [n].
     *)
    let sn   = count s in
    let snew = make (n+sn) in
    Array.iteri (fun i t -> snew.args.(i+n) <- Term.up n t) s.args;
    Array.blit s.flags 0 snew.flags n  sn;
    snew



  let remove_bottom (n:int) (s:t): t =
    (** Remove [n] variables from the bottom, i.e. shift all
          terms down by [n].
     *)
    let sn = count s in
    assert (n <= sn);
    let snew = make (sn-n) in
    Array.iteri
      (fun i _ -> snew.args.(i) <- Term.down n s.args.(i+n)) snew.args;
    Array.blit s.flags n   snew.flags 0   (sn-n);
    snew
  end (* Term_sub_arr *)






module TVars: sig

  type t
  val make: int -> constraints -> t
  val make_local: int -> t
  val count_local: t -> int
  val count_global: t -> int
  val count: t -> int
  val constraints: t -> constraints
  val add_global: constraints -> t -> t
  val add_local:  int -> t -> t
  val remove_local: int -> t -> t

end = struct

  type t = {nlocal:int; constraints: constraints}

  let make (ntvs:int) (cs:constraints): t = {nlocal=ntvs;constraints=cs}
  let make_local (ntvs:int) : t           = {nlocal=ntvs;constraints=[||]}
  let count_local (tvs:t): int = tvs.nlocal
  let count_global (tvs:t): int = Array.length tvs.constraints
  let count (tvs:t): int = tvs.nlocal + (count_global tvs)
  let constraints (tvs:t): constraints = tvs.constraints
  let add_global (cs:constraints) (tvs:t): t =
    {tvs with constraints = Array.append tvs.constraints cs}
  let add_local (n:int) (tvs:t): t =
    {tvs with nlocal = tvs.nlocal + n}
  let remove_local (n:int) (tvs:t): t =
    assert (n <= (count_local tvs));
    {tvs with nlocal = tvs.nlocal - n}
end (* TVars *)




module TVars_sub: sig

  type t
  val make:         int -> t
  val count:        t -> int
  val get:          int -> t -> term
  val count_global: t -> int
  val count_local:  t -> int
  val concept:      int -> t -> term
  val tvars:        t -> TVars.t
  val sub:          t -> Term_sub_arr.t
  val args:         t -> term array
  val add_substitution: int -> term -> t -> unit
  val add_global:   constraints -> t -> t
  val add_local:    int -> t -> t
  val remove_local: int -> t -> t
  val update:       t -> t -> unit

end = struct

  type t = {vars: TVars.t;
            sub:  Term_sub_arr.t}

  let make (ntvs: int): t =
    {vars = TVars.make_local ntvs; sub = Term_sub_arr.make ntvs}

  let count (tvars:t): int = TVars.count tvars.vars

  let get (i:int) (tvars:t): term =
    assert (i < (count tvars));
    Term_sub_arr.get i tvars.sub

  let count_global (tv:t): int =
    TVars.count_global tv.vars

  let count_local (tv:t): int =
    TVars.count_local tv.vars

  let concept (i:int) (tv:t): term =
    assert (count_local tv <= i);
    assert (i < count tv);
    (TVars.constraints tv.vars).(i - count_local tv)

  let tvars (tv:t): TVars.t = tv.vars

  let sub (tv:t): Term_sub_arr.t = tv.sub

  let add_substitution (i:int) (t:term) (tv:t): unit =
    Term_sub_arr.add i t tv.sub

  let args (tv:t): term array = Term_sub_arr.args tv.sub

  let add_global (cs:constraints) (tv:t): t =
    {vars = TVars.add_global cs tv.vars;
     sub  = Term_sub_arr.extend (Array.length cs) tv.sub}

  let add_local (n:int) (tv:t): t =
    (** Add [n] local (fresh) type variables without constraints to [tv]
        and shift all type variables up by [n].
     *)
    {vars = TVars.add_local n tv.vars;
     sub  = Term_sub_arr.extend_bottom n tv.sub}

  let remove_local (n:int) (tv:t): t =
    (** Remove [n] local type variables (without constraints) from [tv] and
        shift all type variables down by [n].
     *)
    {vars = TVars.remove_local n tv.vars;
     sub  = Term_sub_arr.remove_bottom n tv.sub}

  let update (tv:t) (tvnew:t): unit =
    (** Update the type variables in [tv] with the type variables in [tvnew].

        This requires that [tv] and [tvnew] have the same number of local type
        variables and [tvnew] might have more globals than [tv]
     *)
    assert ((count tv) <= (count tvnew));
    assert ((count_local tv) = (count_local tvnew));
    let nloc  = count_local tv
    and ndown = (count_global tvnew) - (count_global tv)
    in
    for i=0 to nloc-1 do
      if Term_sub_arr.has i tvnew.sub then
        Term_sub_arr.add
          i
          (Term.down_from ndown nloc (Term_sub_arr.args tvnew.sub).(i))
          tv.sub
    done

end (* TVars_sub *)



module Result_type: sig

  type t
  val empty:        t
  val make_func:    type_term -> t
  val make_proc:    type_term -> t
  val has_result:   t -> bool
  val result:       t -> type_term
  val is_procedure: t -> bool
  val up_from:      int -> int -> t -> t
  val up:           int -> t -> t

end = struct

  type t = (type_term * bool) option
  let empty = None
  let make_func (tp:type_term): t = Some (tp,false)
  let make_proc (tp:type_term): t = Some (tp,true)

  let has_result (rt:t): bool = Option.has rt

  let result(rt:t): type_term =
    assert (has_result rt);
    match rt with
      None -> assert false
    | Some (tp,proc) -> tp

  let  is_procedure (rt:t): bool =
    match rt with
      None -> true
    | Some (_,proc) -> proc

  let up_from (n:int) (start:int) (rt:t): t =
    match rt with
      None -> None
    | Some (tp,proc) -> Some (Term.upbound n start tp, proc)

  let up (n:int) (rt:t): t = up_from n 0 rt
end





module Sign: sig
  type t
  val empty:       t
  val make:        type_term array -> Result_type.t -> t
  val make_func:   type_term array -> type_term -> t
  val make_proc:   type_term array -> type_term -> t
  val make_const:  type_term -> t
  val make_args:   type_term array -> t
  val to_string:   t -> string
  val arity:       t -> int
  val is_constant: t -> bool
  val arguments:   t -> type_term array
  val arg_type:    int -> t -> type_term
  val argument:    int -> t -> t
  val result_type: t -> Result_type.t
  val has_result:  t -> bool
  val is_binary:   t -> bool
  val is_unary:    t -> bool
  val result:      t -> type_term
  val is_procedure:t -> bool
  val up_from:     int -> int -> t -> t
  val up:          int -> t -> t
  val up2:         int -> int -> int -> t -> t
  val to_function: int -> t -> t

end = struct

  type t = {args: type_term array;
            rt:   Result_type.t}

  let empty: t = {args = [||]; rt = Result_type.empty (*result = None*)}

  let make (args: type_term array) (rt:Result_type.t): t =
    {args = args; rt = rt}

  let make_func (args: type_term array) (result:type_term): t =
    {args = args; rt = Result_type.make_func result}

  let make_args (args: type_term array): t =
    {args = args; rt = Result_type.empty}

  let make_const (result:type_term): t =
    {args = [||]; rt = Result_type.make_func result}

  let make_proc (args: type_term array) (result:type_term): t =
    {args = args; rt = Result_type.make_proc result}

  let arity (s:t): int = Array.length s.args

  let is_constant (s:t): bool = (arity s) = 0

  let arguments (s:t): type_term array = s.args

  let arg_type (i:int) (s:t): type_term =
    assert (i < (arity s));
    s.args.(i)

  let argument (i:int) (s:t): t =
    assert (i < (arity s));
    make_func [||] s.args.(i)

  let result_type (s:t): Result_type.t = s.rt

  let has_result (s:t): bool = Result_type.has_result s.rt

  let is_binary (s:t): bool = (has_result s) && ((arity s) = 2)
  let is_unary  (s:t): bool = (has_result s) && ((arity s) = 1)

  let result (s:t): type_term =
    assert (has_result s);
    Result_type.result s.rt

  let is_procedure (s:t): bool = Result_type.is_procedure s.rt


  let to_string (s:t): string =
    let argsstr =
      if (arity s) = 0 then ""
      else
        "("
        ^ (String.concat
             ","
             (List.map Term.to_string (Array.to_list s.args)))
        ^ ")"
        ^ (if has_result s then ":" else "")
    and retstr =
      if has_result s then Term.to_string (result s)
      else ""
    in
    argsstr ^ retstr

  let up_from (n:int) (start:int) (s:t): t =
    (** Shift all types up by [n] starting from [start].
     *)
    {args = Array.map (fun t -> Term.upbound n start t) s.args;
     rt   = Result_type.up_from n start s.rt}


  let up (n:int) (s:t): t =
    (** Shift all types up by [n].
     *)
    up_from n 0 s


  let up2 (n1:int) (start:int) (n2:int) (s:t): t =
    (** Shift all types up by [n1] starting from type [start] and then
        shift all types up by [n2] i.e. the operation creates a hole
        of [n1] starting from [start+n2] and a hole of [n2] starting from
        0.
     *)
    up n2 (up_from n1 start s)



  let to_function (nargs:int) (s:t): t =
    (** Convert the constant signature [s] into a function signature with
        [nargs] arguments. The [nargs] argument types are fresh type variables,
        therefore the result type of [s] has to be shifted up by [nargs].
     *)
    assert (has_result s);
    assert (is_constant s);
    {args   = Array.init nargs (fun i -> Variable i);
     rt     = Result_type.up nargs s.rt}

end (* Sign *)
