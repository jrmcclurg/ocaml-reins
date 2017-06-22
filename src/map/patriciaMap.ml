(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

module Map_ = struct

  type key = int
  type 'a key_ = key

  type 'a tree = 
    | Empty
    | Leaf of int * 'a 
    | Branch of int * int * 'a tree * 'a tree (* (prefix * branchbit * l * r) *)

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let singleton k v = Leaf(k,v)

  let zero_bit k m = (k land m) = 0

  let mask k m = (k lor (m-1)) land (lnot m)

  let match_prefix k p m = (mask k m) = p
    
  let lowest_bit x = x land (-x)

  let highest_bit x m = 
    let x' = x land (lnot (m-1)) in
    let rec highb x = 
      let m = lowest_bit x in
	if x = m then m else highb (x-m)
    in highb x'

  let branching_bit p0 m0 p1 m1 = 
    highest_bit (p0 lxor p1) (max 1 (2*(max m0 m1)))

  let rec find x = function
    | Empty -> raise Not_found
    | Leaf(k,v) -> if x = k then v else raise Not_found
    | Branch(p,m,t0,t1) ->
	if not (match_prefix x p m) then raise Not_found
	else if zero_bit x m then find x t0
	else find x t1

  let mem x t = try ignore(find x t);true with Not_found -> false

  let branch p m t1 t2 = match t1,t2 with
    | Empty, t | t, Empty -> t
    | _ -> Branch(p,m,t1,t2)

  let get_branch_bit = function
    | Empty | Leaf _ -> 0
    | Branch(_,b,_,_) -> b

  let join p0 t0 p1 t1 = 
    let m = branching_bit p0 (get_branch_bit t0) p1 (get_branch_bit t1) in
      if zero_bit p0 m then Branch(mask p0 m, m, t0, t1)
      else Branch(mask p0 m, m, t1, t0)

  let add k v t = 
    let rec ins = function
      | Empty -> Leaf(k,v)
      | Leaf(k',v') as t ->
	  if k = k' then Leaf(k,v) (* repalce binding *)
	  else join k (Leaf(k,v)) k' t
      | Branch(p,m,t0,t1) as t ->
	  if match_prefix k p m then
	    if zero_bit k m then Branch(p,m,ins t0, t1)
	    else Branch(p,m,t0,ins t1)
	  else join k (Leaf(k,v)) p t
    in ins t

  let rec merge f s t = match s,t with
    | Empty,t | t,Empty -> t
    | Leaf(k,v), t | t, Leaf(k,v)  -> 
	begin try
	  let v' = find k t in
	    if v == v' then add k v t
	    else add k (f k v v') t
	with Not_found -> add k v t
	end
    | Branch(p,m,s0,s1),Branch(q,n,t0,t1) ->
	if m = n && match_prefix q p m then (* same prefix, just recurse *)
	  Branch(p,m,merge f s0 t0, merge f s1 t1)

	else if m > n && match_prefix q p m then (* q contains p*)
	  if zero_bit q m 
	  then Branch(p,m,merge f s0 t,s1)
          else Branch(p,m,s0,merge f s1 t)
	else if m < n && match_prefix p q n then (* p contains q*)
	  if zero_bit p n 
	  then Branch(q,n,merge f s t0,t1)
	  else Branch(q,n,t0,merge f s t1)
	else (* different prefixes *)
	  join p s q t

  let rec remove x t = match t with
    | Empty -> Empty
    | Leaf(k,v) -> if x = k then Empty else t
    | Branch (p,m,t0,t1) -> 
	if match_prefix x p m then
	  if zero_bit x m 
	  then branch p m (remove x t0) t1
	  else branch p m t0 (remove x t1)
	else t

  let rec min_key = function
    | Empty -> raise Not_found
    | Leaf(k,_) -> k
    | Branch(_,_,t0,_) -> min_key t0

  let rec max_key = function
    | Empty -> raise Not_found
    | Leaf(k,_) -> k
    | Branch(_,_,_,t1) -> max_key t1

  let rec min_keyval = function
    | Empty -> raise Not_found
    | Leaf(k,v) -> k,v
    | Branch(_,_,t0,_) -> min_keyval t0

  let rec max_keyval = function
    | Empty -> raise Not_found
    | Leaf(k,v) -> k,v
    | Branch(_,_,_,t1) -> max_keyval t1

  let rec cardinal = function
    | Empty -> 0
    | Leaf _ -> 1
    | Branch (_,_,t0,t1) -> (cardinal t0) + (cardinal t1)

  let rec iter f = function
    | Empty -> ()
    | Leaf(k,v) -> f k v
    | Branch(_,_,t0,t1) -> iter f t0; iter f t1
    
  let rec fold f acc t = match t with
    | Empty -> acc
    | Leaf(k,v) -> f acc k v
    | Branch (_,_,t0,t1) -> fold f (fold f acc t0) t1

  let rec no_empty_under_branch = function
    | Empty -> true
    | Leaf _ -> true
    | Branch(_,_,Empty,_)
    | Branch(_,_,_,Empty) -> false
    | Branch(_,_,t0,t1) -> 
	(no_empty_under_branch t0) && (no_empty_under_branch t1)
      
  let well_formed t = 
    no_empty_under_branch t

  let rec to_string to_s t = 
    let rec h = function
      | Empty -> ""
      | Leaf(k,v) -> Printf.sprintf "(%d => %s)" k (to_s v)
      | Branch(_,_,subt,Empty) -> h subt
      | Branch(_,_,Empty,subt) -> h subt
      | Branch(_,_,t0,t1) -> Printf.sprintf "%s, %s" (h t0) (h t1) 
    in "{" ^ (h t) ^ "}"
    
  let rec compare cmp s t = match s,t with
    | Empty, Empty -> 0
    | Empty, _ -> -1
    | _, Empty -> 1

    | Leaf(lk,lv), Leaf(rk,rv) -> 
	(** pervasives is ok since keys always have type int *)
	let res = Pervasives.compare lk rk in
	  if res = 0 then cmp lv rv
	  else res
    | Leaf _, Branch _ -> -1
    | Branch _, Leaf _ -> 1

    | Branch(p,m,s0,s1),Branch(q,n,t0,t1) ->
	if p < q then -1
	else if p > q then 1 
	else if m < n then -1 
	else if m > n then 1
	else match compare cmp s0 t0 with
	  | 0 -> compare cmp s1 t1
	  | c -> c

  let compare_keys s t = compare (fun _ _ -> 0) s t

  let rec equal elt_eq s t =  match s,t with
    | Empty, Empty -> true
    | Empty, _ | _, Empty -> false

    | Leaf(lk,lv), Leaf(rk,rv) -> (lk = rk) && (elt_eq lv rv)

    | Leaf _, Branch _ | Branch _, Leaf _ -> false

    | Branch(p,m,s0,s1),Branch(q,n,t0,t1) ->
	(p=q) && (m=n) && equal elt_eq s0 t0 && equal elt_eq s1 t1

  let union = merge

  (** if k is not in t then return s.  Otherwise if f returns true
      when applied to the k and the respective values, return s with k
      removed.  Otherwise return s unchanged. *)
  let remove_if f k v s t = 
    begin try 
      let v' = find k t in
	if f k v v' (* are they "equal" in the user's eyes? *)
	then remove k s (* yes, remove the leaf *)
	else s (* no keep the leaf *)
    with Not_found -> s
    end

  let never_merge k v v = assert false

  let rec diff f s t = match s,t with
    | Empty,t -> Empty
    | s,Empty -> s
    | Leaf(k,v), t -> remove_if f k v s t
    | s, Leaf(k,v) -> remove_if f k v s s
    | Branch(p,m,s0,s1), Branch(q,n,t0,t1) ->
	if m = n && match_prefix q p m (* same prefix, just recurse *)
	then merge never_merge (diff f s0 t0) (diff f s1 t1)

	else if m > n && match_prefix q p m then (* q contains p*)
	  if zero_bit q m 
	  then merge never_merge (diff f s0 t) s1
	  else merge never_merge s0 (diff f s1 t)

	else if m < n && match_prefix p q n then (* p contains q*)
	  if zero_bit p n
	  then diff f s t0
	  else diff f s t1

	else (* different prefixes *)
	  s

  let rec inter f s t =  match s,t with
    | Empty,_ -> Empty | _,Empty -> Empty

    | Leaf(k,v), t
    | t, Leaf(k,v) -> 
	begin try
	  let v' = find k t in
	    Leaf(k,f k v v')
	with Not_found -> Empty
	end

    | Branch(p,m,s0,s1), Branch(q,n,t0,t1) ->
	if m = n && match_prefix q p m (* same prefix, just recurse *)
	then merge never_merge (inter f s0 t0) (inter f s1 t1)

	else if m > n && match_prefix q p m then (* q contains p *)
	  if zero_bit q m 
	  then inter f s0 t
	  else inter f s1 t

	else if m < n && match_prefix p q n then (* p contains q *)
	  if zero_bit p n 
	  then inter f s t0
	  else inter f s t1

	else (* different prefixes *)
	  Empty

  let rec mapi f = function
    | Empty -> Empty
    | Leaf(k,v) -> Leaf(k, f k v)
    | Branch(p,m,l,r) -> Branch(p,m,mapi f l, mapi f r)

  let map f t = mapi (fun _ v -> f v) t

  let gen2
      (kgen : (?size:int -> Random.State.t -> int))
      (vgen : (?size:int -> Random.State.t -> 'v))
      ?(size=50) rs : 'v tree = 
    let num = Random.State.int rs size in
    let rec loop n t = 
      if n <= 0 then t
      else
	let k = kgen ~size:size rs in
	let v = vgen ~size:size rs in
	  loop (n-1) (add k v t)
    in
      loop num empty

  let gen1 (vgen : (?size:int -> Random.State.t -> 'v))
      ?size rs : 'v tree = 
    gen2 Types.Int.gen vgen ?size rs
      
  type 'a path = 
    | Top
    | PathL of 'a path * 'a tree
    | PathR of 'a tree * 'a path

  type 'a curs = 'a path * 'a tree

  let to_cursor t = Top,t

  let at_top = function
    | Top,_ -> true
    | _ -> false

  let at_right = function
    | _, Empty
    | _,Leaf _ -> true
    | _ -> false

  let at_left = at_right

  let went_left = function PathL _,_ -> true | _ -> false
  let went_right = function PathR _,_ -> true | _ -> false

  let move_up = function
    | Top, _ -> failwith "move_up"
    | PathL(p,r),l -> p, (merge (fun k v1 v2 -> v1) l r)
	(* we use the join function to choose the binding which
	   was modified in by values in the subtree of the cursor *)
    | PathR(l,p),r -> p, (merge (fun k v1 v2 -> v2) l r)

  let move_down_right (p,t) = match t with
    | Empty | Leaf _ -> failwith "move_down_right"
    | Branch(_,_,_,r) -> PathR(t,p),r

  let move_down_left (p,t) = match t with 
    | Empty | Leaf _ -> failwith "move_down_left"
    | Branch(_,_,l,_) -> PathL(p,t),l

  let has_value = function _,Leaf _ -> true | _ -> false

  let get_value = function
    | _,Leaf(k,v) -> k,v
    | _,_ -> failwith "get_value"

  let rec from_cursor curs = 
    if at_top curs then snd curs
    else from_cursor (move_up curs)

  let of_result x = x
end


module MonoKeyMap = struct
  include Map_

  type ('k,'v) map = 'v tree
  type 'a t = 'a tree

  type 'e elt = 'e
  type 'e elt_ = 'e

  type 'v cursor = 'v curs
  type ('k,'v) cursor_ = 'v cursor

  type ('a,'e) result = 'a
  type ('a,'k,'e) result_ = 'a

end

module GenKeyMap = MonoKeyMap

module MonoMap(C : Types.Mono.Comparable) = struct

  include Map_
    
  type ('k,'v) map = C.t tree

  type t = C.t tree
  type elt = C.t
  type 'e elt_ = C.t

  type cursor = C.t curs
  type ('k,'v) cursor_ = cursor

  type 'a result = 'a
  type ('a,'k,'e) result_ = 'a

  let compare x y = compare C.compare x y
  let to_string t = to_string C.to_string t
end

module GenMap(C : Types.Mono.ArbitraryComparable) = struct
  include MonoMap(C)

  let gen ?size rs = gen1 C.gen ?size rs
end
