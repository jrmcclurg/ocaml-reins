(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

module MonoSet = struct

  type elt = int

  type t = 
    | Empty
    | Leaf of int
    | Branch of int * int * t * t (* (prefix * branchbit * l * r) *)

  type 'a elt_ = elt
  type 'a set = t

  type 'a result = 'a
  type ('a,'b) result_ = 'a

  let of_result x = x

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let singleton x = Leaf x

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

  let rec mem x = function
    | Empty -> false
    | Leaf k -> x = k
    | Branch(p,m,t0,t1) ->
	if not (match_prefix x p m) then false
	else if zero_bit x m then mem x t0
	else mem x t1
	
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

  let add x t = 
    let rec ins = function
      | Empty -> Leaf x
      | (Leaf y) as t ->
	  if x = y then t
	  else join x (Leaf x) y t
      | Branch(p,m,t0,t1) as t ->
	  if match_prefix x p m then
	    if zero_bit x m then Branch(p,m,ins t0, t1)
	    else Branch(p,m,t0,ins t1)
	  else join x (Leaf x) p t
    in ins t

  let rec merge s t = match s,t with
    | Empty,t | t,Empty -> t
    | Leaf(x), t | t, Leaf x  -> add x t
    | Branch(p,m,s0,s1),Branch(q,n,t0,t1) ->
	if m = n && match_prefix q p m then (* same prefix, just recurse *)
	  Branch(p,m,merge s0 t0, merge s1 t1)

	else if m > n && match_prefix q p m then (* q contains p*)
	  if zero_bit q m 
	  then Branch(p,m,merge s0 t,s1)
          else Branch(p,m,s0,merge s1 t)
	else if m < n && match_prefix p q n then (* p contains q*)
	  if zero_bit p n 
	  then Branch(q,n,merge s t0,t1)
	  else Branch(q,n,t0,merge s t1)
	else (* different prefixes *)
	  join p s q t

  let rec remove x t = match t with
    | Empty -> Empty
    | Leaf y -> if x = y then Empty else t
    | Branch (p,m,t0,t1) -> 
	if match_prefix x p m then
	  if zero_bit x m 
	  then branch p m (remove x t0) t1
	  else branch p m t0 (remove x t1)
	else t

  let rec min_elt = function
    | Empty -> raise Not_found
    | Leaf x -> x
    | Branch(_,_,t0,_) -> min_elt t0

  let rec max_elt = function
    | Empty -> raise Not_found
    | Leaf x -> x
    | Branch(_,_,_,t1) -> max_elt t1

  let rec cardinal = function
    | Empty -> 0
    | Leaf _ -> 1
    | Branch (_,_,t0,t1) -> (cardinal t0) + (cardinal t1)

  let rec choose = function
    | Empty -> raise Not_found
    | Leaf k -> k
    | Branch (_, _,t0,_) -> choose t0

  let rec iter f = function
    | Empty -> ()
    | Leaf x -> f x
    | Branch(_,_,t0,t1) -> iter f t0; iter f t1
    
  let rec fold f acc t = match t with
    | Empty -> acc
    | Leaf x -> f acc x
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

  let to_string t = 
    let rec h = function
      | Empty -> ""
      | Leaf x -> string_of_int x
      | Branch(_,_,Empty,Empty) -> ""
      | Branch(_,_,subt,Empty)
      | Branch(_,_,Empty,subt) -> h subt
      | Branch(_,_,t0,t1) -> Printf.sprintf "%s, %s" (h t0) (h t1) 
    in "{" ^ (h t) ^ "}"
    
  let rec compare s t = match s,t with
    | Empty, Empty -> 0
    | Empty, _ -> -1
    | _, Empty -> 1

    | Leaf x, Leaf y -> Pervasives.compare x y
    | Leaf _, Branch _ -> -1
    | Branch _, Leaf _ -> 1

    | Branch(p,m,s0,s1),Branch(q,n,t0,t1) ->
	if p < q then -1
	else if p > q then 1 
	else if m < n then -1 
	else if m > n then 1
	else match compare s0 t0 with
	    | 0 -> compare s1 t1
	    | c -> c

  let equal s t = compare s t = 0

  let union = merge

  let rec diff s t = match s,t with
    | Empty,t -> Empty
    | s,Empty -> s
    | Leaf(x), t -> if mem x t then Empty else s
    | s, Leaf x  -> remove x s
    | Branch(p,m,s0,s1), Branch(q,n,t0,t1) ->
	if m = n && match_prefix q p m (* same prefix, just recurse *)
	then merge (diff s0 t0) (diff s1 t1)

	else if m > n && match_prefix q p m then (* q contains p*)
	  if zero_bit q m 
	  then merge (diff s0 t) s1
	  else merge s0 (diff s1 t)

	else if m < n && match_prefix p q n then (* p contains q*)
	  if zero_bit p n
	  then diff s t0
	  else diff s t1

	else (* different prefixes *)
	  s

  let rec inter s t =  match s,t with
    | Empty,_ -> Empty | _,Empty -> Empty
    | (Leaf x as lf), t -> if mem x t then lf else Empty
    | t, (Leaf x as lf) -> if mem x t then lf else Empty
    | Branch(p,m,s0,s1), Branch(q,n,t0,t1) ->
	if m = n && match_prefix q p m (* same prefix, just recurse *)
	then merge (inter s0 t0) (inter s1 t1)

	else if m > n && match_prefix q p m then (* q contains p *)
	  if zero_bit q m 
	  then inter s0 t
	  else inter s1 t

	else if m < n && match_prefix p q n then (* p contains q *)
	  if zero_bit p n 
	  then inter s t0
	  else inter s t1

	else (* different prefixes *)
	  Empty

  let gen1 (agen : (?size:int -> Random.State.t -> int)) ?(size=50) rs = 
    let num = Random.State.int rs size in
    let rec loop n t = 
      if n <= 0 then t
      else loop (n-1) (add (agen ~size:size rs) t)
    in
      loop num empty

  let gen ?size rs = gen1 Types.Int.gen ?size rs

  type path = 
    | Top
    | PathL of path *  t
    | PathR of t * path

  type cursor = path * t
  type 'a cursor_ = cursor

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
    | PathL(p,r),l
    | PathR(l,p),r -> p, (merge l r)

  let move_down_right (p,t) = match t with
    | Empty | Leaf _ -> failwith "move_down_right"
    | Branch(_,_,_,r) -> PathR(t,p),r

  let move_down_left (p,t) = match t with 
    | Empty | Leaf _ -> failwith "move_down_left"
    | Branch(_,_,l,_) -> PathL(p,t),l

  let has_value = function _,Leaf _ -> true | _ -> false

  let get_value = function
    | _,Leaf v -> v
    | _,_ -> failwith "get_value"

  let rec from_cursor curs = 
    if at_top curs then snd curs
    else from_cursor (move_up curs)

end

module GenSet = MonoSet
