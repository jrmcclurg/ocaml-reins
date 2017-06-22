(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Types

(** The main functor for implementing sets.  The paramater field
    HeightDiff.v specifies the maximum difference between the heights
    of two subtrees joined at a node.
*)
module BaseSet (HeightDiff : sig val v : int end) = struct

(** The types of AVL trees.  An element can be either stored in a Leaf
    if it has no children, or in a Node if it has at least 1 child.
    The constructor Node(l,v,r,h) also contains the left branch 'l' (all
    elements are smaller than v), the right branch 'r' (all elements
    greater than v) and the heigh of the tree at that point. 
*)
  type 'a tree = 
      | Empty
      | Leaf of 'a
      | Node of 'a tree * 'a * 'a tree * int

  let of_result x = x

  let empty = Empty      

  let singleton x = Leaf x
    
  let is_empty = function
    | Empty -> true
    | _ -> false

  let rec mem cmp x = function
    | Empty -> false
    | Leaf y -> (cmp x y) = 0
    | Node(l,y,r,_) -> match cmp x y with
	| 0 -> true
	| c when c < 0 -> mem cmp x l
	| _ -> mem cmp x r

  let rec fold f acc t = match t with
    | Empty -> acc
    | Leaf x -> f acc x
    | Node(l,v,r,_) ->
	fold f (f (fold f acc l) v) r

  let rec iter f t = match t with
    | Empty -> ()
    | Leaf x -> f x
    | Node(l,v,r,_) ->
	iter f l; f v; iter f r

  let rec min_elt = function
    | Empty -> raise Not_found
    | Leaf x -> x
    | Node(Empty,v,_,_) -> v
    | Node(l,_,_,_) -> min_elt l

  let rec max_elt = function
    | Empty -> raise Not_found
    | Leaf x -> x
    | Node(_,v,Empty,_) -> v
    | Node(_,_,r,_) -> max_elt r

  let height = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node(_,_,_,h) -> h

  (** N-"smart" constructor (a la Stephen Adams).   This function
      chooses the right constructor based on the number of children
      and ensures that the Node constructor is well formed.
  *)
  let node l v r = 
    match height l, height r with
      | 0,0 -> Leaf v
      | hl,hr -> Node(l,v,r, (max hl hr)+1)
	  
  let pivot ll lv c rv rr = match c with
    | Node(cl,cv,cr,_) ->
	node (node ll lv cl) cv (node cr rv rr)
    | Leaf cv ->
	node (node ll lv Empty) cv (node Empty rv rr)
    | Empty -> assert false

  (** This function will fix the tree if the left subtree has a height
      at most HeightDiff.v +1 more than that of the right subtree. *)
  let rebal_left ll lv lr v r =
    if height ll >= height lr
    then node ll lv (node lr v r)
    else pivot ll lv lr v r

  (** This function will fix the tree if the right subtree has a
      height at most HeightDiff.v +1 more than that of the left
      subtree. *)
  let rebal_right l v rl rv rr = 
    if height rr >= height rl 
    then node (node l v rl) rv rr
    else pivot l v rl rv rr

  (** T'-"smart" constructor: fixes trees by performing at most 1
      rotation. *)
  let rotate l v r = 
    match l,r with
        (* Height 1 tree *)
      | Empty, Empty -> Leaf v
          
      (* Height 2 tree *)
      | Empty, Leaf _
      | Leaf _, Empty
      | Leaf _, Leaf _ -> Node(l,v,r,2)
          
      (* General Height 'h' *)
      | Node(ll,lv,lr,h), Empty -> 
          if h > HeightDiff.v 
	  then rebal_left ll lv lr v r
          else Node(l,v,r,h+1)
      | Empty, Node(rl,rv,rr,h) ->
          if h > HeightDiff.v 
	  then rebal_right l v rl rv rr
          else Node(l,v,r,h+1)

      | Leaf _, Node(_,_,_,h)         (* 1 + for Leaf _ *)
      | Node(_,_,_,h), Leaf _ when h <= (1 + HeightDiff.v) ->
          Node(l,v,r,h+1)

      | Leaf _, Node(rl,rv,rr,h) -> rebal_right l v rl rv rr
      | Node(ll,lv,lr,h), Leaf _  -> rebal_left ll lv lr v r

      | Node(ll,lv,lr,lh), Node(rl,rv,rr,rh) -> 
	  if lh > rh + HeightDiff.v
	  then rebal_left ll lv lr v r
	  else if rh > lh + HeightDiff.v 
	  then rebal_right l v rl rv rr
	  else node l v r

  let rec add cmp newe t = match t with
    | Empty -> Leaf newe
    | Leaf elt ->
        begin match cmp newe elt with
          | 0 -> t
          | c when c < 0 -> Node(Empty,newe,t,2)
          | _ -> Node(t, newe, Empty,2)
        end
    | Node(l,elt,r,_) ->
        match cmp newe elt with
          | 0 -> t
          | c when c < 0 -> rotate (add cmp newe l) elt r
          | _ -> rotate l elt (add cmp newe r)

  let rec get_and_remove_min = function
    | Empty -> raise (Invalid_argument "get_and_remove_min")
    | Leaf elt -> elt, Empty
    | Node(Empty,elt,r,h) -> elt, r
    | Node(l,elt,r,h) -> 
	let d,newl = get_and_remove_min l in
	  d, rotate newl elt r
	
  let rec get_and_remove_max = function
    | Empty -> raise (Invalid_argument "get_and_remove_max")
    | Leaf elt -> elt, Empty
    | Node(l,elt,Empty,h) -> elt, l
    | Node(l,elt,r,h) -> 
	let d,newr = get_and_remove_max r in
	  d, rotate l elt newr

  let rec remove cmp dele t = match t with
    | Empty -> Empty
    | Leaf elt
    | Node(Empty,elt,Empty,_) -> 
	if (cmp dele elt) = 0 then Empty else Leaf elt

    | Node(l,elt,r,_) -> match cmp dele elt with
	| 0 -> 
	    if r = Empty then l
	    else if l = Empty then r else
	      let d,newr = get_and_remove_min r in
		rotate l d newr

 	| c when c < 0 -> rotate (remove cmp dele l) elt r
	| _ -> rotate l elt (remove cmp dele r)

  (** join trees of arbitrary size *)
  let rec concat3 cmp l v r  = match l,r with
    | Empty, r -> add cmp v r
    | l, Empty -> add cmp v l
    | Leaf x, Leaf y -> node l v r
    | Leaf x, Node(l2,v2,r2,h) -> 
	if h > (1 + HeightDiff.v)
	then rotate (concat3 cmp l v l2) v2 r2
	else node l v r
    | Node(l1,v1,r1,h), Leaf x -> 
	if h > (1 + HeightDiff.v)
	then rotate l1 v1 (concat3 cmp r1 v r)
	else node l v r
    | Node(l1,v1,r1,h1),Node(l2,v2,r2,h2) ->
	if h2 > h1 + HeightDiff.v 
	then rotate (concat3 cmp  l v l2) v2 r2
	else if h1 > h2 + HeightDiff.v
	then rotate l1 v1 (concat3 cmp r1 v r)
	else node l v r

  (* equivalent to (split_lt v t), (split_gt v t) *)
  let rec split cmp v t = match t with
    | Empty -> Empty, Empty
    | Leaf elt -> begin match cmp v elt with
	  | 0 -> Empty,Empty 
	  | c when c < 0 -> Empty,t
	  | _ -> t,Empty
      end
    | Node(l1,elt,r1,_) ->
	match cmp v elt with
	  | 0 -> l1,r1
	  | c when c < 0 ->
	      let l2,r2 = split cmp v l1 in
		(l2,concat3 cmp r2 elt r1)
	  | _ ->
	      let l2,r2 = split cmp v r1 in
		(concat3 cmp l1 elt l2), r2

  let rec union cmp t1 t2 = match t1,t2 with
    | Empty, t | t, Empty -> t
    | Leaf x,r -> add cmp x r
    | l,Leaf x -> add cmp x l
    | t1, Node(l,v,r,_) ->
	let l',r' = split cmp v t1 in
	  concat3 cmp (union cmp l' l) v (union cmp r' r)

  let rec concat t1 t2 = match t1,t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | Leaf x, Leaf y -> Node(t1,y,Empty,2)
    | Leaf x, Node(l2,v2,r2,h) ->
	if h > 1+HeightDiff.v 
	then rotate (concat t1 l2) v2 r2
	else 
	  let m,t2' = get_and_remove_min t2 in
	    rotate t1 m t2'
    | Node(l1,v1,r1,h), Leaf x ->
	if h > 1+HeightDiff.v 
	then rotate l1 v1 (concat r1 t2)
	else rotate t1 x Empty (* inline get_*_min for Leaf *)
    | Node(l1,v1,r1,h1), Node(l2,v2,r2,h2) ->
	if h2 > h1 + HeightDiff.v
	then rotate (concat t1 l2) v2 r2
	else if h1 > h2 + HeightDiff.v 
	then rotate l1 v1 (concat r1 t2)
	else 
	  let m,t2' = get_and_remove_min t2 in
	    rotate t1 m t2'

  let rec diff cmp t1 t2 = match t1,t2 with
    | Empty, _ -> Empty 
    | _, Empty -> t1
    | _, Leaf y -> remove cmp y t1
    | _, Node(l,v,r,_) ->
	let l',r' = split cmp v t1 in
	  concat (diff cmp l' l) (diff cmp r' r)

  let rec inter cmp t1 t2 = match t1,t2 with
    | Empty,_ | _,Empty -> Empty
    | t1, Leaf x -> if mem cmp x t1 then t2 else Empty
    | t1, Node(l,v,r,_) ->
	let l',r' = split cmp v t1 in
	  if mem cmp v t1 
	  then concat3 cmp (inter cmp l' l) v (inter cmp r' r)
	  else concat (inter cmp l' l) (inter cmp r' r)
	  
  let choose = function
    | Empty -> raise Not_found
    | Leaf x -> x
    | Node(_,x,_,_) -> x
  
  let rec cardinal = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node(l,_,r,_) -> 1 + (cardinal l) + (cardinal r)

  let rec cmp c x y = 
    match (is_empty x), (is_empty y) with
      | true, true -> 0
      | true, false -> -1
      | false, true -> 1
      | false, false ->
	  let xm = min_elt x in
	  let ym = min_elt y in
	    match c xm ym with
	      | 0 -> cmp c (remove c xm x) (remove c ym y)
	      | v -> v

  let rec well_ordered cmp = function
    | Empty -> true
    | Leaf _ -> true
    | Node(Empty,_,Empty,_) -> true
    | Node(((Leaf x)|Node(_,x,_,_) as l),elt,Empty,_) -> 
	(well_ordered cmp l) && (cmp x elt < 0)
    | Node(Empty,elt,((Leaf x)|Node(_,x,_,_) as r),_) -> 
	(well_ordered cmp r) && (cmp x elt > 0)

    | Node(((Leaf lx)|Node(_,lx,_,_) as l)
	      ,elt,
	  ((Leaf rx)|Node(_,rx,_,_) as r),
	  _) -> 
	(well_ordered cmp l) && (well_ordered cmp r) && 
	  (cmp lx elt < 0) && (cmp rx elt > 0)	

  let well_formed_height = function
    | Empty | Leaf _ -> true
    | Node(l,v,r,h) ->
	let hl = height l in
	let hr = height r in
	  (h = (max hl hr) + 1) &&
	    (abs (hl - hr) <= HeightDiff.v)

  let rec well_formed cmp t = (well_ordered cmp t) && (well_formed_height t)

  type 'a path = 
    | Top
    | PathL of 'a path * 'a * 'a tree
    | PathR of 'a tree * 'a * 'a path

  type 'a curs = 'a path * 'a tree

  let to_cursor t = Top,t

  let at_top (p,t) = (p = Top)

  let at_left (p,t) = match t with
    | Empty | Leaf _ -> true
    | _ -> false

  let at_right (p,t) = match t with
    | Empty | Leaf _ -> true
    | _ -> false
	
  let went_left (p,t) = match p with
    | PathL _ -> true
    | _ -> false

  let went_right (p,t) = match p with
    | PathR _ -> true
    | _ -> false

  let move_up = function
    | Top, _ -> failwith "move_up"
    | PathL(p,x,r),l | PathR(l,x,p),r -> p, (node l x r)

  let move_down_left = function
    | _,Empty
    | _, Leaf _ -> failwith "move_down_left"
    | p, Node(l,v,r,h) -> PathL(p,v,r),l

  let move_down_right = function
    | _,Empty
    | _, Leaf _ -> failwith "move_down_right"
    | p,Node(l,v,r,h) -> PathR(l,v,p),r

  let rec from_cursor ((p,t) as curs) = 
    if at_top curs then t
    else from_cursor (move_up curs)

  let has_value (p,t) = match t with Empty -> false | _ -> true

  let get_value = function
    | _,Empty -> failwith "get_value"
    | _,Leaf x
    | _,Node(_,x,_,_) -> x

  let rec move_to_ancestor cmp x ((p,t) as curs) = match p with
    | Top -> curs
    | PathL(p', v, r) -> 
	if cmp x v < 0 then curs 
	else move_to_ancestor cmp x (move_up curs)
    | PathR(_,v,_) ->
	if cmp x v > 0 then curs 
	else move_to_ancestor cmp x (move_up curs)

  let rec move_to cmp x curs =
    let (p,t) as curs = move_to_ancestor cmp x curs in
      match t with
	| Empty -> raise Not_found
	| Leaf v -> if (cmp x v) = 0 then curs else raise Not_found
	| Node(l,v,r,_) -> match cmp x v with
	    | 0 -> curs
	    | c when c < 0 -> move_to cmp x (move_down_left curs)
	    | _ -> move_to cmp x (move_down_right curs)

  let rec to_string to_s t = 
    let rec h = function
    | Empty -> ""
    | Leaf x -> to_s x
    | Node(Empty,v,Empty,_) -> to_s v
    | Node(l,v,Empty,_) -> Printf.sprintf "%s, %s" (h l) (to_s v)
    | Node(Empty,v,r,_) -> Printf.sprintf "%s, %s" (to_s v) (h r)
    | Node(l,v,r,_) ->
	Printf.sprintf "%s, %s, %s"
	  (h l) (to_s v) (h r)
    in
      "{" ^ (h t) ^ "}"

  let gen_ cmp (agen: ?size:int -> Random.State.t -> 'a) ?(size=50) rs : 'a tree = 
    let num = Random.State.int rs size in
    let rec loop n t = 
      if n <= 0 then t
      else loop (n-1) (add cmp (agen ~size:size rs) t)
    in
      loop num empty

end

module AVL_PolySet (HeightDiff : sig val v : int end) = 
struct
  module BH = BaseSet(HeightDiff)
  include BH
(*  include Cursor.Mixin(BH)*)

  type 'a t = 'a tree
  type 'a set = 'a t
  type 'a elt_ = 'a

  type 'a cursor = 'a curs
  type 'a cursor_ = 'a cursor
  type ('a,'b) result = 'a
  type ('a,'b) result_ = 'a

  let add x t = add Pervasives.compare x t
  let mem x t = mem Pervasives.compare x t
  let remove x t = remove Pervasives.compare x t
  let split v t = split Pervasives.compare v t
  let union t1 t2 = union Pervasives.compare t1 t2
  let diff t1 t2 = diff Pervasives.compare t1 t2
  let inter t1 t2 = inter Pervasives.compare t1 t2
  let well_formed t = well_formed Pervasives.compare t
  let move_to_ancestor cmp x c = move_to_ancestor Pervasives.compare x c
  let compare x y = cmp Pervasives.compare x y
  let equal x y = compare x y = 0

  let gen1 agen ?size rs = gen_ Pervasives.compare agen ?size rs
  (*include Merge_mixin.Make(B)*)

end

module PolySet1 = AVL_PolySet(struct let v = 1 end)
module PolySet2 = AVL_PolySet(struct let v = 2 end)
module PolySet3 = AVL_PolySet(struct let v = 3 end)
module PolySet = PolySet2

module AVL_MonoSet (HeightDiff : sig val v : int end) (C : Mono.Comparable) = 
struct
  module BH = BaseSet(HeightDiff)
  include BH
(*  include Cursor.Mixin(BH)*)

  type elt = C.t
  type t = C.t tree
  type cursor = C.t curs

  type 'a elt_ = elt
  type 'a set = t
  type 'a cursor_ = cursor
  type 'a result = 'a
  type ('a,'b) result_ = 'a

  let add x t = add C.compare x t
  let mem x t = mem C.compare x t
  let remove x t = remove C.compare x t
  let split v t = split C.compare v t
  let union t1 t2 = union C.compare t1 t2
  let diff t1 t2 = diff C.compare t1 t2
  let inter t1 t2 = inter C.compare t1 t2
  let well_formed t = well_formed C.compare t
  let move_to_ancestor cmp x c = move_to_ancestor C.compare x c
  let compare x y = cmp C.compare x y
  let equal x y = compare x y = 0

  let to_string t = to_string C.to_string t
  (*include Merge_mixin.Make(B)*)

  let gen1 agen ?size rs = gen_ C.compare agen ?size rs
end

module MonoSet1 = AVL_MonoSet(struct let v = 1 end)
module MonoSet2 = AVL_MonoSet(struct let v = 2 end)
module MonoSet3 = AVL_MonoSet(struct let v = 3 end)
module MonoSet = MonoSet2

module AVL_GenSet (HeightDiff : sig val v : int end) 
  (C : Types.Mono.ArbitraryComparable) = 
struct
  include AVL_MonoSet(HeightDiff)(C)

  let gen ?size rs = gen1 C.gen ?size rs
end

module GenSet1 = AVL_GenSet(struct let v = 1 end)
module GenSet2 = AVL_GenSet(struct let v = 2 end)
module GenSet3 = AVL_GenSet(struct let v = 3 end)

module GenSet = GenSet2
