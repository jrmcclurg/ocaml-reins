(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** The main functor for implementing maps.  The paramater field
    HeightDiff.v specifies the maximum difference between the heights
    of two subtrees joined at a node.
*)
module BaseMap (HeightDiff : sig val v : int end) = struct

(** The types of AVL trees.  An element can be either stored in a Leaf
    if it has no children, or in a Node if it has at least 1 child.
    The constructor Node(l,v,r,h) also contains the left branch 'l' (all
    elements are smaller than v), the right branch 'r' (all elements
    greater than v) and the heigh of the tree at that point. 
*)
  type ('k,'v) tree = 
      | Empty
      | Leaf of 'k * 'v
      | Node of ('k,'v) tree * 'k * 'v * ('k,'v) tree * int

  let of_result x = x
    
  let empty = Empty      

  let singleton k v = Leaf(k,v)
    
  let is_empty = function
    | Empty -> true
    | _ -> false

  let rec find cmp x = function
    | Empty -> raise Not_found
    | Leaf(k,v) -> if (cmp x k) = 0 then v else raise Not_found
    | Node(l,k,v,r,_) -> match cmp x k with
	| 0 -> v
	| c when c < 0 -> find cmp x l
	| _ -> find cmp x r
	    
  let mem cmp x t = try ignore(find cmp x t);true with Not_found -> false

  let rec fold f acc t = match t with
    | Empty -> acc
    | Leaf(k,v) -> f acc k v
    | Node(l,k,v,r,_) ->
	fold f (f (fold f acc l) k v) r

  let rec iter f t = match t with
    | Empty -> ()
    | Leaf(k,v) -> f k v
    | Node(l,k,v,r,_) ->
	iter f l; f k v; iter f r

  let rec min_key = function
    | Empty -> raise Not_found
    | Leaf(k,_) -> k
    | Node(Empty,k,_,_,_) -> k
    | Node(l,_,_,_,_) -> min_key l

  let rec max_key = function
    | Empty -> raise Not_found
    | Leaf(k,_) -> k
    | Node(_,k,_,Empty,_) -> k
    | Node(_,_,_,r,_) -> max_key r

  let rec min_keyval = function
    | Empty -> raise Not_found
    | Leaf(k,v) -> k,v
    | Node(Empty,k,v,_,_) -> k,v
    | Node(l,_,_,_,_) -> min_keyval l

  let rec max_keyval = function
    | Empty -> raise Not_found
    | Leaf(k,v) -> k,v
    | Node(_,k,v,Empty,_) -> k,v
    | Node(_,_,_,r,_) -> max_keyval r

  let height = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node(_,_,_,_,h) -> h

  (** N-"smart" constructor (a la Stephen Adams).   This function
      chooses the right constructor based on the number of children
      and ensures that the Node constructor is well formed.
  *)
  let node l (k,v) r = 
    match height l, height r with
      | 0,0 -> Leaf(k,v)
      | hl,hr -> Node(l,k,v,r, (max hl hr)+1)
	  
  let pivot ll lkv c rkv rr = match c with
    | Node(cl,ck,cv,cr,_) ->
	node (node ll lkv cl) (ck,cv) (node cr rkv rr)
    | Leaf(ck,cv) ->
	node (node ll lkv Empty) (ck,cv) (node Empty rkv rr)
    | Empty -> assert false

  (** This function will fix the tree if the left subtree has a height
      at most HeightDiff.v +1 more than that of the right subtree. *)
  let rebal_left ll lkv lr kv r =
    if height ll >= height lr
    then node ll lkv (node lr kv r)
    else pivot ll lkv lr kv r

  (** This function will fix the tree if the right subtree has a
      height at most HeightDiff.v +1 more than that of the left
      subtree. *)
  let rebal_right l kv rl rkv rr = 
    if height rr >= height rl 
    then node (node l kv rl) rkv rr
    else pivot l kv rl rkv rr

  (** T'-"smart" constructor: fixes trees by performing at most 1
      rotation. *)
  let rotate l ((k,v) as kv) r = 
    match l,r with
        (* Height 1 tree *)
      | Empty, Empty -> Leaf(k,v)
          
      (* Height 2 tree *)
      | Empty, Leaf _
      | Leaf _, Empty
      | Leaf _, Leaf _ -> Node(l,k,v,r,2)
          
      (* General Height 'h' *)
      | Node(ll,lk,lv,lr,h), Empty -> 
          if h > HeightDiff.v 
	  then rebal_left ll (lk,lv) lr kv r
          else Node(l,k,v,r,h+1)
      | Empty, Node(rl,rk,rv,rr,h) ->
          if h > HeightDiff.v 
	  then rebal_right l kv rl (rk,rv) rr
          else Node(l,k,v,r,h+1)

      | Leaf _, Node(_,_,_,_,h)         (* 1 + for Leaf _ *)
      | Node(_,_,_,_,h), Leaf _ when h <= (1 + HeightDiff.v) ->
          Node(l,k,v,r,h+1)

      | Leaf _, Node(rl,rk,rv,rr,h) -> rebal_right l kv rl (rk,rv) rr
      | Node(ll,lk,lv,lr,h), Leaf _  -> rebal_left ll (lk,lv) lr kv r

      | Node(ll,lk,lv,lr,lh), Node(rl,rk,rv,rr,rh) -> 
	  if lh > rh + HeightDiff.v
	  then rebal_left ll (lk,lv) lr kv r
	  else if rh > lh + HeightDiff.v 
	  then rebal_right l kv rl (rk,rv) rr
	  else node l kv r

  let rec add cmp k v t = match t with
    | Empty -> Leaf(k,v)
    | Leaf(k',v') ->
        begin match cmp k k' with
          | 0 -> Leaf(k,v) (* replace existing binding *)
          | c when c < 0 -> Node(Empty,k,v,t,2)
          | _ -> Node(t, k,v, Empty,2)
        end
    | Node(l,k',v',r,h) ->
        match cmp k k' with 
          | 0 -> Node(l,k,v,r,h) (* repalce existing binding *)
          | c when c < 0 -> rotate (add cmp k v l) (k',v') r
          | _ -> rotate l (k',v') (add cmp k v r)

  let rec get_and_remove_min = function
    | Empty -> raise (Invalid_argument "get_and_remove_min")
    | Leaf(k,v) -> (k, v), Empty
    | Node(Empty,k,v,r,h) -> (k, v), r
    | Node(l,k,v,r,h) -> 
	let kv,newl = get_and_remove_min l in
	  kv, rotate newl (k,v) r

  let rec remove cmp delk t = match t with
    | Empty -> Empty
    | Leaf(k,v)
    | Node(Empty,k,v,Empty,_) -> 
	if (cmp delk k) = 0 then Empty else t

    | Node(l,k,v,r,_) -> match cmp delk k with
	| 0 -> 
	    if r = Empty then l
	    else if l = Empty then r else
	      let kv,newr = get_and_remove_min r in
		rotate l kv newr

 	| c when c < 0 -> rotate (remove cmp delk l) (k,v) r
	| _ -> rotate l (k,v) (remove cmp delk r)

  (** join trees of arbitrary size *)
  let rec concat3 cmp l ((k,v) as kv) r  = match l,r with
    | Empty, r -> add cmp k v r
    | l, Empty -> add cmp k v l
    | Leaf _, Leaf _ -> node l kv r
    | Leaf(lk,lv), Node(rl,rk,rv,rr,h) -> 
	if h > (1 + HeightDiff.v)
	then rotate (concat3 cmp l kv rl) (rk,rv) rr
	else node l kv r
    | Node(ll,lk,lv,lr,h), Leaf(rk,rv) -> 
	if h > (1 + HeightDiff.v)
	then rotate ll (lk,lv) (concat3 cmp lr kv r)
	else node l kv r
    | Node(ll,lk,lv,lr,lh),Node(rl,rk,rv,rr,rh) ->
	if rh > lh + HeightDiff.v 
	then rotate (concat3 cmp l kv rl) (rk,rv) rr
	else if lh > rh + HeightDiff.v
	then rotate ll (lk,lv) (concat3 cmp lr kv r)
	else node l kv r

  (* equivalent to (split_lt v t), (split_gt v t) *)
  let rec split cmp k t = match t with
    | Empty -> Empty, Empty
    | Leaf(k',v') -> begin match cmp k k' with
	  | 0 -> Empty,Empty 
	  | c when c < 0 -> Empty,t
	  | _ -> t,Empty
      end
    | Node(l1,k',v',r1,_) ->
	match cmp k k' with
	  | 0 -> l1,r1
	  | c when c < 0 ->
	      let l2,r2 = split cmp k l1 in
		(l2,concat3 cmp r2 (k',v') r1)
	  | _ ->
	      let l2,r2 = split cmp k r1 in
		(concat3 cmp l1 (k',v') l2), r2

  let rec concat t1 t2 = match t1,t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | Leaf(lk,lv), Leaf(rk,rv) -> Node(t1,rk,rv,Empty,2)
    | Leaf(lk,lv), Node(rl,rk,rv,rr,h) ->
	if h > 1+HeightDiff.v 
	then rotate (concat t1 rl) (rk,rv) rr
	else 
	  let kv,t2' = get_and_remove_min t2 in
	    rotate t1 kv t2'
    | Node(ll,lk,lv,lr,h), Leaf(rk,rv) ->
	if h > 1+HeightDiff.v 
	then rotate ll (lk,lv) (concat lr t2)
	else rotate t1 (rk,rv) Empty (* inline get_*_min for Leaf *)
    | Node(l1,k1,v1,r1,h1), Node(l2,k2,v2,r2,h2) ->
	if h2 > h1 + HeightDiff.v
	then rotate (concat t1 l2) (k2,v2) r2
	else if h1 > h2 + HeightDiff.v 
	then rotate l1 (k1,v1) (concat r1 t2)
	else 
	  let kv,t2' = get_and_remove_min t2 in
	    rotate t1 kv t2'

  let add_join cmp f k v t = 
    try
      let v' = find cmp k t in
	(* don't call join if the values are physically equal *)
	if v' == v 
	then add cmp k v t
	else add cmp k (f k v v') t
    with Not_found -> add cmp k v t

  let rec union cmp f t1 t2 = match t1,t2 with
    | Empty, t | t, Empty -> t
    | Leaf(k,v),r -> add_join cmp f k v r
    | l,Leaf(k,v) ->  add_join cmp f k v l
    | t1, Node(l,k,v,r,_) ->
	let l',r' = split cmp k t1 in
	  (** This is slightly inefficient since we could use concat3
	      if k \in t1, but probably not worth the refactoring *)
	let t' = concat (union cmp f l' l) (union cmp f r' r) in
	  try let v' = find cmp k t1 in
	    add cmp k (f k v v') t'
	  with Not_found -> add cmp k v t'

  let rec diff cmp f t1 t2 = match t1,t2 with
    | Empty, _ -> Empty 
    | _, Empty -> t1
    | _, Leaf(k,v2) -> 
	begin try 
	  let v1 = find cmp k t1 in
	    if f k v1 v2 (* does the client consider these equal values? *)
	    then remove cmp k t1 (* yes, so remove the binding *)
	    else t1 (* no, so keep the binding *)
	with Not_found -> t1
	end
    | _, Node(l,k,v2,r,_) ->
	let l',r' = split cmp k t1 in
	  try 
	    let v1 = find cmp k t1 in
	      if f k v1 v2 (* does v1 = v2? *)
	      then concat (diff cmp f l' l) (diff cmp f r' r)
		(* note k must already be in t1 since find succeeded *)
	      else concat3 cmp (diff cmp f l' l) (k,v1) (diff cmp f r' r)
	  with Not_found -> 
	    (* k's not in t1, so the split will contain all of t1 *)
	    concat (diff cmp f l' l) (diff cmp f r' r)

  let rec inter cmp f t1 t2 = match t1,t2 with
    | Empty,_ | _,Empty -> Empty
    | t1, Leaf(k,v) -> 
	begin try
	  let v' = find cmp k t1 in
	    if v == v' 
	    then t2 (* already exists with the same physical value *)
	    else Leaf(k, (f k v v')) (* use value from t1 *)
	with Not_found -> Empty
	end

    | t1, Node(l,k,v,r,_) ->
	let l',r' = split cmp k t1 in
	  begin try
	    let v1 = find cmp k t1 in
	    let v2 = f k v v1 in
	      concat3 cmp (inter cmp f l' l) (k,v2) (inter cmp f r' r)
	  with Not_found ->
	    concat (inter cmp f l' l) (inter cmp f r' r)
	  end

  let rec mapi f = function
    | Empty -> Empty
    | Leaf(k,v) -> let v' = f k v in Leaf(k,v')
    | Node(l,k,v,r,h) ->
	let l' = mapi f l in
	let v' = f k v in
	let r' = mapi f r in
	  Node(l',k,v',r',h)
	  
  let map f t = mapi (fun _ v -> f v) t

(*
  let choose = function
    | Empty -> raise Not_found
    | Leaf(k,v) -> x
    | Node(_,x,_,_) -> x
*)

  let rec cardinal = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node(l,_,_,r,_) -> 1 + (cardinal l) + (cardinal r)

  let rec compare kcmp ecmp x y = 
    match (is_empty x), (is_empty y) with
      | true, true -> 0
      | true, false -> -1
      | false, true -> 1
      | false, false ->
	  let xk,xe = min_keyval x in
	  let yk,ye = min_keyval y in
	    match kcmp xk yk with
	      | 0 -> begin match ecmp xe ye with
		  | 0 -> compare kcmp ecmp (remove kcmp xk x) (remove kcmp yk y)
		  | v -> v
		end
	      | v -> v

  let compare_keys kcmp s t = compare kcmp (fun _ _ -> 0) s t

  let rec well_ordered cmp = function
    | Empty -> true
    | Leaf _ -> true
    | Node(Empty,_,_,Empty,_) -> assert false
    | Node(((Leaf(lk,_))|Node(_,lk,_,_,_) as l),k,_,Empty,_) -> 
	(well_ordered cmp l) && (cmp lk k < 0)
    | Node(Empty,k,_,((Leaf(rk,_))|Node(_,rk,_,_,_) as r),_) -> 
	(well_ordered cmp r) && (cmp rk k > 0)

    | Node(((Leaf(lk,_))|Node(_,lk,_,_,_) as l)
	      ,k,v,
	  ((Leaf(rk,_))|Node(_,rk,_,_,_) as r),
	  _) -> 
	(well_ordered cmp l) && (well_ordered cmp r) && 
	  (cmp lk k < 0) && (cmp rk k > 0)	

  let well_formed_height = function
    | Empty | Leaf _ -> true
    | Node(l,k,v,r,h) ->
	let hl = height l in
	let hr = height r in
	  (h = (max hl hr) + 1) &&
	    (abs (hl - hr) <= HeightDiff.v)

  let rec well_formed cmp t = 
    (well_ordered cmp t) && (well_formed_height t)

  type ('k,'v) path = 
    | Top
    | PathL of ('k,'v) path * 'k * 'v * ('k,'v) tree
    | PathR of ('k,'v) tree * 'k * 'v * ('k,'v) path

  type ('k,'v) curs = ('k,'v) path * ('k,'v) tree

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
    | PathL(p,k,v,r),l | PathR(l,k,v,p),r -> p, (node l (k,v) r)

  let move_down_left = function
    | _,Empty
    | _, Leaf _ -> failwith "move_down_left"
    | p, Node(l,k,v,r,h) -> PathL(p,k,v,r),l

  let move_down_right = function
    | _,Empty
    | _, Leaf _ -> failwith "move_down_right"
    | p,Node(l,k,v,r,h) -> PathR(l,k,v,p),r

  let rec from_cursor ((p,t) as curs) = 
    if at_top curs then t
    else from_cursor (move_up curs)

  let has_value (p,t) = match t with Empty -> false | _ -> true

  let get_value = function
    | _,Empty -> failwith "get_value"
    | _,Leaf(k,v)
    | _,Node(_,k,v,_,_) -> k,v

  let rec move_to_ancestor cmp x ((p,t) as curs) = match p with
    | Top -> curs
    | PathL(p', k, v, r) -> 
	if cmp x k < 0 then curs 
	else move_to_ancestor cmp k (move_up curs)
    | PathR(_,k,v,_) ->
	if cmp x k > 0 then curs 
	else move_to_ancestor cmp k (move_up curs)

  let rec move_to cmp x curs =
    let (p,t) as curs = move_to_ancestor cmp x curs in
      match t with
	| Empty -> raise Not_found
	| Leaf(k,v) -> if (cmp x k) = 0 then curs else raise Not_found
	| Node(l,k,v,r,_) -> match cmp x k with
	    | 0 -> curs
	    | c when c < 0 -> move_to cmp x (move_down_left curs)
	    | _ -> move_to cmp x (move_down_right curs)

  let rec to_string to_s t = 
    let rec h = function
    | Empty -> ""
    | Leaf(k,v) -> to_s k v
    | Node(Empty,k,v,Empty,_) -> to_s k v
    | Node(l,k,v,Empty,_) -> Printf.sprintf "%s, %s" (h l) (to_s k v)
    | Node(Empty,k,v,r,_) -> Printf.sprintf "%s, %s" (to_s k v) (h r)
    | Node(l,k,v,r,_) ->
	Printf.sprintf "%s, %s, %s"
	  (h l) (to_s k v) (h r)
    in
      "{" ^ (h t) ^ "}"

  let gen_ cmp 
      (kgen: ?size:int -> Random.State.t -> 'k) 
      (egen: ?size:int -> Random.State.t -> 'v)
      ?(size=50) rs : ('k,'v) tree = 
    let num = Random.State.int rs size in
    let rec loop n t = 
      if n <= 0 then t
      else 
	let k = kgen ~size:size rs in
	let v = egen ~size:size rs in
	  loop (n-1) (add cmp k v t)
    in
      loop num empty

end


module AVL_KeyMap (HeightDiff : sig val v : int end) (C : Types.Mono.Comparable) 
= 
struct
  module BH = BaseMap(HeightDiff)
  include BH
(*  include Cursor.Mixin(BH)*)

  type key = C.t
  type 'a key_ = key

  type 'e elt = 'e
  type 'e elt_ = 'e

  type 'v t = (key,'v) tree
  type ('k,'v) map = 'v t

  type 'v cursor = (C.t,'v) curs
  type ('k,'v) cursor_ = 'v cursor

  type ('a,'v) result = 'a
  type ('a,'k,'v) result_ = 'a

  let add x t = add C.compare x t
  let mem x t = mem C.compare x t
  let remove x t = remove C.compare x t
  let find x t = find C.compare x t
  let split v t = split C.compare v t
  let union f t1 t2 = union C.compare f t1 t2
  let diff f t1 t2 = diff C.compare f t1 t2
  let inter f t1 t2 = inter C.compare f t1 t2
  let well_formed t = well_formed C.compare t
  let move_to_ancestor cmp x c = move_to_ancestor C.compare x c
  let compare x y = compare C.compare x y
  let compare_keys t1 t2 = compare_keys C.compare t1 t2
(*  let equal x y = compare x y = 0*)

  let to_string to_s t = 
    to_string (fun k v -> 
		 Printf.sprintf "(%s => %s)" (C.to_string k) (to_s v)
	      ) t
  (*include Merge_mixin.Make(B)*)

  (* need to eta expand these to properly generalize the type
     variables *)
  let gen2
      (kgen: ?size:int -> Random.State.t -> 'k) 
      (egen: ?size:int -> Random.State.t -> 'v)
      ?size rs : ('k,'v) tree = 
    gen_ C.compare kgen egen ?size rs
end

module MonoKey1 = AVL_KeyMap(struct let v = 1 end)
module MonoKey2 = AVL_KeyMap(struct let v = 2 end)
module MonoKey3 = AVL_KeyMap(struct let v = 3 end)
module MonoKeyMap = MonoKey2

module AVL_GenKeyMap (HeightDiff : sig val v : int end) 
  (C : Types.Mono.ArbitraryComparable) = 
struct
  include AVL_KeyMap(HeightDiff)(C)

  let gen1 (agen : (?size:int -> Random.State.t -> 'a)) ?size rs : 'a t = 
    gen2 C.gen agen ?size rs

end

module GenKey1 = AVL_GenKeyMap(struct let v = 1 end)
module GenKey2 = AVL_GenKeyMap(struct let v = 2 end)
module GenKey3 = AVL_GenKeyMap(struct let v = 3 end)
module GenKeyMap = GenKey2

module AVL_PMap (HeightDiff : sig val v : int end) = struct
  module BH = BaseMap(HeightDiff)
  include BH
(*  include Cursor.Mixin(BH)*)

  type 'a key = 'a
  type 'a key_ = 'a
      
  type 'e elt = 'e
  type 'e elt_ = 'e
  type ('k,'v) t = ('k,'v) tree
  type ('k,'v) map = ('k,'v) t

  type ('k,'v) cursor = ('k,'v) curs
  type ('k,'v) cursor_ = ('k,'v) cursor

  type ('a,'k,'v) result = 'a
  type ('a,'k,'v) result_ = 'a

  let add x t = add Pervasives.compare x t
  let mem x t = mem Pervasives.compare x t
  let remove x t = remove Pervasives.compare x t
  let find x t = find Pervasives.compare x t
  let split v t = split Pervasives.compare v t
  let union f t1 t2 = union Pervasives.compare f t1 t2
  let diff f t1 t2 = diff Pervasives.compare f t1 t2
  let inter f t1 t2 = inter Pervasives.compare f t1 t2
  let well_formed t = well_formed Pervasives.compare t
  let move_to_ancestor cmp x c = move_to_ancestor Pervasives.compare x c

    (*  let equal x y = compare x y = 0*)

  let gen2 
      (kgen: ?size:int -> Random.State.t -> 'k) 
      (egen: ?size:int -> Random.State.t -> 'v)
      ?size rs : ('k,'v) tree = 
    gen_ Pervasives.compare kgen egen ?size rs

  (*include Merge_mixin.Make(B)*)

end

module Poly1 = AVL_PMap(struct let v = 1 end)
module Poly2 = AVL_PMap(struct let v = 2 end)
module Poly3 = AVL_PMap(struct let v = 3 end)
module PolyMap = Poly2

module AVL_Map 
  (HeightDiff : sig val v : int end) 
  (K : Types.Mono.Comparable) 
  (E : Types.Mono.Comparable) 
= 
struct
  module BH = BaseMap(HeightDiff)
  include BH
(*  include Cursor.Mixin(BH)*)

  type key = K.t
  type 'a key_ = key

  type elt = E.t
  type 'e elt_ = elt

  type t = (key,elt) tree
  type ('k,'v) map = t

  type cursor = (K.t,E.t) curs
  type ('k,'v) cursor_ = cursor

  type 'a result = 'a
  type ('a,'k,'v) result_ = 'a

  let add x t = add K.compare x t
  let mem x t = mem K.compare x t
  let remove x t = remove K.compare x t
  let find x t = find K.compare x t
  let split v t = split K.compare v t
  let union f t1 t2 = union K.compare f t1 t2
  let diff f t1 t2 = diff K.compare f t1 t2
  let inter f t1 t2 = inter K.compare f t1 t2
  let well_formed t = well_formed K.compare t
  let move_to_ancestor cmp x c = move_to_ancestor K.compare x c

  let compare x y = compare K.compare E.compare x y
  let compare_keys t1 t2 = compare_keys K.compare t1 t2

  let to_string t = 
    to_string (fun k v -> 
		 Printf.sprintf "(%s => %s)" (K.to_string k) (E.to_string v)
	      ) t
  (*include Merge_mixin.Make(B)*)

  let gen2 
      (kgen: ?size:int -> Random.State.t -> 'k) 
      (egen: ?size:int -> Random.State.t -> 'v)
      ?size rs : ('k,'v) tree = 
    gen_ Pervasives.compare kgen egen ?size rs

end

module Mono1 = AVL_Map(struct let v = 1 end)
module Mono2 = AVL_Map(struct let v = 2 end)
module Mono3 = AVL_Map(struct let v = 3 end)
module MonoMap = Mono2

module AVL_GenMap (HeightDiff : sig val v : int end) 
  (K : Types.Mono.ArbitraryComparable) 
  (E : Types.Mono.ArbitraryComparable) = 
struct
  include AVL_Map(HeightDiff)(K)(E)

  let gen ?size rs = gen_ K.compare K.gen E.gen ?size rs
end

module Gen1 = AVL_GenMap(struct let v = 1 end)
module Gen2 = AVL_GenMap(struct let v = 2 end)
module Gen3 = AVL_GenMap(struct let v = 3 end)
module GenMap = Gen2

