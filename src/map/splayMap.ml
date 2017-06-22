(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Types

module BaseMap = struct

  type ('k,'v) tree = 
    | Empty
    | Node of ('k,'v) tree * 'k * 'v * ('k,'v) tree
	
  let of_result (x,_) = x

  type ('k,'v) path = 
    | Top 
    | PathL of ('k,'v) path * ('k,'v) tree
    | PathR of ('k,'v) path * ('k,'v) tree

  type ('k,'v) curs = ('k,'v) path * ('k,'v) tree

  let empty = Empty
  let is_empty = function Empty -> true | _ -> false

  let singleton k v = Node(Empty,k,v,Empty)

  let node l k v r = Node(l,k,v,r)

  let to_cursor t = (Top,t)

  let rec from_cursor (p,t) = match p with
    | Top -> t
    | PathL(p',Node(_,k,v,r)) -> from_cursor (p', Node(t,k,v,r))
    | PathR(p',Node(l,k,v,_)) -> from_cursor (p', Node(l,k,v,t))
    | _ -> assert false

  let at_top (p,t) = (p = Top)
  let has_left (p,t) = match t with
    | Node(Empty,_,_,_) -> false
    | Node _ -> true
    | _ -> false

  let has_right (p,t) = match t with
    | Node(_,_,_,Empty) -> false
    | Node _ -> true
    | _ -> false

  let went_left = function PathL _,_ -> true | _ -> false
  let went_right = function PathR _,_ -> true | _ -> false

  let move_up (p,t) = match p with
    | Top -> failwith "move_up"
    | PathL(p',Node(_,k,v,r)) -> p', Node(t,k,v,r)
    | PathR(p',Node(l,k,v,_)) -> p', Node(l,k,v,t)
    | _ -> assert false (* parent can't be emptytree *)

  let move_down_left (p,t) = match t with
    | Empty -> failwith "move_down_left"
    | Node(l,k,v,r) -> PathL(p,t),l

  let move_down_right (p,t) = match t with
    | Empty -> failwith "move_down_right"
    | Node(l,k,v,r) -> PathR(p,t),r

  let rec move_to_ancestor cmp x ((p,t) as curs) = match p with
    | Top -> curs
    | PathL(p', Node(_,k,v,_)) -> 
	if cmp x k < 0 then curs 
	else move_to_ancestor cmp x (move_up curs)
    | PathR(p', Node(_,k,_,_)) ->
	if cmp x k > 0 then curs 
	else move_to_ancestor cmp x (move_up curs)
    | _ -> assert false

  let rec splay curs = match curs with
    | Top,_ -> curs
    | _, Empty -> splay (move_up curs)

    (* no grand-parent, so just zig one level *)
    | PathL(Top,Node(_,k,v,r)), Node(ll,lk,lv,lr) -> 
	Top,Node(ll,lk,lv,Node(lr,k,v,r))
	
    | PathR(Top,Node(l,k,v,_)),Node(rl,rk,rv,rr) -> 
	Top,Node(Node(l,k,v,rl),rk,rv,rr)

    (* has grand-parent *)
    (* zig-zig *)
    | PathL(PathL(gp_p,Node(_,k,v,r)),Node(_,lk,lv,lr)), Node(lll,llk,llv,llr) ->
	let br = Node(lr,k,v,r) in
	let mr = Node(llr,lk,lv,br) in
	  splay (gp_p, Node(lll,llk,llv,mr))

    (* zig-zig *)
    | PathR(PathR(gp_p,Node(l,k,v,_)),Node(ll,lk,lv,_)), Node(rrl,rrk,rrv,rrr) ->
	let bl = Node(l,k,v,ll) in
	let ml = Node(bl,lk,lv,rrl) in
	  splay (gp_p,Node(ml,rrk,rrv,rrr))

    (* zig-zag *)
    | PathL(PathR(gp_p,Node(l,k,v,_)),Node(_,rk,rv,rr)), Node(rll,rlk,rlv,rlr) ->
	let newl = Node(l,k,v,rll) in
	let newr = Node(rlr,rk,rv,rr) in
	  splay (gp_p,Node(newl, rlk, rlv, newr))
	  
    (* zig-zag *)
    | PathR(PathL(gp_p,Node(_,k,v,r)),Node(ll,lk,lv,_)), Node(lrl,lrk,lrv,lrr) ->
	let newl = Node(ll,lk,lv,lrl) in
	let newr = Node(lrr,k,v,r) in
	  splay(gp_p, Node(newl, lrk, lrv, newr))

    (* all of remaining cases are impossible. e.g., the grandparent
       tree being Empty *)
    | _ -> assert false

  let rec add_at cmp k v (p,t) = match t with
    | Empty -> p,Node(Empty,k,v,Empty)
    | Node(l,k',v',r) -> match cmp k k' with
	| 0 -> p, Node(l,k,v,r) (* replace binding *)
	| c when c < 0 -> add_at cmp k v (PathL(p,t),l)
	| _ -> add_at cmp k v (PathR(p,t),r)

  let add cmp k v t = 
    let curs = add_at cmp k v (to_cursor t) in
      from_cursor (splay curs)

  let rec closest_to cmp x ((p,t) as curs) = match t with
    | Empty -> if at_top curs then curs else move_up curs
    | Node(l,k,v,r) -> match cmp x k with
	| 0 -> curs
	| c when c < 0 -> closest_to cmp x (PathL(p,t),l)
	| _ -> closest_to cmp x (PathR(p,t),r)

  let top_node = function
    | Empty -> raise (Invalid_argument "splay:top_node")
    | Node(_,k,v,_) -> k,v

  let rec goto_min ((p,t) as curs) = match t with 
    | Empty -> curs
    | Node(Empty,_,_,_) -> curs
    | Node(l,_,_,_) -> goto_min ((PathL(p,t)),l)

  let rec goto_max ((p,t) as curs) = match t with 
    | Empty -> curs
    | Node(_,_,_,Empty) -> curs
    | Node(_,_,_,r) -> goto_max ((PathR(p,t)),r)

  let min_keyval t = 
    if is_empty t then raise Not_found
    else
      let c = goto_min (to_cursor t) in
      let t = from_cursor (splay c) in
	top_node t, t

  let max_keyval t = 
    if is_empty t then raise Not_found
    else
      let c = goto_max (to_cursor t) in
      let t = from_cursor (splay c) in
	top_node t, t
      
  let min_key t = let (k,_),t = min_keyval t in k,t
  let max_key t = let (k,_),t = max_keyval t in k,t

  let mem cmp x t = 
    let curs = closest_to cmp x (to_cursor t) in
    let t = from_cursor (splay curs) in
      match t with
	| Empty -> false,t
	| Node(_,k,_,_) -> if cmp x k = 0
	  then true,t
	  else false,t

  let find cmp x t = 
    let ((p,t') as curs) = closest_to cmp x (to_cursor t) in
      match t' with
	| Empty -> raise Not_found
	| Node(l,k,v,r) ->
	    if cmp x k = 0
	    then v, (from_cursor (splay curs))
	    else raise Not_found

  (* TODO: fix this to be better than O(n) stack *)
  let rec iter f = function
    | Empty -> ()
    | Node(l,k,v,r) -> iter f l; f k v; iter f r

  let rec mapi f = function
    | Empty -> Empty
    | Node(l,k,v,r) ->Node(mapi f l, k, f k v, mapi f r)

  let map f t = mapi (fun _ v -> f v) t

  let rec get_and_remove_min = function
    | Empty -> raise (Invalid_argument "remove_min")
    | Node(Empty,k,v,r) -> k,v,r
    | Node(l,k,v,r) -> 
	let k',v',newl = get_and_remove_min l in
	  k',v', Node(newl,k,v,r)

  let remove cmp x t = 
    let (p,t) = closest_to cmp x (to_cursor t) in
    let t = match t with
	| Empty -> t
	| Node(Empty,k,v,r) -> if (cmp x k) = 0 then r else t
	| Node(l,k,v,Empty) -> if (cmp x k) = 0 then l else t
	| Node(l,k,v,r) -> 
	    if (cmp x k) = 0 then
	      let k',v',newl = get_and_remove_min l in
		Node(newl,k',v',r)
	    else t
    in
      from_cursor (splay (p,t))

  let rec compare_ kcmp vcmp t1 t2 = match t1,t2 with
    | Empty, Empty -> 0
    | Empty, Node _ -> -1
    | Node _, Empty -> 1
    | _ ->
	(* This actually may be one of the most efficient ways to
	   implement this since we will always be removing near the
	   top thanks to the splay property. *)
	let xk,xv,t1' = get_and_remove_min t1 in
	let yk,yv,t2' = get_and_remove_min t2 in
	  match kcmp xk yk with
	    | 0 -> begin match vcmp xv yv with
		| 0 -> compare_ kcmp vcmp t1' t2'
		| v -> v
	      end
	    | v -> v

  let compare_keys kcmp t1 t2 = compare_ kcmp (fun _ _ -> 0) t1 t2

  let rec split cmp kelt t = match t with
    | Empty -> Empty, Empty
    | Node(l1,k,v,r1) ->
	match cmp kelt k with
	  | 0 -> l1,r1
	  | c when c < 0 ->
	      let l2,r2 = split cmp kelt l1 in
		l2,Node(r2,k,v,r1)
	  | _ ->
	      let l2,r2 = split cmp kelt r1 in
		Node(l1,k,v,l2), r2

  let rec union cmp f t1 t2 = match t1,t2 with
    | Empty, t | t, Empty -> t
    | t1, Node(l,k,v,r) ->
	let l',r' = split cmp k t1 in
	let v' = 
	  try 
	    let v',_ = find cmp k t1 in
	      f k v v'
	  with Not_found -> v
	in
	  Node((union cmp f l' l),k,v',(union cmp f r' r))

  let rec concat t1 t2 = match t1,t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | Node(l1,k1,v1,r1), Node(l2,k2,v2,r2) ->
	let k',v',t2' = get_and_remove_min t2 in
	  Node(t1,k',v',t2')

  let rec diff cmp f t1 t2 = match t1,t2 with
    | Empty, _ -> Empty 
    | _, Empty -> t1
    | _, Node(l,k,v,r) ->
	let l',r' = split cmp k t1 in
	  concat (diff cmp f l' l) (diff cmp f r' r)

  let rec inter cmp f t1 t2 = match t1,t2 with
    | Empty,_ | _,Empty -> Empty
    | t1, Node(l,k,v,r) ->
	let l',r' = split cmp k t1 in
	  try
	    let v',_ = find cmp k t1 in
	    let uv = f k v v' in
	      Node((inter cmp f l' l),k,uv,(inter cmp f r' r))
	  with Not_found ->
	    concat (inter cmp f l' l) (inter cmp f r' r)

  let at_right = function
    | _,Empty -> true
    | _,Node _ -> false

  let at_left = at_right

  let has_value = function _,Node _ -> true | _ -> false
  let get_value = function
    | _,Empty -> failwith "get_value"
    | _,Node(_,k,v,_) -> k,v
      
  let rec cardinal = function
    | Empty -> 0
    | Node(l,_,_,r) -> 1 + (cardinal l) + (cardinal r)

  (* TODO: fix this to be better than O(n) stack *)
  let rec fold f acc t = match t with
    | Empty -> acc
    | Node(l,k,v,r) ->
	fold f (f (fold f acc l) k v) r
	
  let rec well_ordered cmp = function
    | Empty -> true
    | Node(Empty,_,_,Empty) -> true
    | Node(Node(_,lk,lv,_) as l,k,v,Empty) -> 
	((cmp lk k) < 0) && well_ordered cmp l
    | Node(Empty,k,v,(Node(_,rk,rv,_) as r)) -> 
	((cmp rk k) > 0) && well_ordered cmp r
    | Node(Node(_,lk,_,_) as l,k,_,(Node(_,rk,_,_) as r)) -> 
	((cmp lk k) < 0) &&((cmp rk k) > 0) && 
	  well_ordered cmp l && well_ordered cmp r

  let well_formed t = well_ordered t

  let rec to_string to_s t = 
    let rec h = function
      | Empty -> ""
      | Node(Empty,k,v,Empty) -> to_s k v
      | Node(l,k,v,Empty) -> Printf.sprintf "%s, %s" (h l) (to_s k v)
      | Node(Empty,k,v,r) -> Printf.sprintf "%s, %s" (to_s k v) (h r)
      | Node(l,k,v,r) ->
	  Printf.sprintf "%s, %s, %s"
	    (h l) (to_s k v) (h r)
    in "{" ^ (h t) ^ "}"

  let gen2 cmp
      (kgen : ?size:int -> Random.State.t -> 'k) 
      (vgen : ?size:int -> Random.State.t -> 'v) 
      ?(size=50) rs : ('k,'v) tree = 
    let num = Random.State.int rs size in
    let rec loop n t = 
      if n <= 0 then t
      else
	let k = kgen ~size:size rs in
	let v = vgen ~size:size rs in
	let t = from_cursor (add_at cmp k v (to_cursor t)) in
	  loop (n-1) t
    in
      loop num empty

end


(* CR SW: Is it possible to write a functor that builds a Poly from a BaseMap? *)
module PolyMap = struct
  include BaseMap

  type 'a key = 'a
  type 'a key_ = 'a
      
  type 'e elt = 'e
  type 'e elt_ = 'e
  type ('k,'v) t = ('k,'v) tree
  type ('k,'v) map = ('k,'v) t

  type ('k,'v) cursor = ('k,'v) curs
  type ('k,'v) cursor_ = ('k,'v) cursor

  type ('a,'k,'v) result = 'a * ('k,'v) tree
  type ('a,'k,'v) result_ = ('a,'k,'v) result

  let compare = compare_
  let mem k t = mem Pervasives.compare k t
  let add k v t = add Pervasives.compare k v t
  let remove k t = remove Pervasives.compare k t
  let find k t = find Pervasives.compare k t

  let union f t1 t2 = union Pervasives.compare f t1 t2
  let inter f t1 t2 = inter Pervasives.compare f t1 t2
  let diff f t1 t2 = diff Pervasives.compare f t1 t2

  let well_formed t = well_formed Pervasives.compare t

  let gen2
      (kgen : ?size:int -> Random.State.t -> 'k) 
      (vgen : ?size:int -> Random.State.t -> 'v) 
      ?size rs : ('k,'v) tree = 
    gen2 Pervasives.compare kgen vgen ?size rs

end

module MonoKeyMap(C : Mono.Comparable) = struct
  include BaseMap

  type key = C.t
  type 'a key_ = key
      
  type 'e elt = 'e
  type 'e elt_ = 'e

  type 'v t = (C.t,'v) tree
  type ('k,'v) map = 'v t

  type 'v cursor = (C.t,'v) curs
  type ('k,'v) cursor_ = 'v cursor

  type ('a,'v) result = 'a * 'v t
  type ('a,'k,'v) result_ = ('a,'v) result

  let compare t1 t2 = compare_ C.compare t1 t2
  let compare_keys t1 t2 = compare_keys C.compare t1 t2
  let mem k t = mem C.compare k t
  let add k v t = add C.compare k v t
  let remove k t = remove C.compare k t
  let find k t = find C.compare k t

  let union f t1 t2 = union C.compare f t1 t2
  let inter f t1 t2 = inter C.compare f t1 t2
  let diff f t1 t2 = diff C.compare f t1 t2

  let well_formed t = well_formed C.compare t

  let to_string to_s t = 
    let f k v = Printf.sprintf "(%s => %s)" (C.to_string k) (to_s v) in
      to_string f t

  let gen2
      (kgen : ?size:int -> Random.State.t -> 'k) 
      (vgen : ?size:int -> Random.State.t -> 'v) 
      ?size rs : ('k,'v) tree = 
    gen2 C.compare kgen vgen ?size rs

end

module GenKeyMap (C : Mono.ArbitraryComparable) = struct
  include MonoKeyMap(C)

  let gen1 (vgen : ?size:int -> Random.State.t -> 'v) ?size rs : 'v t = 
    gen2 C.gen vgen ?size rs
end

module MonoMap (K : Mono.Comparable) (V : Mono.Comparable) = struct
  include BaseMap

  type key = K.t
  type 'a key_ = key
      
  type elt = V.t
  type 'e elt_ = elt

  type t = (key,elt) tree
  type ('k,'v) map = t

  type cursor = (key,elt) curs
  type ('k,'v) cursor_ = cursor

  type 'a result = 'a * t
  type ('a,'k,'v) result_ = 'a result

  let compare t1 t2 = compare_ K.compare V.compare t1 t2
  let compare_keys t1 t2 = compare_keys K.compare t1 t2
  let mem k t = mem K.compare k t
  let add k v t = add K.compare k v t
  let remove k t = remove K.compare k t
  let find k t = find K.compare k t

  let union f t1 t2 = union K.compare f t1 t2
  let inter f t1 t2 = inter K.compare f t1 t2
  let diff f t1 t2 = diff K.compare f t1 t2

  let well_formed t = well_formed K.compare t

  let to_string t = 
    let f k v = Printf.sprintf "(%s => %s)" (K.to_string k) (V.to_string v) in
      to_string f t

  let gen2
      (kgen : ?size:int -> Random.State.t -> key) 
      (vgen : ?size:int -> Random.State.t -> elt) 
      ?size rs : t = 
    gen2 K.compare kgen vgen ?size rs
end

module GenMap 
  (K : Types.Mono.ArbitraryComparable) 
  (V : Types.Mono.ArbitraryComparable) = 
struct
  include MonoMap(K)(V)

  let gen ?size rs = gen2 K.gen V.gen ?size rs
end

  
