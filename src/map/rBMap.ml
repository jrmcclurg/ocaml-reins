(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

module BaseMap = struct
    (* Red/Black Trees follow:
       1) all nodes are Red or Black
       2) The root is black 
       3) Empty Trees (i.e. leafs) are black
       4) Both children of a red node are black
       5) Every path from a leaf to the root has the same "black height"
    *)

  (* save a cell by encoding the color in the constructor *)
  type ('a,'b) tree = 
      | Empty
      | RNode of ('a,'b) tree * 'a * 'b * ('a,'b) tree
      | BNode of ('a,'b) tree * 'a * 'b * ('a,'b) tree

  let of_result x = x

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let singleton k v = BNode(Empty,k,v,Empty)

  let is_black = function
    | Empty -> true
    | BNode _ -> true
    | RNode _ -> false

  let rec black_height t = 
    let rec bh acc = function
      | Empty -> 1+acc
      | RNode(l,_,_,r) -> bh acc l
      | BNode(l,_,_,r) -> bh (acc+1) l
    in bh 0 t

  (* true if the top of sub is lt x *)
  let sub_lt cmp x sub = match sub with 
    | RNode(_,k,_,_)
    | BNode(_,k,_,_) -> cmp k x < 0
    | _ -> assert false

  let sub_gt cmp x sub = match sub with
    | RNode(_,k,_,_)
    | BNode(_,k,_,_) -> cmp k x > 0
    | _ -> assert false

  let rec well_ordered cmp = function
    | Empty -> true
    | RNode(Empty,_,_,Empty) | BNode(Empty,_,_,Empty) -> true

    | BNode(Empty,k,_,r)
    | RNode(Empty,k,_,r) -> sub_gt cmp k r && well_ordered cmp r
    | BNode(l,k,_,Empty)
    | RNode(l,k,_,Empty) -> sub_lt cmp k l && well_ordered cmp l

    | RNode(l,e,_,r) | BNode(l,e,_,r) ->
	sub_lt cmp e l && sub_gt cmp e r && 
	  well_ordered cmp l && well_ordered cmp r

  let rec check_red_children = function
    | Empty -> true
    | BNode(l,_,_,r) -> check_red_children l && check_red_children r
    | RNode(l,_,_,r) -> is_black l && is_black r && 
	check_red_children l && check_red_children r

  let rec check_black_height = function
    | Empty -> true
    | RNode(l,_,_,r) | BNode(l,_,_,r) ->
	if ((black_height l) = (black_height r)) 
	then (check_black_height l) && (check_black_height r)
	else failwith "black height is off"

  let well_formed cmp t = 
    well_ordered cmp t && 
      is_black t && (* prop 2 *)
      check_red_children t && (* prop 4 *)
      check_black_height t (* prop 5 *)

  let rec to_string to_s t = 
    let rec h = function
      | Empty -> ""
      | RNode(Empty,k,v,Empty) | BNode(Empty,k,v,Empty) -> to_s k v
      | RNode(l,k,v,Empty) | BNode(l,k,v,Empty) -> 
	  Printf.sprintf "%s, %s" (h l) (to_s k v)
      | RNode(Empty,k,v,r) | BNode(Empty,k,v,r) -> 
	  Printf.sprintf "%s, %s" (to_s k v) (h r)
      | RNode(l,k,v,r) | BNode(l,k,v,r) ->
	  Printf.sprintf "%s, %s, %s"
	    (h l) (to_s k v) (h r)
    in "{" ^ (h t) ^ "}"

  let rec min_keyval t = match t with
    | Empty -> raise Not_found
    | RNode(Empty,k,v,_) | BNode(Empty,k,v,_) -> k,v
    | RNode(l,_,_,_) | BNode(l,_,_,_) -> min_keyval l

  let rec max_keyval t = match t with
    | Empty -> raise Not_found
    | RNode(_,k,v,Empty) | BNode(_,k,v,Empty) -> k,v
    | RNode(_,_,_,r) | BNode(_,_,_,r) -> max_keyval r

  let min_key t = fst (min_keyval t)
  let max_key t = fst (max_keyval t)

  let rec find cmp x t = match t with
    | Empty -> raise Not_found
    | RNode(l,k,v,r) | BNode(l,k,v,r) -> match cmp x k with
	| 0 -> v
	| c when c < 0 -> find cmp x l
	| _ -> find cmp x r

  let mem cmp x t = try ignore(find cmp x t);true with Not_found -> false

  (* Okasaki's rebalancing constructor *)
  let bal_l l (k,v) r = match l with
    | RNode(RNode(t1,k1,v1,t2),k2,v2,t3)
    | RNode(t1,k1,v1,RNode(t2,k2,v2,t3)) ->
	RNode(BNode(t1,k1,v1,t2),k2,v2,BNode(t3,k,v,r))
    | _ -> BNode(l,k,v,r)

  let bal_r l (k,v) r = match r with
    | RNode(RNode(t2,k2,v2,t3),k3,v3,t4)
    | RNode(t2,k2,v2,RNode(t3,k3,v3,t4)) ->
	RNode(BNode(l,k,v,t2),k2,v2,BNode(t3,k3,v3,t4))
    | _ -> BNode(l,k,v,r)
      
  let rec ins cmp x y t = match t with
    | Empty -> RNode(Empty,x,y,Empty)
    | RNode(l,k,v,r) -> begin match cmp x k with
	| 0 -> t
	    (* impossible to violate black height property with a 
	       red node here, so no need to rebalance *)
	| c when c < 0 -> RNode(ins cmp x y l,k,v,r)
	| _ -> RNode(l,k,v,ins cmp x y r)
      end
    | BNode(l,k,v,r) -> begin match cmp x k with
	| 0 -> t
	| c when c < 0 -> bal_l (ins cmp x y l) (k,v) r
	| _ -> bal_r l (k,v) (ins cmp x y r)
      end

  let blackify = function
    | RNode(l,k,v,r) -> BNode(l,k,v,r)
    | t -> t

  let add cmp x y t = blackify (ins cmp x y t)

  let redify = function
    | BNode(l,k,v,r) -> RNode(l,k,v,r)
    | _ -> assert false

  let balance l (k,v) r = match l,k,v,r with
      (* TODO: investigate this first constructor proposed by Kahrs.
	 Is it better to move Red nodes up?*)
    | RNode(a,xk,xv,b),yk,yv,RNode(c,zk,zv,d) 
    | RNode(RNode(a,xk,xv,b),yk,yv,c),zk,zv,d
    | RNode(a,xk,xv,RNode(b,yk,yv,c)),zk,zv,d
    | a,xk,xv,RNode(b,yk,yv,RNode(c,zk,zv,d))
    | a,xk,xv,RNode(RNode(b,yk,yv,c),zk,zv,d) -> 
	RNode(BNode(a,xk,xv,b),yk,yv,BNode(c,zk,zv,d))

    | a,k,v,b -> BNode(a,k,v,b)

  let balleft l ((k,v) as elt) r = match l with
    | RNode(ll,lk,lv,lr) -> RNode(BNode(ll,lk,lv,lr),k,v,r)
    | _ -> match r with
	| BNode(rl,rk,rv,rr) -> balance l elt (RNode(rl,rk,rv,rr))
	| RNode(BNode(a,yk,yv,b),zk,zv,c) ->
	    RNode(BNode(l,k,v,a), yk, yv, (balance b (zk,zv) (redify c)))
	| _ -> assert false

  let balright l ((k,v) as elt) r = match r with
    | RNode(b,yk,yv,c) -> RNode(l,k,v,BNode(b,yk,yv,c))
    | _ -> match l with
	| BNode(a,xk,xy,b) -> balance (RNode(a,xk,xy,b)) elt r
	| RNode(a,xk,xv,BNode(b,yk,yv,c)) -> 
	    RNode(balance (redify a) (xk,xv) b, yk, yv, (BNode(c,k,v,l)))
	| _ -> assert false

  let rec app l r = match l,r with
    | Empty,_ -> r
    | _,Empty -> l
    | RNode(a,xk,xv,b), RNode(c,yk,yv,d) -> begin match app b c with
	| RNode(b',zk,zv,c') -> RNode(RNode(a,xk,xv,b'),zk,zv,RNode(c',yk,yv,d))
	| bc -> RNode(a,xk,xv,RNode(bc,yk,yv,d))
      end
    | BNode(a,xk,xv,b), BNode(c,yk,yv,d) -> begin match app b c with
	| RNode(b',zk,zv,c') -> RNode(BNode(a,xk,xv,b'),zk,zv,BNode(c',yk,yv,d))
	| bc -> balleft a (xk,xv) (BNode(bc, yk, yv, d))
      end
    | a, RNode(b,xk,xv,c) -> RNode(app a b, xk,xv, c)
    | RNode(a,xk,xv,b), c -> RNode(a,xk,xv,app b c)

  (* based on Stefan Kahrs work on RB trees *)
  let rec del cmp x t = match t with
    | Empty -> Empty
    | BNode(l,k,v,r) | RNode(l,k,v,r) -> match cmp x k with
	| 0 -> app l r
	| c when c < 0 -> del_left cmp x l (k,v) r
	| _ -> del_right cmp x l (k,v) r
  and del_left cmp x l ((k,v) as elt) r = match l with
    | BNode _ -> balleft (del cmp x l) elt r
    | _ -> RNode(del cmp x l, k,v, r)
  and del_right cmp x l ((k,v) as elt) r = match r with
    | BNode _ -> balright l elt (del cmp x r)
    | _ -> RNode(l,k,v,del cmp x r)

  let remove cmp x t = blackify (del cmp x t)

  (* join trees of arbitrary size *)
  (* This is still really inefficient since it keeps calling
     black_height which O(log n) raising this to O(n log n).  Should
     only call these once in union/diff/inter and then keep track of
     local differences.  *)
  let rec concat3h cmp l (k,v) r hl hr = 
    match hl - hr with
      | 0 -> begin match l,r with
	  | BNode _, BNode _ -> RNode(l,k,v,r)
	  | _ -> BNode(l,k,v,r)
	end
	  
      | -1 -> (* r has at exactly 1 extra black node *)
	  begin match l,r with
	    | _, Empty -> assert false (* r must have at least 2 black nodes *)
		
	    | RNode(ll,lk,lv,lr),_ ->
		(* if l is red, just color it black to match r *)
		BNode(BNode(ll,lk,lv,lr),k,v,r)
		  
	    | _,RNode(rl,rk,rv,rr) ->
		(* rl and rr must be black by (4) *)
		(* recurse to force l=blk rl=blk *)
		balance (concat3h cmp l (k,v) rl hl hr) (rk,rv) rr

	    | _,BNode(rl,rk,rv,rr) -> 
		begin match rl,rr with
		  | (BNode _|Empty), (BNode _|Empty) ->
		      (*both black, so color their parent red to drop BH,
			then use bnode as parent to restore height *)
		      BNode(l,k,v,RNode(rl,rk,rv,rr))
			
		  | RNode _, RNode _ ->
		      (* push black down to rr and connect rl with l *)
		      RNode(BNode(l,k,v,rl),rk,rv, blackify(rr))
			
		  | (BNode _|Empty), RNode _ ->
		      (* RNode(l,v,rl) will have same height as rr *)
		      BNode(RNode(l,k,v,rl),rk,rv,rr)
			
		  | RNode(rll,rlk,rlv,rlr), (BNode _|Empty) ->
		      (* rll and rlr are black, and all of l,rll,rlr,rr have same BH *)
		      RNode(BNode(l,k,v,rll), rlk, rlv, BNode(rlr,rk, rv,rr));
		end
	  end
      | 1 -> (* l has at exactly 1 extra black node *)
	  begin match l,r with
	    | Empty,_ -> assert false (* l must have at least 2 black nodes *)

	    | _,RNode(rl,rk,rv,rr) ->
		(* if r is red, just color it black to match l *)
		BNode(l,k,v,BNode(rl,rk,rv,rr))
		  
	    | RNode(ll,lk,lv,lr),_ -> 
		(* ll and lr must be black by (4) *)
		(* recurse to force l=blk rl=blk *)
		balance ll (lk,lv) (concat3h cmp lr (k,v) r hl hr)

	    | BNode(ll,lk,lv,lr),_ -> 
		begin match ll,lr with
		  | (BNode _|Empty), (BNode _|Empty) ->
		      (*both black, so color their parent red to drop BH,
			then use bnode as parent to restore height *)
		      BNode(RNode(ll,lk,lv,lr),k,v,r)
			
		  | RNode _, RNode _ ->
		      (* push black down to ll and connect lr with r *)
		      RNode(blackify(ll),lk,lv,BNode(lr,k,v,r))
			
		  | (BNode _|Empty), RNode(lrl,lrk,lrv,lrr) ->
		      (* lrl and lrr are black, and all of l,rll,rlr,rr have same BH *)
		      RNode(BNode(ll,lk,lv,lrl), lrk, lrv, BNode(lrr,k,v,r))
			
		  | RNode _, (BNode _|Empty) ->
		      (* RNode(lr,v,r) will have same height as ll *)
		      BNode(ll,lk,lv,RNode(lr,k,v,r))
		end
	  end
      | c when c < -1 -> (* r has at least 2 more black nodes *)
	  begin match r with
	    | Empty -> assert false
	    | RNode(rl,rk,rv,rr) ->
		let t1 = concat3h cmp l (k,v) rl hl hr in
		let hl = black_height t1 in
		let t2 = concat3h cmp t1 (rk,rv) rr hl hr in
		  t2
	    | BNode(rl,rk,rv,rr) ->
		let t1 = concat3h cmp l (k,v) rl hl (hr-1) in
		let hl = black_height t1 in
		let t2 = concat3h cmp t1 (rk,rv) rr hl (hr-1)in

		  t2
	  end
      | _ -> match l with (* l has at least 2 more black nodes *)
	  | Empty -> assert false
	  | RNode(ll,lk,lv,lr) ->
	      let t1 = concat3h cmp lr (k,v) r hl hr in
	      let hr = black_height t1 in
	      let t' = concat3h cmp ll (lk,lv) t1 hl hr in
		t'
	  | BNode(ll,lk,lv,lr) ->
	      let t1 = concat3h cmp lr (k,v) r (hl-1) hr in
	      let hr = black_height t1 in
	      let t' = concat3h cmp ll (lk,lv) t1 (hl-1) hr in
		t'
      
  and concat3 cmp l v r = 
    let hl = black_height l in
    let hr = black_height r in
      concat3h cmp l v r hl hr
		
  let rec split cmp s t = match t with
    | Empty -> Empty, Empty
    | BNode(l1,k,v,r1)
    | RNode(l1,k,v,r1) ->
	match cmp s k with
	  | 0 -> l1,r1
	  | c when c < 0 ->
	      let l2,r2 = split cmp s l1 in
	      let t' = concat3 cmp r2 (k,v) r1 in
		(l2,t')
	  | _ ->
	      let l2,r2 = split cmp s r1 in
	      let t' = concat3 cmp l1 (k,v) l2 in
		t', r2

  (* Inefficient, easy version for now *)
  let get_and_remove_min cmp t = 
    let (k,v as kv) = min_keyval t in
      kv, (remove cmp k t)

  (* Inefficient, easy version for now *)
  let concat cmp t1 t2 = 
    if is_empty t2
    then t1
    else 
      let rm,t2 = get_and_remove_min cmp t2 in
	concat3 cmp t1 rm t2

  let union cmp f t1 t2 = 
    let rec u t1 t2 = match t1,t2 with
      | Empty, t | t, Empty -> t
      | t1, (BNode(l,k,v,r) | RNode(l,k,v,r)) ->
	  let l',r' = split cmp k t1 in
	  let t' = concat cmp (u l' l) (u r' r) in
	    try let v' = find cmp k t1 in
	      add cmp k (f k v v') t'
	    with Not_found -> add cmp k v t'
    in blackify (u t1 t2)

  let rec diff cmp f t1 t2 = 
    let rec helper t1 t2 =  match t1,t2 with
      | Empty, _ -> Empty 
      | _, Empty -> t1
      | _, (BNode(l,k,v,r)|RNode(l,k,v,r)) ->
	  let l',r' = split cmp k t1 in
	    concat cmp (helper l' l) (helper r' r)
    in
      blackify (helper t1 t2)

  let rec inter cmp f t1 t2 = match t1,t2 with
    | Empty,_ | _,Empty -> Empty
    | t1, (BNode(l,k,v,r)|RNode(l,k,v,r)) ->
	let l',r' = split cmp k t1 in
	let t =
	  begin try
	    let v1 = find cmp k t1 in
	    let v2 = f k v v1 in
	      concat3 cmp (inter cmp f l' l) (k,v2) (inter cmp f r' r)
	  with Not_found ->concat cmp (inter cmp f l' l) (inter cmp f r' r)
	  end
	in blackify t

  let rec mapi f = function
    | Empty -> Empty
    | RNode(l,k,v,r) -> RNode(mapi f l, k, f k v, mapi f r)
    | BNode(l,k,v,r) -> BNode(mapi f l, k, f k v, mapi f r)

  let map f t = mapi (fun _ v -> f v) t

  let rec cardinal = function
    | Empty -> 0
    | BNode(l,_,_,r) | RNode(l,_,_,r) -> 1 + (cardinal l) + (cardinal r)

  let rec iter f = function
    | Empty -> ()
    | RNode(l,k,v,r) | BNode(l,k,v,r) ->
	iter f l; f k v; iter f r

  let rec fold f acc t = match t with
    | Empty -> acc
    | RNode(l,k,v,r) | BNode(l,k,v,r) ->
	fold f (f (fold f acc l) k v) r

  type ('a,'b) path = 
    | Top
    | PathL of ('a,'b) path * 'a * 'b * ('a,'b) tree * bool (* is_black *)
    | PathR of ('a,'b) tree * 'a * 'b * ('a,'b) path * bool (* is_black *)
	  
  type ('a,'b) curs = ('a,'b) path * ('a,'b) tree

  let to_cursor c = Top, c

  let has_value = function 
    | _,Empty -> false
    | _ -> true

  let get_value = function
    | _,Empty -> failwith "get_value"
    | _,RNode(_,k,v,_)
    | _,BNode(_,k,v,_) -> k,v

  let at_top = function (Top,_) -> true | _ -> false

  let at_left (_,t) = match t with
    | Empty -> true
    | _ -> false

  let at_right (_,t) = match t with
    | Empty -> true
    | _ -> false

  let went_left = function PathL _,_ -> true | _ -> false
  let went_right = function PathR _,_ -> true | _ -> false

  let try_color blk t = 
    if blk then blackify t
    else match t with
	(* try to color t red *)
      | Empty -> t (* can't *)
      | RNode _ -> t (* already *)
      | BNode(l', k', v', r') ->
	  if is_black l' && is_black r'
	  then RNode(l',k', v',r') (* can change to red and still satisfy (4) *)
	  else t (* have to leave it black *)

  let move_up cmp = function
    | Top, _ -> failwith "move_up"
    | PathL(p,k,v,r,blk),l
    | PathR(l,k,v,p,blk),r -> 
	let t = concat3 cmp l (k,v) r in
	  (* We try and keep the same color as the original tree if
	     possible so that we don't do any unnecessary rotations
	     when rebuilding the tree.  Besides being more efficient,
	     this is also required to make traversals work properly
	     (otherwise the tree might rotate in the middle of the
	     traversal, giving incorrect results *)
	let t = try_color blk t in
	  p, t

  let move_down_left = function
    | _,Empty -> failwith "move_down_left"
    | p, RNode(l,k,v,r) -> PathL(p,k,v,r,false),l
    | p, BNode(l,k,v,r) -> PathL(p,k,v,r,true),l
	
  let move_down_right = function
    | _,Empty -> failwith "move_down_right"
    | p,RNode(l,k,v,r) -> PathR(l,k,v,p,false),r
    | p,BNode(l,k,v,r) -> PathR(l,k,v,p,true),r

  let rec from_cursor cmp curs = 
    if at_top curs then blackify (snd curs)
    else from_cursor cmp (move_up cmp curs)

  (** Step the cursor one position "in-order".  Does not keep any
      state *)
  let rec step_io = function
    | Top, Empty -> raise Exit
    | PathL(p,k,v,r,_),Empty -> (k,v),(p,r)
    | p, RNode(l,k,v,r) -> step_io (PathL(p,k,v,r,false),l)
    | p, BNode(l,k,v,r) -> step_io (PathL(p,k,v,r,true),l)
    | PathR _, Empty -> assert false

  let can_step = function Top, Empty -> false | _ -> true
	
  let compare kcmp vcmp t1 t2 = 
    let rec helper c1 c2 = 
      match (can_step c1), (can_step c2) with
	| false, false -> 0
	| true, false -> -1
	| false, true -> 1
	| true, true ->
	    let (k1,v1),c1 = step_io c1 in
	    let (k2,v2),c2 = step_io c2 in
	      match kcmp k1 k2 with
		| 0 -> 
		    let c = vcmp v1 v2 in
		      if c = 0 then helper c1 c2 
		      else c
		| c -> c
    in
      helper (to_cursor t1) (to_cursor t2)

  let compare_keys kcmp t1 t2 = compare kcmp (fun _ _ -> 0) t1 t2

  let gen_ cmp 
      (kgen:?size:int -> Random.State.t -> 'a)
      (vgen:?size:int -> Random.State.t -> 'b) ?(size=50) rs : ('a,'b) tree = 
    let num = Random.State.int rs size in
    let rec loop n t = 
      if n <= 0 then t
      else
	let k = kgen ~size:size rs in
	let v = vgen ~size:size rs in
	  loop (n-1) (add cmp k v t)
    in
      loop num empty

end

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

  type ('a,'k,'v) result = 'a
  type ('a,'k,'v) result_ = 'a

  let add x t = add Pervasives.compare x t
  let mem x t = mem Pervasives.compare x t
  let remove x t = remove Pervasives.compare x t
  let find x t = find Pervasives.compare x t

  let union f t1 t2 = union Pervasives.compare f t1 t2

  let diff f t1 t2 = diff Pervasives.compare f t1 t2
  let inter f t1 t2 = inter Pervasives.compare f t1 t2
  let well_formed t = well_formed Pervasives.compare t

  let from_cursor c = from_cursor Pervasives.compare c
  let move_up c = move_up Pervasives.compare c

  let gen2 
      (kgen: ?size:int -> Random.State.t -> 'k) 
      (egen: ?size:int -> Random.State.t -> 'v)
      ?size rs : ('k,'v) tree = 
    gen_ Pervasives.compare kgen egen ?size rs
end

module MonoKeyMap(C : Types.Mono.Comparable) = struct
  include BaseMap

  type key = C.t
  type 'a key_ = C.t
      
  type 'e elt = 'e
  type 'e elt_ = 'e

  type 'v t = (C.t,'v) tree
  type ('k,'v) map = 'v t

  type 'v cursor = (C.t,'v) curs
  type ('k,'v) cursor_ = 'v cursor

  type ('a,'v) result = 'a
  type ('a,'k,'v) result_ = 'a

  let add x t = add C.compare x t
  let mem x t = mem C.compare x t
  let remove x t = remove C.compare x t
  let find x t = find C.compare x t

  let union f t1 t2 = union C.compare f t1 t2

  let diff f t1 t2 = diff C.compare f t1 t2
  let inter f t1 t2 = inter C.compare f t1 t2
  let well_formed t = well_formed C.compare t

  let from_cursor c = from_cursor C.compare c
  let move_up c = move_up C.compare c

  let compare vcmp t1 t2 = compare C.compare vcmp t1 t2
  let compare_keys t1 t2 = compare_keys C.compare t1 t2

  let to_string to_s t = 
    to_string (fun k v -> 
		 Printf.sprintf "(%s => %s)" (C.to_string k) (to_s v)
	      ) t

  let gen2 
      (kgen: ?size:int -> Random.State.t -> 'k) 
      (egen: ?size:int -> Random.State.t -> 'v)
      ?size rs : ('k,'v) tree = 
    gen_ C.compare kgen egen ?size rs
end

module GenKeyMap(C : Types.Mono.ArbitraryComparable) = struct
  include MonoKeyMap(C)
    
  let gen1(egen: ?size:int -> Random.State.t -> 'v) ?size rs : 'v t = 
    gen2 C.gen egen ?size rs
end

module MonoMap (K : Types.Mono.Comparable) (V : Types.Mono.Comparable) = struct
  include BaseMap

  type key = K.t
  type 'a key_ = K.t
      
  type elt = V.t
  type 'e elt_ = elt

  type t = (K.t,V.t) tree
  type ('k,'v) map = t

  type cursor = (K.t,V.t) curs
  type ('k,'v) cursor_ = cursor

  type 'a result = 'a
  type ('a,'k,'v) result_ = 'a

  let add x t = add K.compare x t
  let mem x t = mem K.compare x t
  let remove x t = remove K.compare x t
  let find x t = find K.compare x t

  let union f t1 t2 = union K.compare f t1 t2

  let diff f t1 t2 = diff K.compare f t1 t2
  let inter f t1 t2 = inter K.compare f t1 t2
  let well_formed t = well_formed K.compare t

  let from_cursor c = from_cursor K.compare c
  let move_up c = move_up K.compare c

  let compare t1 t2 = compare K.compare V.compare t1 t2
  let compare_keys t1 t2 = compare_keys K.compare t1 t2

  let to_string t = 
    to_string (fun k v -> 
		 Printf.sprintf "(%s => %s)" (K.to_string k) (V.to_string v)
	      ) t

  let gen2 
      (kgen: ?size:int -> Random.State.t -> 'k) 
      (egen: ?size:int -> Random.State.t -> 'v)
      ?size rs : ('k,'v) tree = 
    gen_ K.compare kgen egen ?size rs
end

module GenMap
  (K : Types.Mono.ArbitraryComparable) 
  (V : Types.Mono.ArbitraryComparable) = struct
  include MonoMap(K)(V)

  let gen ?size rs = gen2 K.gen V.gen ?size rs
end
