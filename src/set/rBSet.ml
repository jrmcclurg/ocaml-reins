(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

module BaseSet = struct
    (* Red/Black Trees follow:
       1) all nodes are Red or Black
       2) The root is black 
       3) Empty Trees (i.e. leafs) are black
       4) Both children of a red node are black
       5) Every path from a leaf to the root has the same "black height"
    *)

  (* save a cell by encoding the color in the constructor *)
  type 'a tree = 
      | Empty
      | RNode of 'a tree * 'a * 'a tree
      | BNode of 'a tree * 'a * 'a tree

  let of_result x = x

  let empty = Empty
  let is_empty = function Empty -> true | _ -> false
  let singleton x = BNode(Empty,x,Empty)

  let is_black = function
    | Empty -> true
    | BNode _ -> true
    | RNode _ -> false

  let rec black_height t = 
    let rec bh acc = function
      | Empty -> 1+acc
      | RNode(l,_,r) -> bh acc l
      | BNode(l,_,r) -> bh (acc+1) l
    in bh 0 t

  (* true if the top of sub is lt x *)
  let sub_lt cmp x sub = match sub with 
    | RNode(_,y,_)
    | BNode(_,y,_) -> cmp y x < 0
    | _ -> assert false

  let sub_gt cmp x sub = match sub with
    | RNode(_,y,_)
    | BNode(_,y,_) -> cmp y x > 0
    | _ -> assert false

  let rec well_ordered cmp = function
    | Empty -> true
    | RNode(Empty,e,Empty) | BNode(Empty,e,Empty) -> true

    | BNode(Empty,e,r)
    | RNode(Empty,e,r) -> sub_gt cmp e r && well_ordered cmp r
    | BNode(l,e,Empty)
    | RNode(l,e,Empty) -> sub_lt cmp e l && well_ordered cmp l

    | RNode(l,e,r) | BNode(l,e,r) ->
	sub_lt cmp e l && sub_gt cmp e r && 
	  well_ordered cmp l && well_ordered cmp r

  let rec check_red_children = function
    | Empty -> true
    | BNode(l,_,r) -> check_red_children l && check_red_children r
    | RNode(l,_,r) -> is_black l && is_black r && 
	check_red_children l && check_red_children r

  let rec check_black_height = function
    | Empty -> true
    | RNode(l,_,r) | BNode(l,_,r) ->
	if ((black_height l) = (black_height r)) 
	then (check_black_height l) && (check_black_height r)
	else failwith "black height is off"

  let well_formed_not1 cmp t =
    well_ordered cmp t && 
      check_red_children t && (* prop 4 *)
      check_black_height t (* prop 5 *)

  let well_formed cmp t = 
    well_ordered cmp t && 
      is_black t && (* prop 2 *)
      check_red_children t && (* prop 4 *)
      check_black_height t (* prop 5 *)

  let rec to_string to_s t = 
    let rec h = function
      | Empty -> ""
      | RNode(Empty,v,Empty) | BNode(Empty,v,Empty) -> to_s v
      | RNode(l,v,Empty) | BNode(l,v,Empty) -> 
	  Printf.sprintf "%s, %s" (h l) (to_s v)
      | RNode(Empty,v,r) | BNode(Empty,v,r) -> 
	  Printf.sprintf "%s, %s" (to_s v) (h r)
      | RNode(l,v,r) | BNode(l,v,r) ->
	  Printf.sprintf "%s, %s, %s"
	    (h l) (to_s v) (h r)
    in "{" ^ (h t) ^ "}"
	    
  let rec min_elt t = match t with
    | Empty -> raise Not_found
    | RNode(Empty,elt,_) | BNode(Empty,elt,_) -> elt
    | RNode(l,_,_) | BNode(l,_,_) -> min_elt l

  let rec max_elt t = match t with
    | Empty -> raise Not_found
    | RNode(_,elt,Empty) | BNode(_,elt,Empty) -> elt
    | RNode(_,_,r) | BNode(_,_,r) -> max_elt r

  let rec mem cmp x t = match t with
    | Empty -> false
    | RNode(l,elt,r) | BNode(l,elt,r) -> match cmp x elt with
	| 0 -> true
	| c when c < 0 -> mem cmp x l
	| _ -> mem cmp x r

  (* Okasaki's rebalancing constructor *)
  let bal_l l elt r = match l with
    | RNode(RNode(t1,a1,t2),a2,t3)
    | RNode(t1,a1,RNode(t2,a2,t3)) ->
	RNode(BNode(t1,a1,t2),a2,BNode(t3,elt,r))
    | _ -> BNode(l,elt,r)

  let bal_r l elt r = match r with
    | RNode(RNode(t2,a2,t3),a3,t4)
    | RNode(t2,a2,RNode(t3,a3,t4)) ->
	RNode(BNode(l,elt,t2),a2,BNode(t3,a3,t4))
    | _ -> BNode(l,elt,r)
      
  let rec ins cmp x t = match t with
    | Empty -> RNode(Empty,x,Empty)
    | RNode(l,elt,r) -> begin match cmp x elt with
	| 0 -> t
	    (* impossible to violate black height property with a 
	       red node here, so no need to rebalance *)
	| c when c < 0 -> RNode(ins cmp x l,elt,r)
	| _ -> RNode(l,elt,ins cmp x r)
      end
    | BNode(l,elt,r) -> begin match cmp x elt with
	| 0 -> t
	| c when c < 0 -> bal_l (ins cmp x l) elt r
	| _ -> bal_r l elt (ins cmp x r)
      end

  let blackify = function
    | RNode(l,elt,r) -> BNode(l,elt,r)
    | t -> t

  let add cmp x t = blackify (ins cmp x t)

  let redify = function
    | BNode(l,e,r) -> RNode(l,e,r)
    | _ -> assert false

  let balance l v r = match l,v,r with
      (* TODO: investigate this first constructor proposed by Kahrs.
	 Is it better to move Red nodes up?*)
    | RNode(a,x,b),y,RNode(c,z,d) 
    | RNode(RNode(a,x,b),y,c),z,d
    | RNode(a,x,RNode(b,y,c)),z,d
    | a,x,RNode(b,y,RNode(c,z,d))
    | a,x,RNode(RNode(b,y,c),z,d) -> RNode(BNode(a,x,b),y,BNode(c,z,d))

    | a,x,b -> BNode(a,x,b)

  let balleft l elt r = match l with
    | RNode(ll,lv,lr) -> RNode(BNode(ll,lv,lr),elt,r)
    | _ -> match r with
	| BNode(rl,rv,rr) -> balance l elt (RNode(rl,rv,rr))
	| RNode(BNode(a,y,b),z,c) ->
	    RNode(BNode(l,elt,a), y, (balance b z (redify c)))
	| _ -> assert false

  let balright l elt r = match r with
    | RNode(b,y,c) -> RNode(l,elt,BNode(b,y,c))
    | _ -> match l with
	| BNode(a,x,b) -> balance (RNode(a,x,b)) elt r
	| RNode(a,x,BNode(b,y,c)) -> 
	    RNode(balance (redify a) x b, y, (BNode(c,elt,l)))
	| _ -> assert false

  let rec app l r = match l,r with
    | Empty,_ -> r
    | _,Empty -> l
    | RNode(a,x,b), RNode(c,y,d) -> begin match app b c with
	| RNode(b',z,c') -> RNode(RNode(a,x,b'),z,RNode(c',y,d))
	| bc -> RNode(a,x,RNode(bc,y,d))
      end
    | BNode(a,x,b), BNode(c,y,d) -> begin match app b c with
	| RNode(b',z,c') -> RNode(BNode(a,x,b'),z,BNode(c',y,d))
	| bc -> balleft a x (BNode(bc, y, d))
      end
    | a, RNode(b,x,c) -> RNode(app a b, x, c)
    | RNode(a,x,b), c -> RNode(a,x,app b c)

  (* based on Stefan Kahrs work on RB trees *)
  let rec del cmp x t = match t with
    | Empty -> Empty
    | BNode(l,elt,r) | RNode(l,elt,r) -> match cmp x elt with
	| 0 -> app l r
	| c when c < 0 -> del_left cmp x l elt r
	| _ -> del_right cmp x l elt r
  and del_left cmp x l elt r = match l with
    | BNode _ -> balleft (del cmp x l) elt r
    | _ -> RNode(del cmp x l, elt, r)
  and del_right cmp x l elt r = match r with
    | BNode _ -> balright l elt (del cmp x r)
    | _ -> RNode(l,elt,del cmp x r)

  let remove cmp x t = blackify (del cmp x t)

  (* join trees of arbitrary size *)
  (* This is still really inefficient since it keeps calling
     black_height which O(log n) raising this to O(n log n).  Should
     only call these once in union/diff/inter and then keep track of
     local differences.  *)
  let rec concat3h cmp l v r hl hr = 
    match hl - hr with
      | 0 -> begin match l,r with
	  | BNode _, BNode _ -> RNode(l,v,r)
	  | _ -> BNode(l,v,r)
	end
	  
      | -1 -> (* r has at exactly 1 extra black node *)
	  begin match l,r with
	    | _, Empty -> assert false (* r must have at least 2 black nodes *)
		
	    | RNode(ll,lv,lr),_ ->
		(* if l is red, just color it black to match r *)
		BNode(BNode(ll,lv,lr),v,r)
		  
	    | _,RNode(rl,rv,rr) ->
		(* rl and rr must be black by (4) *)
		(* recurse to force l=blk rl=blk *)
		balance (concat3h cmp l v rl hl hr) rv rr

	    | _,BNode(rl,rv,rr) -> 
		begin match rl,rr with
		  | (BNode _|Empty), (BNode _|Empty) ->
		      (*both black, so color their parent red to drop BH,
			then use bnode as parent to restore height *)
		      BNode(l,v,RNode(rl,rv,rr))
			
		  | RNode _, RNode _ ->
		      (* push black down to rr and connect rl with l *)
		      RNode(BNode(l,v,rl),rv, blackify(rr))
			
		  | (BNode _|Empty), RNode _ ->
		      (* RNode(l,v,rl) will have same height as rr *)
		      BNode(RNode(l,v,rl),rv,rr)
			
		  | RNode(rll,rlv,rlr), (BNode _|Empty) ->
		      (* rll and rlr are black, and all of l,rll,rlr,rr have same BH *)
		      RNode(BNode(l,v,rll), rlv, BNode(rlr,rv,rr));
		end
	  end
      | 1 -> (* l has at exactly 1 extra black node *)
	  begin match l,r with
	    | Empty,_ -> assert false (* l must have at least 2 black nodes *)

	    | _,RNode(rl,rv,rr) ->
		(* if r is red, just color it black to match l *)
		BNode(l,v,BNode(rl,rv,rr))
		  
	    | RNode(ll,lv,lr),_ -> 
		(* ll and lr must be black by (4) *)
		(* recurse to force l=blk rl=blk *)
		balance ll lv (concat3h cmp lr v r hl hr)

	    | BNode(ll,lv,lr),_ -> 
		begin match ll,lr with
		  | (BNode _|Empty), (BNode _|Empty) ->
		      (*both black, so color their parent red to drop BH,
			then use bnode as parent to restore height *)
		      BNode(RNode(ll,lv,lr),v,r)
			
		  | RNode _, RNode _ ->
		      (* push black down to ll and connect lr with r *)
		      RNode(blackify(ll),lv,BNode(lr,v,r))
			
		  | (BNode _|Empty), RNode(lrl,lrv,lrr) ->
		      (* lrl and lrr are black, and all of l,rll,rlr,rr have same BH *)
		      RNode(BNode(ll,lv,lrl), lrv, BNode(lrr,v,r))
			
		  | RNode _, (BNode _|Empty) ->
		      (* RNode(lr,v,r) will have same height as ll *)
		      BNode(ll,lv,RNode(lr,v,r))
		end
	  end
      | c when c < -1 -> (* r has at least 2 more black nodes *)
	  begin match r with
	    | Empty -> assert false
	    | RNode(rl,rv,rr) ->
		let t1 = concat3h cmp l v rl hl hr in
		let hl = black_height t1 in
		let t2 = concat3h cmp t1 rv rr hl hr in
		  t2
	    | BNode(rl,rv,rr) ->
		let t1 = concat3h cmp l v rl hl (hr-1) in
		let hl = black_height t1 in
		let t2 = concat3h cmp t1 rv rr hl (hr-1)in

		  t2
	  end
      | _ -> match l with (* l has at least 2 more black nodes *)
	  | Empty -> assert false
	  | RNode(ll,lv,lr) ->
	      let t1 = concat3h cmp lr v r hl hr in
	      let hr = black_height t1 in
	      let t' = concat3h cmp ll lv t1 hl hr in
		t'
	  | BNode(ll,lv,lr) ->
	      let t1 = concat3h cmp lr v r (hl-1) hr in
	      let hr = black_height t1 in
	      let t' = concat3h cmp ll lv t1 (hl-1) hr in
		t'
      
  and concat3 cmp l v r = 
    let hl = black_height l in
    let hr = black_height r in
      concat3h cmp l v r hl hr
		
  let rec split cmp v t = match t with
    | Empty -> Empty, Empty
    | BNode(l1,elt,r1)
    | RNode(l1,elt,r1) ->
	match cmp v elt with
	  | 0 -> l1,r1
	  | c when c < 0 ->
	      let l2,r2 = split cmp v l1 in
	      let t' = concat3 cmp r2 elt r1 in
		(*assert(well_formed_not1 cmp t');*)
		(l2,t')
	  | _ ->
	      let l2,r2 = split cmp v r1 in
	      let t' = concat3 cmp l1 elt l2 in
		(*assert(well_formed_not1 cmp t');*)
		(t'), r2

  let union cmp t1 t2 = 
    let rec u t1 t2 = match t1,t2 with
      | Empty, t | t, Empty -> t
      | t1, (BNode(l,v,r) | RNode(l,v,r)) ->
	  let l',r' = split cmp v t1 in
	  let t' = concat3 cmp (u l' l) v (u r' r) in
	    (*assert(well_formed_not1 cmp t');*)
	    t'
    in blackify (u t1 t2)

  (* Inefficient, easy version for now *)
  let get_and_remove_min cmp t = 
    let m = min_elt t in
      m, (remove cmp m t)

  (* Inefficient, easy version for now *)
  let concat cmp t1 t2 = 
    if is_empty t2
    then t1
    else 
      let rm,t2 = get_and_remove_min cmp t2 in
	concat3 cmp t1 rm t2

  let rec diff cmp t1 t2 = 
    let rec helper t1 t2 =  match t1,t2 with
      | Empty, _ -> Empty 
      | _, Empty -> t1
      | _, (BNode(l,v,r)|RNode(l,v,r)) ->
	  let l',r' = split cmp v t1 in
	    concat cmp (helper l' l) (helper r' r)
    in
      blackify (helper t1 t2)

  let rec inter cmp t1 t2 = match t1,t2 with
    | Empty,_ | _,Empty -> Empty
    | t1, (BNode(l,v,r)|RNode(l,v,r)) ->
	let l',r' = split cmp v t1 in
	let t =
	  if mem cmp v t1 
	  then concat3 cmp (inter cmp l' l) v (inter cmp r' r)
	  else concat cmp (inter cmp l' l) (inter cmp r' r)
	in blackify t

  let rec cardinal = function
    | Empty -> 0
    | BNode(l,_,r) | RNode(l,_,r) -> 1 + (cardinal l) + (cardinal r)

  let choose = function
    | Empty -> raise Not_found
    | BNode(_,v,_) | RNode(_,v,_) -> v

  let rec iter f = function
    | Empty -> ()
    | RNode(l,v,r) | BNode(l,v,r) ->
	iter f l; f v; iter f r

  let rec fold f acc t = match t with
    | Empty -> acc
    | RNode(l,v,r) | BNode(l,v,r) ->
	fold f (f (fold f acc l) v) r

  type 'a digit = 
      | One  of 'a * 'a tree
      | Two of 'a * 'a tree * 'a * 'a tree
	  
  let rec incr a1 t1 ds = match ds with
    | [] -> [One(a1,t1)]
    | One(a2,t2)::tl -> Two(a1,t1,a2,t2) :: tl
    | Two(a2,t2,a3,t3)::tl -> 
	One(a1,t1) :: (incr a2 (BNode(t2,a3,t3)) tl)

  let link l = function
    | One(a,t) -> BNode(l,a,t)
    | Two(a1,t1,a2,t2) -> BNode(RNode(l,a1,t1),a2,t2)

  let linkall lst = 
    List.fold_right (fun dig t -> link t dig) lst Empty

(*  let add a lst = incr a Empty lst

  let bottom_up lst = 
      linkall (List.fold_right add lst [])*)

  type 'a path = 
    | Top
    | PathL of 'a path * 'a * 'a tree * bool (* is_black *)
    | PathR of 'a tree * 'a * 'a path * bool (* is_black *)
	  
  type 'a curs = 'a path * 'a tree

  let to_cursor c = Top, c

  let has_value = function 
    | _,Empty -> false
    | _ -> true

  let get_value = function
    | _,Empty -> failwith "get_value"
    | _,RNode(_,v,_)
    | _,BNode(_,v,_) -> v

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
      | BNode(l', v', r') ->
	  if is_black l' && is_black r'
	  then RNode(l',v',r') (* can change to red and still satisfy (4) *)
	  else t (* have to leave it black *)


  let move_up cmp = function
    | Top, _ -> failwith "move_up"
    | PathL(p,x,r,blk),l
    | PathR(l,x,p,blk),r -> 
	let t = concat3 cmp l x r in
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
    | p, RNode(l,v,r) -> PathL(p,v,r,false),l
    | p, BNode(l,v,r) -> PathL(p,v,r,true),l
	
  let move_down_right = function
    | _,Empty -> failwith "move_down_right"
    | p,RNode(l,v,r) -> PathR(l,v,p,false),r
    | p,BNode(l,v,r) -> PathR(l,v,p,true),r

  let rec from_cursor cmp curs = 
    if at_top curs then blackify (snd curs)
    else from_cursor cmp (move_up cmp curs)

  (** Step the cursor one position "in-order".  Does not keep any
      state *)
  let rec step_io = function
    | Top, Empty -> raise Exit
    | PathL(p,x,r,_),Empty -> x,(p,r)
    | p, RNode(l,x,r) -> step_io (PathL(p,x,r,false),l)
    | p, BNode(l,x,r) -> step_io (PathL(p,x,r,true),l)
    | PathR _, Empty -> assert false

  let can_step = function Top, Empty -> false | _ -> true
	
  let cmp kcmp t1 t2 = 
    let rec helper c1 c2 = 
      match (can_step c1), (can_step c2) with
	| false, false -> 0
	| true, false -> -1
	| false, true -> 1
	| true, true ->
	    let x1,c1 = step_io c1 in
	    let x2,c2 = step_io c2 in
	      match kcmp x1 x2 with
		| 0 -> helper c1 c2
		| c -> c
    in
      helper (to_cursor t1) (to_cursor t2)
	
  let gen_ cmp (agen:?size:int -> Random.State.t -> 'a) ?(size=50) rs = 
    let num = Random.State.int rs size in
    let rec loop n t = 
      if n <= 0 then t
      else loop (n-1) (add cmp (agen ~size:size rs) t)
    in
      loop num empty

end
module PolySet (*: Tree.PolyTreeSet*) = struct
  include BaseSet

  type 'a t = 'a tree
  type 'a set = 'a t
  type 'a elt_ = 'a

  type ('a,'b) result = 'a
  type ('a,'b) result_ = 'a

  let add x t = add Pervasives.compare x t
  let mem x t = mem Pervasives.compare x t
  let remove x t = remove Pervasives.compare x t
  let union t1 t2 = union Pervasives.compare t1 t2
  let diff t1 t2 = diff Pervasives.compare t1 t2
  let inter t1 t2 = inter Pervasives.compare t1 t2
  let compare x y = cmp Pervasives.compare x y
  let equal x y = compare x y = 0

  let well_formed t = well_formed Pervasives.compare t

  type 'a cursor = 'a curs
  type 'a cursor_ = 'a cursor
  let move_up c = move_up Pervasives.compare c
  let from_cursor c = from_cursor Pervasives.compare c

  let gen1 (agen:?size:int -> Random.State.t -> 'a) ?size rs = 
    gen_ Pervasives.compare agen ?size rs
end

module MonoSet (C : Types.Mono.Comparable) = 
struct
  include BaseSet

  type elt = C.t
  type 'a elt_ = elt

  type t = C.t tree
  type 'a set = t

  type 'a result = 'a
  type ('a,'b) result_ = 'a

  let add x t = add C.compare x t
  let mem x t = mem C.compare x t
  let remove x t = remove C.compare x t
  let union t1 t2 = union C.compare t1 t2
  let diff t1 t2 = diff C.compare t1 t2
  let inter t1 t2 = inter C.compare t1 t2
  let compare x y = cmp C.compare x y
  let equal x y = compare x y = 0

  let well_formed t = well_formed C.compare t
  let to_string t = to_string C.to_string t

  type cursor = C.t curs
  type 'a cursor_ = cursor

  let move_up c = move_up C.compare c
  let from_cursor c = from_cursor C.compare c

  let gen1 (agen:?size:int -> Random.State.t -> 'a) ?size rs = 
    gen_ C.compare agen ?size rs

end

module GenSet (C : Types.Mono.ArbitraryComparable) = struct
  include MonoSet(C)

  let gen ?size rs = gen1 C.gen ?size rs

end
