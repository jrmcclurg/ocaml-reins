(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Types

module Base = struct 
  type 'a tree = Node of int * 'a * 'a list * 'a tree list 
  type 'a skewheap = 'a tree list

  let empty = []

  let is_empty = function [] -> true | _ -> false
      
  let rank (Node(r,x,xs,c)) = r
  let root (Node(r,x,xs,c)) = x

  let link cmp (Node(r,x1,xs1,c1) as t1) (Node(_,x2,xs2,c2) as t2) = 
    if cmp x1 x2 <= 0 
    then Node(r+1,x1,xs1,t2::c1)
    else Node(r+1,x2,xs2,t1::c2)

  let skew_link cmp x t1 t2 = 
    let Node(r,y,ys,c) = link cmp t1 t2 in
      if cmp x y <= 0
      then Node(r,x,y::ys,c)
      else Node(r,y,x::ys,c)

  let rec insTree cmp t1 t = match t with
    | [] -> [t1]
    | t2::ts ->
	if rank t1 < rank t2
	then t1::t2::ts
	else insTree cmp (link cmp t1 t2) ts

  let rec mergeTrees cmp tl1 tl2 = match tl1,tl2 with
    | _,[] -> tl1
    | [],_ -> tl2
    | t1::ts1, t2::ts2 ->
	if rank t1 < rank t2 then t1::(mergeTrees cmp ts1 (t2::ts2))
	else if rank t2 < rank t1 then t2::(mergeTrees cmp (t1::ts1) ts2)
	else insTree cmp (link cmp t1 t2) (mergeTrees cmp ts1 ts2)

  let normalize cmp = function
    | [] -> []
    | t::ts -> insTree cmp t ts

  let insert cmp x ts = match ts with
    | t1::t2::rest ->
	if rank t1 = rank t2 
	then (skew_link cmp x t1 t2) :: rest
	else Node(0,x,[],[])::ts
    | _ -> Node(0,x,[],[])::ts

  let singleton x = [Node(0,x,[],[])]

	  
  let merge cmp ts1 ts2 = mergeTrees cmp (normalize cmp ts1) (normalize cmp ts2)

  let rec find_min cmp = function
    | [] -> raise Not_found
    | [t] -> root t
    | t::ts -> 
	let x = root t in
	let y = find_min cmp ts in
	  if cmp x y <= 0 then x else y

  let delete_min cmp = function
    | [] -> failwith "SkewBinomial:delete_min"
    | ts ->
	let rec get_min = function
	  | [] -> assert false
	  | [t] -> t,[]
	  | t::ts ->
	      let t',ts' = get_min ts in
		if cmp (root t) (root t') <= 0
		then t,ts
		else t', (t::ts')
	in
	let Node(_,x,xs,c),ts' = get_min ts in
	let rec insert_all t1 t2 = match t1 with
	  | [] -> t2
	  | x::xs -> insert_all xs (insert cmp x t2)
	in
	  insert_all xs (mergeTrees cmp (List.rev c) (normalize cmp ts'))

  let to_string cmp t = "<heap>"
end

module MonoHeap (C : Types.Mono.Comparable) = struct
  include Base
  type elt = C.t
  type 'a elt_ = elt
  type t = C.t skewheap
  type 'a heap = t

  let insert x t = insert C.compare x t
  let merge t1 t2 = merge C.compare t1 t2
  let find_min t = find_min C.compare t
  let delete_min t = delete_min C.compare t
  let to_string t = to_string C.compare t

end

module GenHeap (C : Types.Mono.ArbitraryComparable) = struct
  include MonoHeap(C)
  let gen ?(size=50) rs = 
    let num = Random.State.int rs size in
    let rec loop n t = 
      if n <= 0 then t
      else loop (n-1) (insert (C.gen rs) t)
    in
      loop num empty
end

module PolyHeap = struct
  include Base

  type 'a elt_ = 'a
  type 'a t = 'a skewheap
  type 'a heap = 'a skewheap

  let insert x t = insert Pervasives.compare x t
  let merge t1 t2 = merge Pervasives.compare t1 t2
  let find_min t = find_min Pervasives.compare t
  let delete_min t = delete_min Pervasives.compare t
  let to_string t = to_string t

end
