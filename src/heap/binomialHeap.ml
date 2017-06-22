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

  type 'a tree = Node of int * 'a * 'a tree list
    (* An element of type 'a tree is a binomial tree where the
       children are kept in a pre-order traversal with respect to
       their comparison function.
    *)

  type 'a binheap = 'a tree list
      (* A heap is a sparse collection of trees kept in increasing
	 order of rank. *)

  let empty = []

  let is_empty = function [] -> true | _ -> false

  let singleton x = [Node(0,x,[])]

  let link cmp t1 t2 = match t1,t2 with
    | Node(r,x1,c1), Node(_,x2,c2) ->
	if cmp x1 x2 <= 0
	then Node(r+1,x1,t2::c1)
	else Node(r+1,x2,t1::c2)

  let rank = function
    | Node(r,_,_) -> r

  (* find a tree with the same rank and link them *)
  let rec insTree cmp t1 h = match h with
    | [] -> [t1]
    | t2::rest as ts ->
	if rank t1 < rank t2
	then t1::ts
	else insTree cmp (link cmp t1 t2) rest

  let insert cmp x h = insTree cmp (Node(0,x,[])) h

  let rec merge cmp h1 h2 = match h1,h2 with
    | [], h | h, [] -> h
    | t1::t1s, t2::t2s ->
	if rank t1 < rank t2 then t1::(merge cmp t1s (t2::t2s))
	else if rank t2 < rank t1 then t2::(merge cmp (t1::t1s) t2s)
	else insTree cmp (link cmp t1 t2) (merge cmp t1s t2s)

  let root = function Node(_,v,_) -> v

  let rec find_min cmp = function
    | [] -> raise Not_found
    | t::[] -> root t
    | t::ts -> 
	let x = root t in
	let y = find_min cmp ts in
	  if cmp x y <= 0 then x else y

  let delete_min cmp = function
    | [] -> raise Not_found
    | ts -> 
	let rec get_min = function
	  | [] -> assert false
	  | t::[] -> t, []
	  | t::ts -> 
	      let t',ts' = get_min ts in
		if cmp (root t) (root t') <= 0
		then (t,ts)
		else (t', t::ts')
	in
	let Node(_,t,ts1),ts2 = get_min ts in
	  merge cmp (List.rev ts1) ts2

  let to_string cmp t = "<heap>"
end

module MonoHeap(C : Types.Mono.Comparable) = struct
  include Base
  type elt = C.t
  type 'a elt_ = elt
  type t = C.t binheap
  type 'a heap = t

  let insert x t = insert C.compare x t
  let merge t1 t2 = merge C.compare t1 t2
  let find_min t = find_min C.compare t
  let delete_min t = delete_min C.compare t

  let to_string t = to_string C.compare t
end

module GenHeap(C : Types.Mono.ArbitraryComparable) = struct
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
  type 'a t = 'a binheap
  type 'a heap = 'a t

  let insert x t = insert Pervasives.compare x t
  let merge t1 t2 = merge Pervasives.compare t1 t2
  let find_min t = find_min Pervasives.compare t
  let delete_min t = delete_min Pervasives.compare t

end
