(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(* The type RList.t is based on skew binary numbers.  A skew binary
   number is comprised of a sequence of the digits 0,1, or 2 where
   the weight of the ith digit is 2^{i+1}-1 instead of the typical
   2^i with traditional binary numbers.  Furthermore, all skew
   binary numbers can be written where only the lowest non-zero
   digit is 2 (canonical form).
   
   To increment a skew binary number in canoncical form, we simply
   reset the (single) 2 to 0 and increment the next digit (which
   can't be a 2).  If there are no 2's, we can simply increment the
   lowest digit.  Here are the first 10 skew binary numbers to
   illustrate: 
   0
   1
   2
   10
   11
   12
   20
   100 # increment the 2, not the 0
   101
   102

   Decrementing is simply the reverse of this operation (decrement
   the 2, if it exists, otherwise decrement the lowest non-zero
   digit, possibly carrying a result one plce).  *)


(* The type elt is a complete binary tree.  
*)
type 'a elt = 
    | Leaf of 'a
    | Node of 'a * 'a elt * 'a elt

(* A random access list is a list of complete binary trees paired
   with their size.  These trees each have size 2^i-1 making them
   ideal for representing digits in this number system.  We
   represent a '2' digit by a pair of adjacent trees with the same
   weight.  Otherwise, each tree represents a '1' digit while '0'
   digits are omitted from the list.  The list is kept in increasing
   order of size so that a '2' digit exists iff the first two
   elements have the same height.  *)
type 'a t = (int * 'a elt) list

let empty = []
let is_empty = function [] -> true | _ -> false

(* The cons operation adds a new element to the list.  Therefore we
   must "increment" the skew binary number.  To do so, we look at
   the bottom two digits and compare their weights.  If they have
   the same weight, this is a '2' digit and therefore we reset it to
   0 and propogate the bit.  We do this by creating a new tree
   combining the two previous trees with an incremented weight (thus
   there is no tree with the original weight, creating the 0 digit).
   If the bottom digit is not a '2', then we can simply add 'x' as a
   leaf to the beginning of the list, incrementing the lowest digit
   (to either '1' or '2').
*)
let cons x ts = match ts with
  | (w1,t1)::(w2,t2)::ts' when w1=w2 ->
      (1+w1+w2, Node(x,t1,t2)) :: ts'
  | _ -> (1,Leaf x) :: ts

let hd = function
  | [] -> failwith "hd"
  | (w,Leaf x) :: ts -> assert(w=1);x
  | (w,Node(x,t1,t2)) :: ts -> x

(* The tl operation must remove an element from the list and
   therefore we must decrement the skew binary number.  To do this
   we simply remove the root of the first tree and add its children
   back as a new '2' digit (which has smaller weight than any other
   tree, preserving canonical form).
*)
let tl = function
  | [] -> failwith "tl"
  | (w,Leaf x) :: ts -> assert(w=1); ts
  | (w,Node(x,t1,t2))::ts ->
      (w / 2, t1) :: (w / 2, t2) :: ts

let pop = function
  | [] -> failwith "pop"
  | (w,Leaf x) :: ts -> assert(w=1); x,ts
  | (w,Node(x,t1,t2))::ts ->
      x, ((w / 2, t1) :: (w / 2, t2) :: ts)

(* Returns the i'th element in the complete tree 't'.  Raises
   Not_found if i is greater than the cardinality of the tree.  *)
let rec lookup_tree i = function
  | 1, Leaf x -> if i = 0 then x else raise Not_found
  | _, Leaf _ -> assert false
  | w, Node(x,t1,t2) -> 
      if i = 0 then x	(* The tree is pre-ordered, so the first 
			   element is at the root. *)

      else if i <= w/2 then lookup_tree (i-1) (w/2,t1)
	(* Decrement i, since we skip over the first element *)

      else lookup_tree (i-(1+(w/2))) (w/2,t2)
	(* Subtract 1+w/2 since we skip over the first element and
	   all of the elements in t1. *)

(* Return the i'th element (0-indexed) in the list.  We first find
   the tree which contains i'th element, and then call loookup_tree
   to extract the proper element from that tree.  Raises Not_found
   if the list does not have at least i+1 elements (+1 because of
   0-index).  *)
let rec lookup i = function
  | [] -> raise Not_found
  | ((w,t) as elt)::ts ->
      if i < w then lookup_tree i elt else lookup (i-w) ts
	
(* Returns the tree 't' with the i'th element replaced by 'y'.
   Raises Not_found if i is greater than the cardinality of the
   tree.  *)
let rec update_tree i y = function
  | (1, Leaf x) -> if i = 0 then Leaf y else raise Not_found
  | (_, Leaf _) -> assert false
  | (w, Node(x,t1,t2)) ->
      if i = 0 then Node(y,t1,t2) (* The tree is pre-ordered, so the
				     first element is at the root. *)
      else if i <= w / 2 
      then Node(x, update_tree (i-1) y (w/2,t1), t2)
	(* Decrement i, since we skip over the first element *)
      else Node(x,t1,update_tree (i-1-(w/2)) y (w/2,t2))
	(* Subtract 1+w/2 since we skip over the first element and
	   all of the elements in t1. *)

(* Return the list with the i'th element (0-indexed) in the list
   replaced by v.  We first find the tree which contains i'th
   element, and then call update_tree to update the proper element
   from that tree.  Raises Not_found if the list does not have at
   least i+1 elements (+1 because of 0-index).  *)
let rec update i v = function
  | [] -> raise Not_found
  | ((w,t) as l)::ts ->
      if i < w then (w, update_tree i v l)::ts
      else (w,t)::(update (i-w) v ts)

(* fold over a single tree *)
let rec fold_elt f acc = function
  | Leaf x -> f acc x
  | Node(x,t1,t2) ->
      fold_elt f (fold_elt f (f acc x) t1) t2

let fold_left f acc t = 
  List.fold_left (fun acc (_,e) -> fold_elt f acc e) acc t

let fold = fold_left

let rec iter_elt f = function
  | Leaf x -> f x
  | Node(x,t1,t2) ->
      f x;
      iter_elt f t1;
      iter_elt f t2

let iter f t = List.iter (fun (_,e) -> iter_elt f e) t

let length t = List.fold_left (fun acc (w,_) -> acc + w) 0 t

let from_list lst = 
  List.fold_left (fun acc x -> cons x acc) empty (List.rev lst)

let to_list t = List.rev (fold_left (fun acc x -> x::acc) [] t)

let rev t = fold_left (fun acc x -> cons x acc) empty t

(* This could be made more efficient if we were smarter about
   keeping existing trees and performing skew binary addition.  The
   worst case would still be O(n), but some cases would reduce to
   O(log n).  TODO *)
let append t1 t2 = fold_left (fun acc x -> cons x acc) t2 (rev t1)

(* Since this will cause all of the trees to shift by one, this
   operation is the worst case scenario of append. *)
let snoc x t = append t (cons x empty)

let rec last_tree = function
  | Leaf x -> x
  | Node(_,_,r) -> last_tree r

let rec last = function
  | [] -> failwith "last"
  | (w,t)::[] -> last_tree t
  | _::tl -> last tl

let rev_map f t = fold_left (fun acc t -> cons (f t) acc) empty t

let rec map_tree f = function
  | Leaf x -> Leaf (f x)
  | Node(x,t1,t2) -> Node(f x, map_tree f t1, map_tree f t2)
      
let map f t = 
  (* List.map is not tail recursive, but the list we are mapping is
     only (log n) long, so it will only use log n stack. *)
  List.map (fun (w,t) -> w, map_tree f t) t

(* We can't use append since it is O(n).  Instead we do two linear
   passes over the list of lists.  First we convert the list of type
   ('a t t) into a list of type ('a list list), but keep both the
   individual and aggregate lists in reverse order.  Then we simply
   fold over this collection accumulating the result using cons. *)
let flatten t = 
  let rlsts = 
    fold_left (fun acc lst -> (fold_left (fun acc x -> x::acc) [] lst)::acc) [] t 
  in
    List.fold_left (List.fold_left (fun acc x -> cons x acc)) empty rlsts

(* TODO: replace with more efficient version *)
let rec compare c t1 t2 = SList.compare c (to_list t1) (to_list t2)

let to_string to_s t = ListCommon.to_string iter pop to_s t

let gen genA ?(size=50) rs = 
  let t = SList.gen genA ~size:size rs in
    from_list t
      
