(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Printf
open OUnit
open Test_helper
open Reins
open Types

let desc = "TreeSet Iterator"

module RandomTests
  (Tree : Sets.GenSetSig)
  (Elt : Mono.ArbitraryComparable with type t = Tree.elt) = 
struct
  module Iter = TreeSetIterator.Make(Tree)

  let tree_of_list lst = 
    List.fold_left (fun x y -> Tree.add y x) Tree.empty lst
      
  let list_of_iter it = 
    List.rev (Iter.fold (fun acc x -> x::acc) [] it)
      
  let assert_equal_ilist l1 l2 = 
    let module M = Mono.ComposeComparable(SList)(Int) in
      assert_equal ~cmp:(fun x y -> M.compare x y = 0)
	~printer:M.to_string l1 l2


  let unit_suite =
    [
      ("while false traverse" >:: fun () ->
	let rs = Random.State.make_self_init () in
	let trav = Iter.Traverse_While (fun _ -> false) in
	let s = Tree.singleton (Elt.gen rs) in
	let _ = Iter.create (Iter.Ascending Iter.InOrder) trav s in
	  ()
      );

    ]


  let random_suite = 
    [
      (let module T = RandCheck(struct
	module Arg = Mono.ComposeGen(SList)(Elt)
	let desc = "asc inorder is follows List.sort"
	let law lst = 
	  let sorted_lst = SList.sort Elt.compare lst in
	  let t = tree_of_list lst in
	  let dir = Iter.Ascending Iter.InOrder in
	  let it = Iter.create dir Iter.Traverse_All t in
	  let iter_lst = list_of_iter it in
	    if SList.compare Elt.compare sorted_lst iter_lst = 0
	    then true
	    else begin
		let msg = Printf.sprintf "sorted: %s iter: %s\n"
		  (SList.to_string Elt.to_string sorted_lst)
		  (SList.to_string Elt.to_string iter_lst)
		in
		  failwith msg
	      end
      end) in (T.desc, T.test));

      (let module T = RandCheck(struct
	module Arg = Mono.ComposeGen(SList)(Elt)
	let desc = "desc inorder is follows reversed List.sort"
	let law lst = 
	let sorted_lst = SList.sort (fun x y -> -(Elt.compare x y)) lst in
	let t = tree_of_list lst in
	let dir = Iter.Descending Iter.InOrder in
	let it = Iter.create dir Iter.Traverse_All t in
	let iter_lst = list_of_iter it in
	  SList.compare Elt.compare sorted_lst iter_lst = 0
      end) in (T.desc, T.test));

      (let module T = RandCheck(struct
	module Arg = Tree
	let desc = "asc inorder is reverse of desc inorder"
	let law t = 
	  let module I = TreeSetIterator.Make(Arg) in
	  let it1 = I.create (I.Ascending I.InOrder) I.Traverse_All t in
	  let it2 = I.create (I.Descending I.InOrder) I.Traverse_All t in
	  let lst1 = I.fold (fun acc x -> x::acc) [] it1 in
	  let lst2 = I.fold (fun acc x -> x::acc) [] it2 in
	  let module L = Mono.ComposeComparable(SList)(Elt) in
	    (L.compare lst1 (List.rev lst2)) = 0
      end) in (T.desc, T.test));

      (let module T = RandCheck(struct
	module Arg = Tree
	let desc = "asc preorder is reverse of desc preorder"
	let law t = 
	  let module I = TreeSetIterator.Make(Arg) in
	  let it1 = I.create (I.Ascending I.PreOrder) I.Traverse_All t in
	  let it2 = I.create (I.Descending I.PreOrder) I.Traverse_All t in
	  let lst1 = I.fold (fun acc x -> x::acc) [] it1 in
	  let lst2 = I.fold (fun acc x -> x::acc) [] it2 in
	  let module L = Mono.ComposeComparable(SList)(Elt) in
	    (L.compare lst1 (List.rev lst2)) = 0
      end) in (T.desc, T.test));

      (let module T = RandCheck(struct
	module Arg = Tree
	let desc = "asc postorder is reverse of desc postorder"
	let law t = 
	  let module I = TreeSetIterator.Make(Arg) in
	  let it1 = I.create (I.Ascending I.PostOrder) I.Traverse_All t in
	  let it2 = I.create (I.Descending I.PostOrder) I.Traverse_All t in
	  let lst1 = I.fold (fun acc x -> x::acc) [] it1 in
	  let lst2 = I.fold (fun acc x -> x::acc) [] it2 in
	  let module L = Mono.ComposeComparable(SList)(Elt) in
	    (L.compare lst1 (List.rev lst2)) = 0
      end) in (T.desc, T.test));

(*
      (let module T = RandCheck(struct
	module Arg = Tree
	let desc = "folding asc preorder follows Tree.fold"
	let law t = 
	  
	let module ISet = AVL.GenSet(Int) in
	let module I = TreeSetIterator.MonoIterator(ISet) in
	let s = ISet.add 3 ISet.empty in
	let it = I.create (I.Ascending I.PreOrder) I.Traverse_All s in
	let it_ans = I.fold (+) 0 it in
	let std_ans = 3 in
	  assert_equal ~printer:Int.to_string std_ans it_ans 
      end) in (T.desc, T.test));
*)
    ]


end


