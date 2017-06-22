(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Reins
open Types
open Test_helper

module GenList = Mono.ComposeGenComparable(SList)

module type HOHeap = 
  functor(C : Mono.ArbitraryComparable) -> 
    Heaps.GenHeapSig with type elt = C.t

module RandomTests(H : HOHeap)(A : Mono.ArbitraryComparable) = struct
  module Heap = H(A)

  let unit_suite = []

  let random_suite = [

    (let module T = RandCheck(struct
      module Arg = A
      let desc = "ins/find 1 element"
      let law i = 
	let t = Heap.insert i Heap.empty in
	let i' = Heap.find_min t in
	  A.compare i i' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = A
      let desc = "ins/del is empty"
      let law i = 
	let t = Heap.insert i Heap.empty in
	let t = Heap.delete_min t in
	  Heap.is_empty t
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GenList(A)
      let desc = "insert list then find_min/del_min each gives sorted output"
      let law l = 
	let t = List.fold_left (fun acc x -> Heap.insert x acc) Heap.empty l in
	let lst' = 
	  let rec loop acc t = 
	    if Heap.is_empty t then acc
	    else loop ((Heap.find_min t)::acc) (Heap.delete_min t)
	  in loop [] t
	in
	let lst' = List.rev lst' in
	let sortlst = List.sort A.compare l in
	  lst' = sortlst
    end) in (T.desc, T.test));


  ]
end
