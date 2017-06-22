(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open OUnit
open Reins
open Test_helper
open Types

let desc = "DoubleQueue"

module GenList = Mono.ComposeGenComparable(SList)

module GTests = GenericListTest.Make(DoubleQueue)(Int)

let random_suite = 
  [ 
    (let module T = RandCheck(struct
      module Arg = GenList(Int)
      let desc = "queue all elements in list.  repeated dequeue gives same order as list fold"
      let law l = 
	let q = List.fold_left (fun acc x -> DoubleQueue.enqueue x acc) 
	  DoubleQueue.empty l in
	let t,_ = 
	  List.fold_left (fun (t,acc) x -> 
	    let hd,tl = DoubleQueue.dequeue acc in
	      (t && hd = x),tl
	  ) (true,q) l 
	in
	  t
    end) in (T.desc, T.test));

  ] @ GTests.random_suite

let unit_suite = [

] @ GTests.unit_suite
