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

let desc = "DoubleList"

module DSList = DoubleList.Make(SList)

module G = Mono.ComposeGenComparable(DSList)(Int)
module GTests = GenericListTest.Make(DSList)(Int)

let random_suite = [
  ( let module T = RandCheck(struct
    module Arg = Mono.GenPair(G)(G)
    let desc = "splice list is eq to cons each individual"
    let law (l1,l2) = 
      let res1 = DSList.splice l1 l2 in
      let lfront = DSList.goto_front l1 in
      let res2 = 
	DSList.fold (fun acc x -> DSList.next (DSList.cons x acc)) l2 lfront
      in
	G.compare res1 res2 = 0
  end) in (T.desc, T.test));

  (let module T = RandCheck(struct
    module Arg = G
    let desc = "x = (prev (next x))"
    let law l = 
      if DSList.at_back l then true
      else G.compare l (DSList.prev (DSList.next l)) = 0
  end) in (T.desc, T.test));

  (let module T = RandCheck(struct
    module Arg = G
    let desc = "x = (next (prev x))"
    let law l = 
      if DSList.at_front l then true
      else G.compare l (DSList.next (DSList.prev l)) = 0
  end) in (T.desc, T.test));

  (let module T = RandCheck(struct
    module Arg = Mono.GenPair(Int)(G)
    let desc = "pop of (cons x l) is x,l"
    let law (i,l) = 
      let x,tl = DSList.pop (DSList.cons i l) in
	(Int.compare x i) = 0 && (G.compare tl l) = 0
  end) in (T.desc, T.test));
  
  (let module T = RandCheck(struct
    module Arg = Mono.GenPair(G)(G)
    let desc = "append l is same as to cons'ing individually at end"
    let law (l1,l2) = 
      let l = DSList.append l1 l2 in
      let l' = 
	DSList.fold
	  (fun acc x -> DSList.cons x (DSList.goto_back acc))
	  l1 (DSList.goto_front l2)
      in
	G.compare l l' = 0
  end) in (T.desc, T.test));
    
  (let module T = RandCheck(struct
    module Arg = Mono.ComposeGenComparable(DSList)(G)
    let desc = "DSList.flatten mirrors List.flatten"
    let law dll = 
      let lst = List.map DSList.to_list (DSList.to_list dll) in
      let lst' = List.flatten lst in
      let dll1 = DSList.from_list lst' in
      let dll2 = DSList.flatten dll in
	G.compare dll1 dll2 = 0
  end) in (T.desc, T.test));

] @ GTests.random_suite

let unit_suite = GTests.unit_suite
