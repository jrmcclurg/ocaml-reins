(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Reins
open Quickcheck
open Types
open Lists
open Test_helper
open OUnit

let desc = "SkewBinaryList"

module G = Mono.ComposeGenComparable(SkewBinaryList)(Int)
module GTests = GenericListTest.Make(SkewBinaryList)(Int)

let random_suite = [
    (let module T = RandCheck(struct
      module Arg = Int
      let desc = "List of {0..n-1} can lookup {0,...,l-1}"
      let law len = 
	let len = (max 1 len) mod 100 in
	let rec f n l = 
	  if n < 0 then l 
	  else f (n-1) (SkewBinaryList.cons n l)
	in
	let l = f len SkewBinaryList.empty in
	  for i = 0 to (len-1) do
	    assert (SkewBinaryList.lookup i l = i)
	  done;
	  true
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = G
      let desc = "List of length l can update {0,...,l-1}"
      let law l = 
	let len = SkewBinaryList.length l in
	  for i = 0 to (len-1) do
	    ignore(SkewBinaryList.update i 10 l);
	  done;
	  true
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = G
      let desc = "lookup sees updates"
      let law l = 
	if SkewBinaryList.is_empty l then raise Trivial;
	let i = Int.gen (Random.State.make_self_init()) in
	let len = SkewBinaryList.length l in
	let idx = Random.int len in
	  SkewBinaryList.lookup idx (SkewBinaryList.update idx i l) = i
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = G
      let desc = "lookup failure raises Not_found"
      let law l = 
	let len = SkewBinaryList.length l in
	  try ignore(SkewBinaryList.lookup len l); false
	  with Not_found -> true
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = G
      let desc = "update failure raises Not_found"
      let law l = 
	let len = SkewBinaryList.length l in
	  try ignore(SkewBinaryList.update len 10 l); false
	  with Not_found -> true
    end) in (T.desc, T.test));

] @ GTests.random_suite

module L = SkewBinaryList

let unit_suite = 
  [
    
  ] @ GTests.unit_suite
