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
open Reins
open Test_helper

open Types

let desc = "Splay"

module M = SplayMap.GenMap(Int)(Int)

let top_key t = 
  let c = M.to_cursor t in
    fst (M.get_value c)
  
let mem_at_top i t = 
  let m,t = M.mem i t in
    assert_bool (sprintf "mem failed for %d" i) (m = true);
    assert_equal i (top_key t);
    t

let (++) f g = g f

let add_mem_test = 
  "add/mem sequential" >:: fun () ->
    let t = 
      M.add 1 1 M.empty ++
	M.add 2 2 ++
	M.add 3 3 ++
	M.add 4 4 ++
	M.add 5 5
    in
      ignore(mem_at_top 1 t ++
		mem_at_top 2 ++
		mem_at_top 3 ++
		mem_at_top 4 ++
		mem_at_top 5)

    
module MapTests = GenericMapTest.RandomMapTests(Int)(M)

let random_suite = 
  [
    
  ] 
  @ MapTests.random_suite 

let unit_suite =
  [
    add_mem_test
  ] 
  @ MapTests.unit_suite
