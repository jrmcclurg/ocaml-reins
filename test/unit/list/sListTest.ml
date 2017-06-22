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
open Types

module List_IT = ListIterator.From_List(SList)

module GTests = GenericListTest.Make(SList)(Int)

let desc = "Standard List"

let fold_test = 
  "iterator fold" >:: fun () ->
    let lst = [1;2;3;4;5] in
    let it = List_IT.create List_IT.Left_Right List_IT.Traverse_All lst in
    let it_ans = List_IT.fold (+) 0 it in
    let std_ans = List.fold_left (+) 0 lst in
      assert_equal ~printer:Int.to_string std_ans it_ans 
    
let unit_suite =
  [
    fold_test
  ] @ GTests.unit_suite

let random_suite = 
  [
  ] @ GTests.random_suite
