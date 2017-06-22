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
open Quickcheck
open Printf

let desc = "AVL"

module SetTests1 = GenericSetTest.RandomSetTests(AVLSet.GenSet1(Int))(Int)
module SetTests2 = GenericSetTest.RandomSetTests(AVLSet.GenSet2(Int))(Int)
module SetTests3 = GenericSetTest.RandomSetTests(AVLSet.GenSet3(Int))(Int)

module Iter1 = TreeSetIteratorTest.RandomTests(AVLSet.GenSet1(Int))(Int)
module Iter2 = TreeSetIteratorTest.RandomTests(AVLSet.GenSet2(Int))(Int)
module Iter3 = TreeSetIteratorTest.RandomTests(AVLSet.GenSet3(Int))(Int)

let random_suite = 
  [ 
  ] @ SetTests1.random_suite @ SetTests2.random_suite @ SetTests3.random_suite
    @ Iter1.random_suite @ Iter2.random_suite @ Iter3.random_suite


let unit_suite = 
  [
  ]
  @ SetTests1.unit_suite @ SetTests2.unit_suite @ SetTests3.unit_suite
  @ Iter1.unit_suite @ Iter2.unit_suite @ Iter3.unit_suite
