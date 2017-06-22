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

module MapTests1 = GenericMapTest.RandomMapTests(Int)(AVLMap.Gen1(Int)(Int))
module MapTests2 = GenericMapTest.RandomMapTests(Int)(AVLMap.Gen2(Int)(Int))
module MapTests3 = GenericMapTest.RandomMapTests(Int)(AVLMap.Gen3(Int)(Int))

let random_suite = 
  [ 
  ] @ MapTests1.random_suite @ MapTests2.random_suite @ MapTests3.random_suite


let unit_suite = 
  [
  ]
  @ MapTests1.unit_suite @ MapTests2.unit_suite @ MapTests3.unit_suite
