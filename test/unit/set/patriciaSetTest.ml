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

let desc = "Patricia Sets"

module SetTests = GenericSetTest.RandomSetTests(PatriciaSet.GenSet)(Int)
module IterTests = TreeSetIteratorTest.RandomTests(PatriciaSet.GenSet)(Int)

let random_suite = 
  [
  ] 
  @ SetTests.random_suite 
  @ IterTests.random_suite 

let unit_suite = 
  [
  ] 
  @ SetTests.unit_suite
  @ IterTests.unit_suite

