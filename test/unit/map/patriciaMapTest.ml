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

module MapTests = GenericMapTest.RandomMapTests(Int)(PatriciaMap.GenMap(Int))

let random_suite = 
  [
  ] 
  @ MapTests.random_suite

let unit_suite = 
  [
  ] 
  @ MapTests.unit_suite

