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
open Quickcheck
open Types
open Lists

let desc = "CatenableList"

module GTests = GenericListTest.Make(CatenableList)(Int)

let random_suite = [
  
] @ GTests.random_suite

let unit_suite = GTests.unit_suite
