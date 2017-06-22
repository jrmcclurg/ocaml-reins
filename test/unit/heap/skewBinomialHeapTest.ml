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

let desc = "Skew Binomial"

module ITest = GenericHeapTest.RandomTests(SkewBinomialHeap.GenHeap)(Int)

let random_suite = 
  [
  ] @ ITest.random_suite

let unit_suite = [] @ ITest.unit_suite
