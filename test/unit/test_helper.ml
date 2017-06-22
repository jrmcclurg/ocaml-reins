(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Reins

module type TestSuite = sig
  val desc : string
  val unit_suite : OUnit.test list
  val random_suite : (string * (Random.State.t -> unit)) list
end

module Conf = struct
  let num_iterations = 100
  let size_arg = Some 100
  let max_trivial_percentage = 10.0
end

module RandCheck = Quickcheck.Check(Conf)

let rec do_times n f acc = 
  if n <= 0 then acc else do_times (n-1) f (f acc)
