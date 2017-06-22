(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Reins.Types

type ('a,'b) bench_func = {
  setup : 'a -> 'b;
  run : 'b -> 'b;
  teardown : 'b -> unit;
}


type ('a,'b) benchmark = 
  | BenchGroup of (string * ('a,'b) bench_func * 'a) list
  | BenchList of ('a,'b) benchmark list
  | BenchLabel of string * ('a,'b)benchmark

(*
let random_int_list n = 
  let rs = Random.State.make_self_init ()in
    loop n (fun y -> (Int.gen rs)::y) []
*)
