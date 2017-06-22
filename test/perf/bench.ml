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

let time f arg =
  let prev = Unix.gettimeofday () in
  let _ = f arg in
  let aft = Unix.gettimeofday () in
    aft -. prev

let rec loop n f acc =
  if n <= 0 then acc
  else loop (n-1) f (f acc)

let random_int_list n = 
  let rs = Random.State.make_self_init ()in
    loop n (fun y -> (Int.gen rs)::y) []
