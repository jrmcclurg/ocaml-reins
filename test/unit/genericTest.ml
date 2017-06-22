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
open Test_helper

module ComparableTests(C : Mono.ArbitraryComparable) = struct

  let random_suite = 
    [
      (let module T = RandCheck(struct
	module Arg = C
	let desc = "Compare is reflexive"
	let law t = C.compare t t = 0
      end) in (T.desc, T.test));
      
      (let module T = RandCheck(struct
	module Arg = Mono.GenPair(C)(C)
	let desc = "Compare is anti-symmetric"
	let law (t1,t2) = 
	  let c1 = C.compare t1 t2 in
	  let c2 = C.compare t2 t1 in
	    c1 = -c2
      end) in (T.desc, T.test));
      
      (let module T = RandCheck(struct
	module Arg = Mono.Gen3Tuple(C)(C)(C)
	let desc = "Compare is transitive"
	let law (t1, t2, t3) = 
	  match (C.compare t1 t2), (C.compare t2 t3) with
	    | 0,0 -> (C.compare t1 t3) = 0
	    | x,y when x < 0 && y < 0 -> (C.compare t1 t3) < 0
	    | x,y when x > 0 && y > 0 -> (C.compare t1 t3) > 0
	    | x, y when x < 0 && y > 0 -> raise Quickcheck.Trivial
	    | x, y (* x > 0 && y < 0*) -> raise Quickcheck.Trivial
      end) in (T.desc, T.test));
    ]

  let unit_suite = []
end
