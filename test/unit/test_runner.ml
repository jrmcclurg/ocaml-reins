(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Printf
open OUnit
open Test_helper

module Test(T : TestSuite) = struct
  let test rs = T.desc >:::
    ["Unit Tests" >::: T.unit_suite;
     "Random Tests" >:::
       (List.map (fun (desc,suite) -> desc >:: (fun () -> suite rs)) T.random_suite)
    ]
end

let all_tests = fun rs -> "All Tests" >:::
  [ 
    (let module T = Test(SListTest) in T.test rs);
    (let module T = Test(DoubleQueueTest) in T.test rs);
    (let module T = Test(DoubleListTest) in T.test rs);
    (let module T = Test(CatenableListTest) in T.test rs);
    (let module T = Test(SkewBinaryListTest) in T.test rs);
    (let module T = Test(AVLSetTest) in T.test rs);
    (let module T = Test(RBSetTest) in T.test rs);
    (let module T = Test(SplaySetTest) in T.test rs);
    (let module T = Test(PatriciaSetTest) in T.test rs);
    (let module T = Test(AVLMapTest) in T.test rs);
    (let module T = Test(RBMapTest) in T.test rs);
    (let module T = Test(SplayMapTest) in T.test rs);
    (let module T = Test(PatriciaMapTest) in T.test rs);
    (let module T = Test(BinomialHeapTest) in T.test rs);
    (let module T = Test(SkewBinomialHeapTest) in T.test rs);
  ]

(*
let stime = ref 0.0
let time_tests = function
  | EStart p -> stime := Unix.gettimeofday ()
  | EEnd p -> 
      let dtime = Unix.gettimeofday () in
	printf "%f : %s\n%!" (dtime -. !stime) (string_of_path p)
  | EResult(RError(p,s)) -> failwith s
  | EResult(RFailure(p,s)) -> failwith s
  | EResult _ -> ()
*)
  
let _ = 
  Format.printf "Running unit tests\n";
  let rs = Random.State.make_self_init () in
(*
  let _ = perform_test time_tests (all_tests rs) in
    ()*)
  let _ = run_test_tt_main (all_tests rs) in 
    Format.printf "\n*** All tests passed ***\n\n"


