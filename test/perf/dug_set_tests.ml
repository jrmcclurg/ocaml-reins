(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Printf

open Reins
open Types

module ISet = AVLSet.GenSet(Int)

module ExSet = OracleSet.Extractor(Int)
module BenchSet = OracleSet.Benchmark(ISet)

module SetProf = DugProfile.Make(OracleSet)

let pure_acc iters = 
  Bench.loop iters (ExSet.add 10) ExSet.empty
  

module B1(S : Sets.GenSetSig with type elt = int) = struct
  let bench iters = 
    let rs = Random.State.make_self_init () in 
    let t0 = S.singleton 50 in
    let rec helper n acc = 
    if n <= 0 then acc
    else 
      let _ = S.add 60 t0 in 
      let t' = S.add (Int.gen rs) acc in
      let t'' = S.add (Int.gen rs) acc in
      let acc = S.union t' t'' in
	ignore(S.is_empty acc);
	ignore(S.is_empty t0);
	helper (n-1) acc
    in
      helper iters S.empty
end

let inst_bench iters = 
  let rs = Random.State.make_self_init () in 
  let t0 = ExSet.singleton 50 in
  let rec helper n acc = 
    if n <= 0 then acc
    else 
      let _ = ExSet.add 60 t0 in 
      let t' = ExSet.add (Int.gen rs) acc in
      let t'' = ExSet.add (Int.gen rs) acc in
      let acc = ExSet.union t' t'' in
	ignore(ExSet.is_empty acc);
	ignore(ExSet.is_empty t0);
	helper (n-1) acc
  in
    helper iters ExSet.empty

let real_bench iters = 
  let rs = Random.State.make_self_init () in 
  let t0 = ISet.singleton 50 in
  let rec helper n acc = 
    if n <= 0 then acc
    else 
      let _ = ISet.add 60 t0 in 
      let t' = ISet.add (Int.gen rs) acc in
      let t'' = ISet.add (Int.gen rs) acc in
      let acc = ISet.union t' t'' in
	ignore(ISet.is_empty acc);
	ignore(ISet.is_empty t0);
	helper (n-1) acc
  in
    helper iters ISet.empty

let test () = 
  let iters = 10000 in
  let start = Unix.gettimeofday () in
  let _ = inst_bench iters in
  let mid = Unix.gettimeofday () in
  let _ = real_bench iters in
  let fin = Unix.gettimeofday () in
    printf "wrapped: %f\n" (mid -. start);
    printf "actual: %f\n" (fin -. mid);
    let dug = ExSet.get_dug () in
    let prof = SetProf.profile dug in
    let s = SetProf.to_string prof in
      printf "profile: %s\n" s

let test_profile () = 
  let v0 = ExSet.singleton 10 in
  let v1 = ExSet.add 10 v0 in
  let v2 = ExSet.empty in
  let v3 = ExSet.add 20 v2 in
  let v4 = ExSet.choose v3 in
  let v5 = ExSet.union v1 v3 in
  let v6 = ExSet.remove 20 v5 in
  let v7 = ExSet.union v1 v6 in
  let v8 = ExSet.remove 10 v7 in
  let v9 = ExSet.mem 15 v7 in
  let v10 = ExSet.is_empty v8 in
    ignore(v4,v9,v10)

let test2 () = 
  let () = test_profile () in
  let dug = ExSet.get_dug () in
  let prof = SetProf.profile dug in
    printf "profile: %s\n" (SetProf.to_string prof);
    List.iter
      (fun (op,c) ->
	 printf " %f : %s\n" c (OracleSet.op_to_string (OracleSet.coerce_mut op))
      ) prof.SetProf.mut_cdf;
    prof

module SetGen = DugGenerator.Make(OracleSet)(Int)

let test_gen () = 
  (*let _ = inst_bench 100 in*)
  let _ = Bench.loop 500 test_profile () in
  let dug1 = ExSet.get_dug () in
  let prof1 = SetProf.profile dug1 in
  let _ = printf "profile: %s\n" (SetProf.to_string prof1) in
  let dug2 = SetGen.generate prof1 (Dug.Id.to_int (Dug.size dug1)) in
  let prof2 = SetProf.profile dug2 in
  let _ = printf "generated: %s\n" (SetProf.to_string prof2) in
  let tim = BenchSet.benchmark dug2 in
    printf "got time: %f\n" tim;
(*
  let dug3 = SetGen.generate prof2 (Dug.Id.to_int (Dug.size dug2)) in
  let prof3 = SetProf.profile dug3 in
  let _ = printf "regenerated: %s\n" (SetProf.to_string prof3) in
*)
    ()

module ASet = AVLSet.GenSet(Int)
module RBSet = RBSet.GenSet(Int)
module PatSet = PatriciaSet.GenSet

module ABench = OracleSet.Benchmark(ASet)
module RBBench = OracleSet.Benchmark(RBSet)
module PatBench = OracleSet.Benchmark(PatSet)

module A_B1 = B1(ASet)
module RB_B1 = B1(RBSet)
module Pat_B1 = B1(PatSet)
module Inst_B1 = B1(OracleSet.Extractor(Int))

let _ = 
  let () = Gc.compact () in
  let avl_real = Bench.time A_B1.bench 1000 in
  let () = Gc.compact () in
  let rb_real = Bench.time RB_B1.bench 1000 in
  let () = Gc.compact () in
  let pat_real = Bench.time Pat_B1.bench 1000 in
  let () = Gc.compact () in
  let inst = Bench.time inst_bench 1000 in
  let () = Gc.compact () in
  let dug = ExSet.get_dug () in
  let () = Gc.compact () in
  let avl = Bench.time BenchSet.benchmark dug in
  let () = Gc.compact () in
  let pat = Bench.time PatBench.benchmark dug in
  let () = Gc.compact () in
  let rb = Bench.time RBBench.benchmark dug in
    printf "avl actual: %f\n" avl_real;
    printf "r/b actual: %f\n" rb_real;
    printf "patricia actual: %f\n" pat_real;
    printf "instrumented: %f\n" inst;
    printf "AVL replay: %f\n" avl;
    printf "R/B replay: %f\n" rb;
    printf "Patricia replay: %f\n" pat

