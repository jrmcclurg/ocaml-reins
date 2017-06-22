(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(*open Benchmark*)
open Bench_helper
(*
let all_suites = [
  Set_bench.suites;
]
*)
      

(*
let rec run_bench = function
  | BenchGroup lst -> 
      let res = latencyN ~style:Nil 500 lst in
	print_newline();
	tabulate res

  | BenchList lst -> List.iter run_bench lst
  | BenchLabel (label,bench) ->
      Printf.printf "start group: %s\n%!" label;
      run_bench bench
          
let _ = 
  List.iter run_bench all_suites
                                                 
*)
