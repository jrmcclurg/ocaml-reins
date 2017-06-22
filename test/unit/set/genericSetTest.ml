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

module GenList = Mono.ComposeGenComparable(SList)

module RandomSetTests(GSet : Sets.GenSetSig)
  (A : Mono.ArbitraryComparable with type t = GSet.elt) = struct

  let add_list t l = List.fold_left (fun t e -> GSet.add e t) t l

  module CmpTests = GenericTest.ComparableTests(GSet)

  let random_suite = CmpTests.random_suite @ [

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(A)(GenList(A))
      let desc = "[Set] Add first element"
      let law (i,lst) = 
	let t = GSet.add i GSet.empty in
	let t = add_list t lst in
	  GSet.of_result (GSet.mem i t)
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg =  Mono.Gen3Tuple(GSet)(A)(GenList(A))
      let desc = "[Set] Add middle element"
      let law (t,i,lst) = 
	let t = GSet.add i t in
	let t = add_list t lst in
	  GSet.of_result (GSet.mem i t)
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg =  Mono.GenPair(A)(GSet)
      let desc = "[Set] Add last element"
      let law (i,t) = 
	GSet.of_result (GSet.mem i (GSet.add i t))
    end) in (T.desc, T.test));
    
    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(A)(GenList(A))
      let desc = "[Set] Remove first element"
      let law (i,lst) = 
	let t = GSet.add i GSet.empty in
	let t = add_list t lst in
	let t = GSet.remove i t in
	  not (GSet.of_result (GSet.mem i t))
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.Gen3Tuple(GSet)(A)(GenList(A))
      let desc = "[Set] Remove middle element"
      let law (t,i,lst) = 
	let t = GSet.add i t in
	let t = add_list t lst in
	let t = GSet.remove i t in
	  not (GSet.of_result (GSet.mem i t))
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GSet)(A)
      let desc = "[Set] Remove last element"
      let law (t,i) = 
	let t = GSet.add i t in
	let t = GSet.remove i t in
	  not (GSet.of_result (GSet.mem i t))
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GSet)(GSet)
      let desc = "[Set] Union is commutative"
      let law (t1,t2) = 
	let t = GSet.union t1 t2 in
	let t' = GSet.union t2 t1 in
	  GSet.compare t t' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GenList(A))(GenList(A))
      let desc = "[Set] Union follows list concatenation"
      let law (l1,l2) = 
	let t1 = add_list GSet.empty l1 in
	let t2 = add_list GSet.empty l2 in
	let t = GSet.union t1 t2 in
	let t' = add_list GSet.empty (l1 @ l2) in
	  GSet.compare t t' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GSet)(GSet)
      let desc = "[Set] Intersection is commutative"
      let law (t1,t2) = 
	let t = GSet.inter t1 t2 in
	let t' = GSet.inter t2 t1 in
	  GSet.compare t t' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GenList(A))(GenList(A))
      let desc = "[Set] Intersection follows list find_all"
      let law (l1,l2) = 
	let t1 = add_list GSet.empty l1 in
	let t2 = add_list GSet.empty l2 in
	let t = GSet.inter t1 t2 in
	let ilst = List.find_all (fun x -> List.mem x l1) l2 in
	let t' = add_list GSet.empty ilst in
	  GSet.compare t t' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GSet
      let desc = "[Set] diff x x is empty"
      let law t = 
	let t' = GSet.diff t t in
	  GSet.is_empty t'
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GenList(A))(GenList(A))
      let desc = "[Set] diff follows list filter"
      let law (l1,l2) = 
	let t1 = add_list GSet.empty l1 in
	let t2 = add_list GSet.empty l2 in
	let t = GSet.diff t1 t2 in
	let dlist = List.filter (fun x -> not (List.mem x l2)) l1 in
	let t' = add_list GSet.empty dlist in
	  GSet.compare t t' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GSet)(GSet)
      let desc = "[Set] A intersected with B is B - (B - A)"
      let law (t1,t2) = 
	let t = GSet.inter t1 t2 in
	let t' = GSet.diff t2 (GSet.diff t2 t1) in
	  GSet.compare t t' = 0
    end) in (T.desc, T.test));

    (* Well formedness tests *)

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(A)(GSet)
      let desc = "[Set] GSet Well-Formed after add"
      let law (i,t) = 
	assert(GSet.well_formed t);
	GSet.well_formed (GSet.add i t) 
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(A)(GSet)
      let desc = "[Set] GSet Well-Formed after remove"
      let law (i,t) = 
	assert(GSet.well_formed t);
	GSet.well_formed (GSet.add i t)
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GSet)(GSet)
      let desc = "[Set] GSet Well-Formed after union"
      let law (t1,t2) = 
	assert(GSet.well_formed t1);
	assert(GSet.well_formed t2);
	GSet.well_formed (GSet.union t1 t2)
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GSet)(GSet)
      let desc = "[Set] GSet Well-Formed after diff"
      let law (t1,t2) = 
	assert(GSet.well_formed t1);
	assert(GSet.well_formed t2);
	GSet.well_formed (GSet.diff t1 t2)
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GSet)(GSet)
      let desc = "[Set] GSet Well-Formed after inter"
      let law (t1,t2) = 
	assert(GSet.well_formed t1);
	assert(GSet.well_formed t2);
	GSet.well_formed (GSet.inter t1 t2)
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GSet
      let desc = "[Set] Iter visits nodes in increasing order"
      let law t = 
	if GSet.is_empty t then true
	else 
	  let acc = ref (GSet.of_result (GSet.min_elt t)) in
	    GSet.iter 
	      (fun x -> 
		 if A.compare x !acc < 0 then failwith "Failed!"
		 else acc := x
	      ) t;
	    true
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GSet
      let desc = "[Set] fold visits nodes in increasing order"
      let law t = 
	if GSet.is_empty t then true
	else 
	  let min = GSet.of_result (GSet.min_elt t) in
	  let _ = 
	    GSet.fold
	      (fun acc x -> 
		 if A.compare x acc < 0 then failwith "Failed!"
		 else x
	      ) min t
	  in
	    true
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GSet
      let desc = "[Set] fold finds max_elt"
      let law t = 
	if GSet.is_empty t then raise Quickcheck.Trivial;
	let mk = GSet.fold
	  (fun acc k -> if A.compare acc k < 0 then k else acc)
	  (GSet.of_result (GSet.min_elt t)) t 
	in
	  A.compare mk (GSet.of_result (GSet.max_elt t)) = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GSet
      let desc = "[Set] fold finds min_elt"
      let law t = 
	if GSet.is_empty t then raise Quickcheck.Trivial;
	let mk = GSet.fold
	  (fun acc k -> if A.compare acc k > 0 then k else acc)
	  (GSet.of_result (GSet.max_elt t)) t 
	in
	  A.compare mk (GSet.of_result (GSet.min_elt t)) = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GSet)(A)
      let desc = "[Set] removing a non-existent element is no-op"
      let law (t,x) = 
	if GSet.of_result (GSet.mem x t) then raise Quickcheck.Trivial
	else GSet.compare t (GSet.remove x t) = 0
    end) in (T.desc, T.test));

  ]

  let unit_suite = CmpTests.unit_suite @ [
    ("min_elt empty raises Not_found" >:: fun () ->
       assert_raises ~msg:"(min_elt empty) should raise Not_found"
	 Not_found (fun () -> (GSet.min_elt GSet.empty)) 
    );

    ("max_elt empty raises Not_found" >:: fun () ->
       assert_raises ~msg:"(max_elt empty) should raise Not_found"
	 Not_found (fun () -> (GSet.max_elt GSet.empty)) 
    );

    ("choose empty raises Not_found" >:: fun () ->
       assert_raises ~msg:"(choose empty) should raise Not_found"
	 Not_found (fun () -> (GSet.choose GSet.empty)) 
    );

    ("the cardinal of empty is 0" >:: fun () ->
       assert_equal ~printer:string_of_int 0 (GSet.cardinal GSet.empty)
    );

    ("the cardinal of a singleton is 1" >:: fun () ->
       let rs = Random.State.make_self_init  () in
       let t = GSet.singleton (A.gen rs) in
	 assert_equal ~printer:string_of_int 1 (GSet.cardinal t)
    );

    ("move_up from the top raises Failure 'move up'" >:: fun () ->
       assert_raises ~msg:"move_up should raise Failure"
	 (Failure "move_up") 
	 (fun () -> (GSet.move_up (GSet.to_cursor GSet.empty)))
    );

    ("move_down_left raises Failure 'move_down_left'" >:: fun () ->
       assert_raises ~msg:"move_down_left should raise Failure"
	 (Failure "move_down_left") 
	 (fun () -> (GSet.move_down_left (GSet.to_cursor GSet.empty)))
    );

    ("move_down_right top raises Failure 'move down_right'" >:: fun () ->
       assert_raises ~msg:"move_down_right should raise Failure"
	 (Failure "move_down_right") 
	 (fun () -> (GSet.move_down_right (GSet.to_cursor GSet.empty)))
    );

    ("empty is well formed" >:: fun () ->
       assert_bool "empty should be well-formed" (GSet.well_formed GSet.empty)
    );

  ]

end
