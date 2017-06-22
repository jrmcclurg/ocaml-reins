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

module RandomMapTests
  (A : Mono.ArbitraryComparable)
  (GMap : Maps.GenMapSig with type key = A.t and type elt = A.t)
  = struct

  module CmpTests = GenericTest.ComparableTests(GMap)
  module KV = Mono.ComparablePair(A)(A)
      
  let add_list t l = List.fold_left (fun t e -> GMap.add e e t) t l
  let join_max k v1 v2 = max v1 v2
  let diff_true _ _ _ = true

  let random_suite = CmpTests.random_suite @ [
    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(A)(GenList(A))
      let desc = "[Map] Add first element"
      let law (i,lst) = 
	let t = GMap.add i i GMap.empty in
	let t = add_list t lst in
	  GMap.of_result (GMap.mem i t)
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg =  Mono.Gen3Tuple(GMap)(A)(GenList(A))
      let desc = "[Map] Add middle element"
      let law (t,i,lst) = 
	let t = GMap.add i i t in
	let t = add_list t lst in
	  GMap.of_result (GMap.mem i t)
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg =  Mono.GenPair(A)(GMap)
      let desc = "[Map] Add last element"
      let law (i,t) = 
	GMap.of_result (GMap.mem i (GMap.add i i t))
    end) in (T.desc, T.test));
    
    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(A)(GenList(A))
      let desc = "[Map] Remove first element"
      let law (i,lst) = 
	let t = GMap.add i i GMap.empty in
	let t = add_list t lst in
	let t = GMap.remove i t in
	  not (GMap.of_result (GMap.mem i t))
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.Gen3Tuple(GMap)(A)(GenList(A))
      let desc = "[Map] Remove middle element"
      let law (t,i,lst) = 
	let t = GMap.add i i t in
	let t = add_list t lst in
	let t = GMap.remove i t in
	  not (GMap.of_result (GMap.mem i t))
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GMap)(A)
      let desc = "[Map] Remove last element"
      let law (t,i) = 
	let t = GMap.add i i t in
	let t = GMap.remove i t in
	  not (GMap.of_result (GMap.mem i t))
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GMap)(GMap)
      let desc = "[Map] Union is commutative"
      let law (t1,t2) = 
	(* Note: the join function must also be commutative *)
	let t = GMap.union join_max t1 t2 in
	let t' = GMap.union join_max t2 t1 in
	  GMap.compare t t' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GenList(A))(GenList(A))
      let desc = "[Map] Union follows list concatenation"
      let law (l1,l2) = 
	let t1 = add_list GMap.empty l1 in
	let t2 = add_list GMap.empty l2 in
	let t = GMap.union join_max t1 t2 in
	let t' = add_list GMap.empty (l1 @ l2) in
	  GMap.compare t t' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GMap)(GMap)
      let desc = "[Map] Intersection is commutative"
      let law (t1,t2) = 
	let t = GMap.inter join_max t1 t2 in
	let t' = GMap.inter join_max t2 t1 in
	  GMap.compare t t' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GenList(A))(GenList(A))
      let desc = "[Map] Intersection follows list find_all"
      let law (l1,l2) = 
	let t1 = add_list GMap.empty l1 in
	let t2 = add_list GMap.empty l2 in
	let t = GMap.inter join_max t1 t2 in
	let ilst = List.find_all (fun x -> List.mem x l1) l2 in
	let t' = add_list GMap.empty ilst in
	  GMap.compare t t' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GMap
      let desc = "[Map] diff x x is empty"
      let law t = 
	let t' = GMap.diff diff_true t t in
	  GMap.is_empty t'
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GenList(A))(GenList(A))
      let desc = "[Map] diff follows list filter"
      let law (l1,l2) = 
	let t1 = add_list GMap.empty l1 in
	let t2 = add_list GMap.empty l2 in
	let t = GMap.diff diff_true t1 t2 in
	let dlist = List.filter (fun x -> not (List.mem x l2)) l1 in
	let t' = add_list GMap.empty dlist in
	  GMap.compare t t' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GMap)(GMap)
      let desc = "[Map] A intersected with B has same keys as B - (B - A)"
      let law (t1,t2) = 
	let t = GMap.inter join_max t1 t2 in
	let t' = GMap.diff diff_true t2 (GMap.diff diff_true t2 t1) in
	  GMap.compare_keys t t' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GMap
      let desc = "[Map] map id produces equivalent Map"
      let law t = 
	let t' = GMap.map (fun x -> x) t in
	  (GMap.compare t t') = 0
    end) in (T.desc, T.test));

    (* Well formedness tests *)

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(A)(GMap)
      let desc = "[Map] GMap Well-Formed after add"
      let law (i,t) = 
	assert(GMap.well_formed t);
	GMap.well_formed (GMap.add i i t)
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(A)(GMap)
      let desc = "[Map] GMap Well-Formed after remove"
      let law (i,t) = 
	assert(GMap.well_formed t);
	GMap.well_formed (GMap.add i i t)
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GMap)(GMap)
      let desc = "[Map] GMap Well-Formed after union"
      let law (t1,t2) = 
	assert(GMap.well_formed t1);
	assert(GMap.well_formed t2);
	GMap.well_formed (GMap.union join_max t1 t2)
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GMap)(GMap)
      let desc = "[Map] GMap Well-Formed after diff"
      let law (t1,t2) = 
	assert(GMap.well_formed t1);
	assert(GMap.well_formed t2);
	GMap.well_formed (GMap.diff diff_true t1 t2)
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GMap)(GMap)
      let desc = "[Map] GMap Well-Formed after inter"
      let law (t1,t2) = 
	assert(GMap.well_formed t1);
	assert(GMap.well_formed t2);
	GMap.well_formed (GMap.inter join_max t1 t2)
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GMap
      let desc = "[Map] Iter visits nodes in increasing order"
      let law t = 
	if GMap.is_empty t then true
	else 
	  let acc = ref (GMap.of_result (GMap.min_key t)) in
	    GMap.iter 
	      (fun x _ -> 
		 if A.compare x !acc < 0 then failwith "Failed!"
		 else acc := x
	      ) t;
	    true
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GMap
      let desc = "[Map] fold visits nodes in increasing order"
      let law t = 
	if GMap.is_empty t then true
	else 
	  let min = GMap.of_result (GMap.min_key t) in
	  let _ = 
	    GMap.fold
	      (fun acc x _ -> 
		 if A.compare x acc < 0 then failwith "Failed!"
		 else x
	      ) min t
	  in
	    true
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GMap
      let desc = "[Map] fold (+1) equals the cardinality"
      let law t = 
	let c = GMap.fold (fun acc _ _ -> acc+1) 0 t in
	  (GMap.cardinal t) = c
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GMap
      let desc = "[Map] fold finds max_key"
      let law t = 
	if GMap.is_empty t then raise Quickcheck.Trivial;
	let mk = GMap.fold
	  (fun acc k _ -> if A.compare acc k < 0 then k else acc)
	  (GMap.of_result (GMap.min_key t)) t 
	in
	  A.compare mk (GMap.of_result (GMap.max_key t)) = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GMap
      let desc = "[Map] fold finds min_key"
      let law t = 
	if GMap.is_empty t then raise Quickcheck.Trivial;
	let mk = GMap.fold
	  (fun acc k _ -> if A.compare acc k > 0 then k else acc)
	  (GMap.of_result (GMap.max_key t)) t 
	in
	  A.compare mk (GMap.of_result (GMap.min_key t)) = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GMap
      let desc = "[Map] fold finds max_keyval"
      let law t = 
	if GMap.is_empty t then raise Quickcheck.Trivial;
	let mkv = GMap.fold
	  (fun (kacc,vacc) k v -> if A.compare kacc k < 0 then (k,v) else (kacc,vacc))
	  (GMap.of_result (GMap.min_keyval t)) t 
	in
	  KV.compare mkv (GMap.of_result (GMap.max_keyval t)) = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GMap
      let desc = "[Map] fold finds min_keyval"
      let law t = 
	if GMap.is_empty t then raise Quickcheck.Trivial;
	let mkv = GMap.fold
	  (fun (kacc,vacc) k v -> if A.compare kacc k > 0 then (k,v) else (kacc,vacc))
	  (GMap.of_result (GMap.max_keyval t)) t 
	in
	  KV.compare mkv (GMap.of_result (GMap.min_keyval t)) = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GMap)(A)
      let desc = "[Map] removing a non-existent element is no-op"
      let law (t,x) = 
	if GMap.of_result (GMap.mem x t) then raise Quickcheck.Trivial
	else GMap.compare t (GMap.remove x t) = 0
    end) in (T.desc, T.test));
    
  ]

  let unit_suite = CmpTests.unit_suite @ [
    ("min_key empty raises Not_found" >:: fun () ->
       assert_raises ~msg:"(min_key empty) should raise Not_found"
	 Not_found (fun () -> (GMap.min_key GMap.empty)) 
    );

    ("max_key empty raises Not_found" >:: fun () ->
       assert_raises ~msg:"(max_key empty) should raise Not_found"
	 Not_found (fun () -> (GMap.max_key GMap.empty)) 
    );

    ("min_keyval empty raises Not_found" >:: fun () ->
       assert_raises ~msg:"(min_keyval empty) should raise Not_found"
	 Not_found (fun () -> (GMap.min_keyval GMap.empty)) 
    );

    ("max_keyval empty raises Not_found" >:: fun () ->
       assert_raises ~msg:"(max_keyval empty) should raise Not_found"
	 Not_found (fun () -> (GMap.max_keyval GMap.empty)) 
    );

    ("the cardinal of empty is 0" >:: fun () ->
       assert_equal ~printer:string_of_int 0 (GMap.cardinal GMap.empty)
    );

    ("the cardinal of a singleton is 1" >:: fun () ->
       let rs = Random.State.make_self_init  () in
       let t = GMap.singleton (A.gen rs) (A.gen rs) in
	 assert_equal ~printer:string_of_int 1 (GMap.cardinal t)
    );

    ("move_up from the top raises Failure 'move up'" >:: fun () ->
       assert_raises ~msg:"move_up should raise Failure"
	 (Failure "move_up") 
	 (fun () -> (GMap.move_up (GMap.to_cursor GMap.empty)))
    );

    ("move_down_left raises Failure 'move_down_left'" >:: fun () ->
       assert_raises ~msg:"move_down_left should raise Failure"
	 (Failure "move_down_left") 
	 (fun () -> (GMap.move_down_left (GMap.to_cursor GMap.empty)))
    );

    ("move_down_right top raises Failure 'move down_right'" >:: fun () ->
       assert_raises ~msg:"move_down_right should raise Failure"
	 (Failure "move_down_right") 
	 (fun () -> (GMap.move_down_right (GMap.to_cursor GMap.empty)))
    );

    ("empty is well formed" >:: fun () ->
       assert_bool "empty should be well-formed" (GMap.well_formed GMap.empty)
    );

  ]


end

