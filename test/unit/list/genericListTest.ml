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

module Make(L : Lists.ListSig)(A : Mono.ArbitraryComparable) = struct
  module GenL = Mono.ComposeGenComparable(L)(A)

  module CmpTests = GenericTest.ComparableTests(GenL)

  let random_suite = CmpTests.random_suite @ [
    (let module T = RandCheck(struct
      module Arg = A
      let desc = "<List>.rev [x] = [x]"
      let law i = 
	let t = L.cons i L.empty in
	  GenL.compare (L.rev t) t = 0
    end) in (T.desc, T.test));
    
    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(GenL)(GenL)
      let desc = "<List>.rev (x@y) = (<List>.rev y) @ (<List>.rev x)"
      let law (l1,l2) = 
	let l1' = L.rev (L.append l1 l2) in
	let l2' = L.append (L.rev l2) (L.rev l1) in
	  GenL.compare l1' l2' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GenL
      let desc = "<List>.rev x = <List>.rev (<List>.rev x)"
      let law l1 = 
	let l2 = L.rev (L.rev l1) in
	  GenL.compare l1 l2 = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(A)(GenL)
      let desc = "hd (cons x t) is x"
      let law (x,l) = 
	A.compare (L.hd (L.cons x l)) x = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GenList(A)
      let desc = "Length is n after n cons"
      let law il = 
	let dl = List.fold_left (fun acc x -> L.cons x acc) L.empty il in
	(List.length il) = (L.length dl)
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(A)(GenL)
      let desc = "tail of (cons x l) is l"
      let law (i,l) = 
	let l' = L.tl (L.cons i l) in
	  (GenL.compare l l') = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GenL
      let desc = "from_list (to_list x) is x"
      let law l =
	GenL.compare l (L.from_list (L.to_list l)) = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module AL = GenList(A)
      module Arg = GenList(A)
      let desc = "to_list (from_list x) is x"
      let law l =
	AL.compare l (L.to_list (L.from_list l)) = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module  Arg = GenList(GenL)
      let desc = "flatten mirrors List.flatten"
      let law l = 
	let t = L.from_list l in
	let lst1 = L.to_list (L.flatten t) in
	let t' = List.map L.to_list l in
	let lst2 = List.flatten t' in
	let module ML = GenList(A) in
	  (ML.compare lst1 lst2) = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(A)(GenL)
      let desc = "pop of (cons x l) is x,l"
      let law (i,l) = 
	let x,tl = L.pop (L.cons i l) in
	  (A.compare x i) = 0 && (GenL.compare tl l) = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = Mono.GenPair(A)(GenL)
      let desc = "last (snoc x t) is x"
      let law (x,l) = 
	let x' = L.last (L.snoc x l) in
	  A.compare x x' = 0
    end) in (T.desc, T.test));
    
    (let module T = RandCheck(struct
      module Arg = GenList(A)
      let desc = "to_list preserves order"
      let law l = 
	let lr = List.rev l in
	let t = List.fold_left (fun acc x -> L.cons x acc) L.empty lr in
	let l' = L.to_list t in
	let module ML = GenList(A) in
	  ML.compare l l' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GenL
      let desc = "(map id l) is same as l"
      let law l = 
	let l' = L.map (fun x -> x) l in
	  L.compare A.compare l l' = 0
    end) in (T.desc, T.test));

    (let module T = RandCheck(struct
      module Arg = GenL
      let desc = "l is same as (rev (rev_map id l))"
      let law l = 
	let l' = L.rev (L.rev_map (fun x -> x) l) in
	  L.compare A.compare l l' = 0
    end) in (T.desc, T.test));

  ]

let assert_equal_int x y = 
  assert_equal ~cmp:(=) ~printer:string_of_int
      ~msg:"ints not equal" x y
      
  let unit_suite = CmpTests.unit_suite @ [
    
    ("is_empty empty" >:: fun () ->
      assert_equal true (L.is_empty L.empty));
    
    ("empty,ins,del,is_empty" >:: fun () ->
      let dl = L.cons 10 L.empty in
      let dl = L.tl dl in
	assert_equal true (L.is_empty dl));

    ("hd of empty raises Failure 'hd'" >:: fun () ->
       (try
	  ignore(L.hd L.empty); 
	  assert_failure "hd of empty should raise an exception"
	with
	  | Failure(s) when s="hd" -> ()
	  | _ -> assert_failure "(hd empty) raised the wrong exception"));

    ("tl of empty raises Failure 'tl'" >:: fun () ->
       (try
	  ignore(L.tl L.empty); 
	  assert_failure "tl of empty should raise an exception"
	with
	  | Failure(s) when s="tl" -> ()
	  | _ -> assert_failure "(tl empty) raised the wrong exception"));

    ("pop of empty raises Failure 'pop'" >:: fun () ->
       (try
	  ignore(L.pop L.empty); 
	  assert_failure "pop of empty should raise an exception"
	with
	  | Failure(s) when s="pop" -> ()
	  | _ -> assert_failure "(pop empty) raised the wrong exception"));

    ("last of empty raises Failure 'last'" >:: fun () ->
       (try
	  ignore(L.last L.empty); 
	  assert_failure "last of empty should raise an exception"
	with
	  | Failure(s) when s="last" -> ()
	  | _ -> assert_failure "(last empty) raised the wrong exception"));

    ("map of (+1) on [1..5] is [2..6]" >:: fun () ->
      let l = L.from_list [1;2;3;4;5] in
      let l' = L.map ((+) 1) l in
      let lr = L.from_list [2;3;4;5;6] in
	assert_equal ~cmp:(fun x y -> L.compare Int.compare x y = 0)
	  ~printer:(L.to_string Int.to_string) l' lr
    );

    ("rev_map of (+1) on [1..5] is [6..2]" >:: fun () ->
      let l = L.from_list [1;2;3;4;5] in
      let l' = L.rev_map ((+) 1) l in
      let lr = L.from_list [6;5;4;3;2] in
	assert_equal ~cmp:(fun x y -> L.compare Int.compare x y = 0)  
	  ~printer:(L.to_string Int.to_string) l' lr
    );

    ("to_string of [1;2;3;4;5] is \"[1;2;3;4;5]\"" >:: fun () ->
      let l = L.from_list [1;2;3;4;5] in
      let s = L.to_string string_of_int l in
      let module ML = GenList(Int) in
	assert_equal ~cmp:(fun x y -> String.compare x y = 0)
	  ~printer:(fun x -> x)
	  s "[1; 2; 3; 4; 5]"
    );

    ("to_string of [] is \"[]\"" >:: fun () ->
      let l = L.from_list [] in
      let s = L.to_string string_of_int l in
      let module ML = GenList(Int) in
	assert_equal ~cmp:(fun x y -> String.compare x y = 0)
	  ~printer:(fun x -> x)
	  s "[]"
    );

    ]

end

