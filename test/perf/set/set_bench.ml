(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Printf
open Bench

open Reins.Types
(*
(** This is a first class representation of a tree which includes all
    of a tree's operations packed into a record.  This can be useful
    when treating trees as first class objects, such as to benchmark
    them *)
type ('set,'elt,'bool_result,'elt_result,'cursor) treeSetDict = {
  empty : 'set;
  is_empty : 'set -> bool;
  mem : 'elt -> 'set -> 'bool_result;
  add : 'elt -> 'set -> 'set;
  singleton : 'elt -> 'set;
  remove : 'elt -> 'set -> 'set;
  well_formed : 'set -> bool;
  compare : 'set -> 'set -> int;
  equal : 'set -> 'set -> bool;
  iter : ('elt -> unit) -> 'set -> unit;
  fold : 'a. ('a -> 'elt  -> 'a) -> 'a -> 'set -> 'a;
  min_elt : 'set -> 'elt_result;
  max_elt : 'set -> 'elt_result;
  choose : 'set -> 'elt_result;
  cardinal : 'set -> int;
  union : 'set -> 'set -> 'set;
  inter : 'set -> 'set -> 'set;
  diff : 'set -> 'set -> 'set;
  to_cursor : 'set -> 'cursor;
  from_cursor : 'cursor -> 'set;
  at_top : 'cursor -> bool;
  at_left : 'cursor -> bool;
  at_right : 'cursor -> bool;
  move_up : 'cursor -> 'cursor;
  move_down_left : 'cursor -> 'cursor;
  move_down_right : 'cursor -> 'cursor;
  went_left : 'cursor -> bool;
  went_right : 'cursor -> bool;
  has_value : 'cursor -> bool;
  get_value : 'cursor -> 'elt;
  of_result : 'bool_result -> bool;
  elt_of_elt_result : 'elt_result -> 'elt;
  gen1 : (?size:int -> Random.State.t -> 'elt) -> 
    ?size:int -> Random.State.t -> 'set
(*
  for_all : ('elt -> bool) -> 'set -> bool
  exists : ('elt -> bool) -> 'set -> bool
  elements : 'set -> 'elt list
  subset : 'set -> 'set -> bool
  filter : ('elt -> bool) -> 'set -> 'set
  partition : ('elt -> bool) -> 'set -> 'set * 'set
  split : 'elt -> 'set -> 'set * bool * 'set
  add_at : 'elt -> cursor -> cursor
  mem_at : 'elt -> cursor -> bool
  remove_at : 'elt -> cursor -> cursor
*)
}
    

module MonoTreeSetToDict(Set : Sets.MonoSet) = struct

  let dict = {
      empty = Set.empty;
    is_empty = Set.is_empty;
    mem = Set.mem;
    add = Set.add;
    singleton = Set.singleton;
    remove = Set.remove;
    well_formed = Set.well_formed;
    compare = Set.compare;
    equal = Set.equal;
    iter = Set.iter;
    fold = Set.fold;
    min_elt = Set.min_elt;
    max_elt = Set.max_elt;
    choose = Set.choose;
    cardinal = Set.cardinal;
    union = Set.union;
    inter = Set.inter;
    diff = Set.diff;
    to_cursor = Set.to_cursor;
    from_cursor = Set.from_cursor;
    at_top = Set.at_top;
    at_left = Set.at_left;
    at_right = Set.at_right;
    move_up = Set.move_up;
    move_down_left = Set.move_down_left;
    move_down_right = Set.move_down_right;
    went_left = Set.went_left;
    went_right = Set.went_right;
    has_value = Set.has_value;
    get_value = Set.get_value;
    of_result = Set.of_result;
    gen1 = Set.gen1;
  }
end
    


module INRIA_Set (C : MonoComparable) = struct
  include Set.Make(C)
  type 'a set = t
  type 'a elt_ = elt

  type elt_result = elt
  type 'a elt_result_ = elt_result

  type bool_result = bool
  type 'a bool_result_ = bool_result


  let fold f acc t = fold (fun x y -> f y x) t acc

  let elt_of_elt_result x = x
  let bool_of_bool_result x = x

  let well_formed t = true
  let to_string t = 
    "[" ^ (fold (fun acc x -> acc ^ ", " ^ (C.to_string x)) "" t) ^ "]"

  let gen1 (agen : (?size:int -> Random.State.t -> elt)) ?(size=50) rs = 
    let num = Random.State.int rs size in
    let rec loop n t = 
      if n <= 0 then t
      else loop (n-1) (add (agen ~size:size rs) t)
    in
      loop num empty

  type cursor
  type 'a cursor_ = cursor
  let get_value _ = assert false
  let has_value _ = assert false
  let went_right _ = assert false
  let went_left _ = assert false
  let move_down_right _ = assert false
  let move_down_left _ = assert false
  let move_up _ = assert false
  let at_right _ = assert false
  let at_left _ = assert false
  let at_top _ = assert false
  let from_cursor _ = assert false
  let to_cursor _ = assert false

end


(*
  A type for abstractly working with a sets.  Using a polymorphic
  record field allows the same 'f' to simultaneously apply to sets of
  arbitrary type. 
*)
type ('elt, 'arg,'res) polyf = {
  f : 'set 'br 'er 'cur. ('set,'elt, 'br,'er,'cur) treeSetDict -> 'arg -> 'res
} 

let modules_map f = [
  (let module D = MonoTreeSetToDict(AVL.Set1(Int)) in f.f D.dict);
  (let module D = MonoTreeSetToDict(AVL.Set2(Int)) in f.f D.dict);
  (let module D = MonoTreeSetToDict(AVL.Set3(Int)) in f.f D.dict);
  (let module D = MonoTreeSetToDict(Patricia.Set) in f.f D.dict);
  (let module D = MonoTreeSetToDict(RedBlack.Set(Int)) in f.f D.dict);
  (let module D = MonoTreeSetToDict(Splay.Set(Int)) in f.f D.dict);
  (let module D = MonoTreeSetToDict(INRIA_Set(Int)) in f.f D.dict);
]

let time_f f arg =
  let prev = Unix.gettimeofday () in
  let _ = f arg in
  let aft = Unix.gettimeofday () in
    aft -. prev

let average n f = 
  let rec loop n acc = 
    if n <= 0 then acc 
    else
      let acc = List.map2 (+.) acc (f ()) in
	loop (n-1) acc
  in
  let lst = loop (n-1) (f()) in
    List.map (fun x -> x /. (float n)) lst
    
let time_rand_union n oc = 
  let rs = Random.State.make_self_init () in
  let f d x = 
    time_f (d.union (d.gen1 ~size:n Int.gen rs)) (d.gen1 ~size:n Int.gen rs) in
  let all_bench = modules_map {f=f} in

  let bench () = List.map (fun x -> x ()) all_bench in
  let results = average 20 bench in
    List.iter (fprintf oc "%f ") results

let time_rand_insert n oc = 
  let f d i = ignore(List.fold_left (fun acc x -> d.add x acc) d.empty i) in
  let all_bench = modules_map {f=f} in
    
  let bench () =
    let input = random_int_list n in
      List.map (fun x -> time_f x input) all_bench
  in
  let results = average 15 bench in
    List.iter (fprintf oc "%f ") results

(*
let _ = 
  let oc = open_out "data.1" in
    for i = 1 to 100 do
      eprintf "at %d\n%!" i;
      let size = i * 50 in
	fprintf oc "%d " size;
	time_rand_union size oc;
	fprintf oc "\n%!"
    done;
    close_out oc
    
*)
*)
