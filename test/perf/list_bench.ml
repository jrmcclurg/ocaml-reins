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
open Bench
open Types

type ('elt,'list) dict = {
    empty : 'list;
    is_empty : 'list -> bool;
    length : 'list -> int;
    rev : 'list -> 'list;
    cons : 'elt -> 'list -> 'list;
    snoc : 'elt -> 'list -> 'list;
    hd : 'list -> 'elt;
    tl : 'list -> 'list;
    pop : 'list -> 'elt * 'list;
    append : 'list -> 'list -> 'list;
    (* flatten *)
    from_list : 'elt list -> 'list;
    to_list : 'list -> 'elt list;
    iter : ('elt -> unit) -> 'list -> unit;
    fold : 'a. ('a -> 'elt -> 'a) -> 'a -> 'list -> 'a;
(*
    rev_map : ('a -> 'b) -> 'a t -> 'b t;
    map : ('a -> 'b) -> 'a t -> 'b t;
*)
    to_string : ('elt -> string) -> 'list -> string;
    compare : ('elt -> 'elt -> int) -> 'list -> 'list -> int;
    gen : (?size:int -> Random.State.t -> 'elt) ->
	   ?size:int -> Random.State.t -> 'list;
  }

module ListDict(L : Lists.ListSig) = struct
  let dict = {
      empty = L.empty;
      is_empty = L.is_empty;
      length = L.length;
      rev = L.rev;
      cons = L.cons;
      snoc = L.snoc;
      hd = L.hd;
      tl = L.tl;
      pop = L.pop;
      append = L.append;
      from_list = L.from_list;
      to_list = L.to_list;
      iter = L.iter;
      fold = L.fold;
      to_string = L.to_string;
      compare = L.compare;
      gen = L.gen;
    }
end

(* A type for abstractly working with a lists.  Using a polymorphic
   record field allows the same 'f' to simultaneously apply to lists of
   arbitrary type.
*)
type ('elt, 'arg, 'res) polyf = {
    f : 'list. ('elt,'list) dict -> 'arg -> 'res
  } 

let modules_map f = [
    (let module D = ListDict(CatenableList) in "CatenableList", f.f D.dict);
    (let module D = ListDict(DoubleList.Make(SList)) in "DoubleList(SList)", f.f D.dict);
    (let module D = ListDict(DoubleQueue) in "DoubleQueue", f.f D.dict);
    (let module D = ListDict(SkewBinaryList) in "SkewBinaryList", f.f D.dict);
    (let module D = ListDict(SList) in "SList", f.f D.dict);
  ]

module SF = Mono.ComposeComparable(SList)(Mono.ComparablePair(String)(Float))

let bench_all polyf arg = 
  let flist = modules_map polyf in
  let times = List.map (fun (s,f) -> s, time f arg) flist in
    printf "%s\n" (SF.to_string times)

let cons_random () = 
  let f dict rs = 
    ignore(loop 100000 (fun l -> dict.cons (Int.gen rs) l) dict.empty); 
  in
  let rs = Random.State.make_self_init () in
    bench_all {f=f} rs

let snoc_random () = 
  let f dict rs = 
    ignore(loop 6000 (fun l -> dict.snoc (Int.gen rs) l) dict.empty); 
  in
  let rs = Random.State.make_self_init () in
    bench_all {f=f} rs

let append1_random () = 
  let f dict rs = 
    ignore(loop 6000
	      (fun l -> 
		let single = dict.cons (Int.gen rs) dict.empty in
		  dict.append l single
	      ) dict.empty); 
  in
  let rs = Random.State.make_self_init () in
    bench_all {f=f} rs

let prepend1_random () = 
  let f dict rs = 
    ignore(loop 10000
	      (fun l -> 
		let single = dict.cons (Int.gen rs) dict.empty in
		  dict.append single l
	      ) dict.empty); 
  in
  let rs = Random.State.make_self_init () in
    bench_all {f=f} rs


let run () = 
  printf "cons: \n%!";
  cons_random ();
  printf "snoc: \n%!";
  snoc_random ();
  printf "append1: \n%!";
  append1_random ();
  printf "prepend1: \n%!";
  prepend1_random ();
  ()

