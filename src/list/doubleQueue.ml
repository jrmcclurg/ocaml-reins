(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

type 'a t = 'a list * 'a list

let empty = [], []

let is_empty (f,r) = match f with [] -> true | _ -> false

(* smart construct to enforce that the first list is only empty
   when the entire queue is empty *)
let dqueue f r = match f with
  | [] -> (List.rev r), []
  | _ -> f,r

let cons x (f,r) = x::f, r

let snoc x (f,r) = dqueue f (x::r)
let cons_back = snoc

let enqueue = snoc

let hd = function
  | [],_ -> failwith "hd"
  | x::_,_ -> x

let tl = function
  | [],_ -> failwith "tl"
  | x::tl,r -> dqueue tl r

let pop = function
  | [],_ -> failwith "pop"
  | x::tl,r -> x, (dqueue tl r)

let dequeue = pop

let last = function
  | _, r::_ -> r
  | [],[] -> failwith "last"
  | hd::[], [] -> hd
  | f::fs, [] -> List.hd (List.rev fs)

let hd_back t = 
  try last  t
  with Failure(s) when s="last" -> failwith "hd_back"

let tl_back = function
  | f, (r::rs) -> (f,rs)
  | [], [] -> failwith "tl_back"
  | hd::[], [] -> empty
  | f::fs, [] ->
      match List.rev fs with
        | [] -> assert false
        | r::rs -> [f], rs

let pop_back = function
  | f, (r::rs) -> (f,rs), r
  | [], [] -> failwith "pop_back"
  | hd::[], [] -> empty, hd
  | f::fs, [] ->
      match List.rev fs with
        | [] -> assert false
        | r::rs -> ([f], rs), r


let length (f,r) = (List.length f) + (List.length r)

let append (f1,r1) (f2,r2) = 
  let r = List.rev_append f2 r1 in
  let r = List.append r2 r in
    f1, r

let rev (f,r) = dqueue r f

let iter func (f,r) = 
  List.iter func f;
  List.iter func (List.rev r)

let fold func acc (f,r) =
  List.fold_left func (List.fold_left func acc f) (List.rev r)

let rev_map func l = fold (fun acc x -> cons (func x) acc) empty l
let map     func l = fold (fun acc x -> snoc (func x) acc) empty l

let to_list (f,r) = 
  List.rev_append (List.rev f) (List.rev r)

let from_list l = (l,[])

(* This is probably not the fastest implementation due to the
   intermediate list reversals, however its at least O(n).  Feel free
   to submit patches with a faster version if you actually use this
   function.
*)
let flatten (f,r) = 
  let f' = List.rev (List.fold_left (fold (fun acc x -> x::acc)) [] f) in
  let r' = List.rev (List.fold_left (fold (fun acc x -> x::acc)) [] r) in
    (f',r')

let compare c ((f1,r1) as l1) ((f2,r2) as l2) = match r1,r2 with
  | [],[] -> SList.compare c f1 f2
  | _ -> SList.compare c (to_list l1) (to_list l2)

let to_string to_s l = ListCommon.to_string iter pop to_s l

let gen (gena: ?size:int -> Random.State.t -> 'a) ?size rs : 'a t = 
  (SList.gen ?size gena rs), (SList.gen ?size gena rs)


