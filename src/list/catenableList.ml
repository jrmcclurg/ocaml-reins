(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)


(** Lists with fast concatenation.  Based on Okasaki's implementation *)

type 'a t = 
    | Empty
    | Cat of 'a * 'a t Lazy.t DoubleQueue.t

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let link (t: 'a t) (s: 'a t Lazy.t) = match t with
  | Empty -> assert false
  | Cat(x,q) -> Cat(x, DoubleQueue.enqueue s q)

let rec link_all q =
  let t = Lazy.force (DoubleQueue.hd q) in
  let q' = DoubleQueue.tl q in
    if DoubleQueue.is_empty q' then t
    else link t (lazy (link_all q'))

let append t1 t2 = match t1,t2 with
  | Empty, t | t, Empty -> t
  | _ -> link t1 (lazy t2)

let cons x xs = append (Cat(x,DoubleQueue.empty)) xs

let snoc x xs = append xs (Cat(x,DoubleQueue.empty))

let rec last = function
  | Empty -> failwith "last"
  | Cat(x,q) -> 
      if DoubleQueue.is_empty q then x
      else 
	let t' = Lazy.force (DoubleQueue.last q) in
	  last t'

let hd = function
  | Empty -> failwith "hd"
  | Cat(x,_) -> x

let pop = function
  | Empty -> failwith "pop"
  | Cat(x,q) -> x, (if DoubleQueue.is_empty q then Empty else link_all q)

let tl = function
  | Empty -> failwith "tl"
  | Cat(x,q) -> if DoubleQueue.is_empty q then Empty else link_all q

let rec iter f = function
  | Empty -> ()
  | t ->
      let hd,tl = pop t in
	f hd; iter f tl

let rec fold_left f acc = function
  | Empty -> acc
  | t ->
      let hd,tl = pop t in
	fold_left f (f acc hd) tl

(*
  let rec fold_right f t acc = match t with
  | Empty -> acc
  | Cat(x,q) -> 
  if DoubleQueue.is_empty q
  then f x acc
  else let rest,last = DoubleQueue.pop_back q in
  fold_right f rest (f acc last) 
*)
let fold = fold_left

let rev t = fold (fun acc x -> cons x acc) empty t

let rev_map f t = 
  let rec helper acc = function
    | Empty -> acc
    | t -> let hd,tl = pop t in helper (cons (f hd) acc) tl
  in helper Empty t

let map f t = 
  let rec helper acc = function
    | Empty -> acc
    | t -> let hd,tl = pop t in helper (snoc (f hd) acc) tl
  in helper Empty t

let length t = fold (fun acc _ -> acc+1) 0 t

let to_list t = List.rev (fold (fun acc x -> x::acc) [] t)

let from_list lst = List.fold_left (fun acc x -> snoc x acc) Empty lst

let rec flatten t = 
  let rec helper acc t = 
    if is_empty t then acc
    else let x,xs = pop t in
	   helper (append acc x) xs
  in helper Empty t

let to_string to_s t = ListCommon.to_string iter pop to_s t

let rec compare cmp t1 t2 = match t1,t2 with
  | Empty, Empty -> 0
  | Empty, t -> -1
  | t, Empty -> 1
  | Cat(x,q1),Cat(y,q2) -> match cmp x y with
      | 0 -> compare cmp (tl t1) (tl t2)
      | v -> v
	  
let rec gen (agen : ?size:int -> Random.State.t -> 'a) ?(size=50) rs : 'a t =
  let rec helper acc s =  
    let s = max s 1 in
    let i = Random.State.int rs s in
      if i <= 1 then acc
      else helper (cons (agen ~size:size rs) acc) (s-1)
  in helper Empty size

