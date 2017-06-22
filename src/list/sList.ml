(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

include List
type 'a t = 'a list

let empty = []
let is_empty = function [] -> true | _ -> false

let pop = function 
  | [] -> failwith "pop"
  | hd::tl -> hd,tl

let fold = fold_left
let cons x t = x::t
let snoc x t = rev (x::(rev t))

let rec last = function
  | [] -> failwith "last"
  | x::[] -> x
  | _::xs -> last xs

let to_list x = x
let from_list x = x

let rec compare cmp x y = match x,y with
  | [],[] -> 0
  | _::_, [] ->  1
  | [], _::_ -> -1
  | hx::xs, hy::ys -> match cmp hx hy with
      | 0 -> compare cmp xs ys
      | c -> c

let rec gen (f : ?size:int -> Random.State.t -> 'a) 
    ?(size=100) (r : Random.State.t) : 'a list = 
  let size = abs size in
    if (Random.State.int r size) = 0
    then []
    else (f r) :: (gen ~size:(size-1) f r)

let to_string to_s t = ListCommon.to_string iter pop to_s t

let fold = fold_left
(*let equal x y = (compare Pervasives.compare x y) = 0*)

