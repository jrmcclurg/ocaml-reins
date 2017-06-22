(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

module type S =
sig
  type 'a list_
  type 'a cursor

  val to_cursor : 'a list_ -> 'a cursor
  val from_cursor : 'a cursor -> 'a list_
  val at_front : 'a cursor -> bool
  val at_back : 'a cursor -> bool
  val move_next : 'a cursor -> 'a cursor
  val move_prev : 'a cursor -> 'a cursor
  val goto_front : 'a cursor -> 'a cursor
  val goto_back : 'a cursor -> 'a cursor

  val value : 'a cursor -> 'a option

  val list : 'a cursor -> 'a list_
  val replace_list : 'a list_ -> 'a cursor -> 'a cursor
end

module Make(L : Lists.ListSig) : S with type 'a list_ = 'a L.t = struct
  type 'a list_ = 'a L.t

  (* Note that this type is same as the standard list type with the
     arguments of the 2nd constructor reversed. *)
  type 'a path = 
      | Top
      | Path of 'a path * 'a

  type 'a cursor = 'a path * 'a L.t

  let to_cursor t = Top, t

  let at_front = function Top,_ -> true | _ -> false

  let at_back (p,t) = L.is_empty t

  let value (_,t) = if L.is_empty t then None else Some (L.hd t)

  let list (_,t) = t
  let replace_list t (p,_) = (p,t)

  let move_next (p,t) = 
    if L.is_empty t then failwith "move_next"
    else
      let x,xs = L.pop t in
	Path(p,x), xs
	  
  let move_prev (p,t) = match p with
    | Top -> failwith "move_prev"
    | Path(p, x) -> 
	p, (L.cons x t)
	  
  let rec goto_front c = 
    if at_front c then c
    else goto_front (move_prev c)

  let rec goto_back c = 
    if at_back c then c
    else goto_back (move_next c)

  let rec from_cursor = function
    | Top,t -> t
    | c -> from_cursor (move_prev c)
	
end
