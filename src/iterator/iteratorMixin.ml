(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)


module type PartIterator = sig
  type 'a elt
  type 'a cursor
  type direction
      
  val has_curs_value : 'a cursor -> bool
  val get_curs_value : 'a cursor -> 'a elt
  val has_more_elements : direction -> 'a cursor -> bool
  val move_cursor_next_element : direction -> 'a cursor -> 'a cursor
  val flip_dir : direction -> direction

end

module Mixin(IT : PartIterator) (* CR SW: add result signature *) = struct

  type 'a traversal =
      | Traverse_All
      | Traverse_If of ('a -> bool)
      | Traverse_While of ('a -> bool)

  type 'a t = {
      curs : 'a IT.cursor;
      dir : IT.direction;
      trav : 'a IT.elt traversal;
      next : 'a t option Lazy.t;
      prev : 'a t option Lazy.t;
    }

  (* This is the main work horse of the module.  It serves to
     simultaneously check if the iterator has reached the end (returns
     [None]) and move the iterator to the next element (retruns [Some
     it]).  This is to minimize duplicated work between calls to
     [at_end] and [next] (and [at_beg]/[prev]).  This is also the
     thunk that is stored in the [next] and [prev] fields of the
     iterator record, so it must not (recursively) force those
     values.  *)
  let rec goto_next it = 
    (* Check if the underlying collection has more elements *)
    if not (IT.has_more_elements it.dir it.curs) then None
    else match it.trav with
      | Traverse_All -> Some (move_one it) 
      | Traverse_If f ->
	  (* We are at the end only if f returns false for every
	     element in the remainder of the collection. *)
	  let it' = move_one it in
	    if f (IT.get_curs_value it'.curs)
	    then Some it' (* found an element where f is true *)
	    else goto_next it' (* check the next element *)
      | Traverse_While f ->
	  (* We are at the end as soon as the condition returns false
	     on any element.  We don't have to scan to the end of the
	     list in this case. *)
	  if not (IT.has_curs_value it.curs)
	  then goto_next (move_one it)
	  else if f (IT.get_curs_value it.curs)
	  then Some (move_one it)
	  else None

  and move_one it = 
    let curs' = IT.move_cursor_next_element it.dir it.curs in
      set_curs curs' it

  and reset_next it = 
    let rec t = {it with 
      next = lazy (goto_next t);
      prev = lazy (goto_next {t with dir=IT.flip_dir it.dir});}
    in t

  and set_dir d it = reset_next {it with dir=d}
  and set_curs c it = reset_next {it with curs=c}

  let flip t = set_dir (IT.flip_dir t.dir) t
    
  let has_next it = match Lazy.force it.next with
    | None -> false
    | _ -> true

  let has_prev it = match Lazy.force it.prev with
    | None -> false
    | _ -> true

  let rec next it = match Lazy.force it.next with
    | None -> failwith "next"
    | Some it' -> it'

  let rec prev it = match Lazy.force it.next with
    | None -> failwith "prev"
    | Some it' -> it'

  let at_end t = 
    not (has_next t) && not (IT.has_curs_value t.curs)

  let at_beg = has_prev

  let rec goto_beg t = 
    if at_beg t then t
    else goto_beg (prev t)

  let rec goto_end t = 
    if at_end t then t
    else goto_end (next t)

  let from_cursor dir trav curs = 
    let t = 
      reset_next
	{dir = dir; trav = trav; curs = curs;
	 prev = lazy None;
	 next = lazy None;
	}
    in
      match trav with
	| Traverse_While f -> 
	    (* Check to see if f is false for the first element and if so
	       return an iterator that is always at_end. *)
	    if IT.has_curs_value curs && not (f (IT.get_curs_value curs)) then
	      {dir = dir; trav = trav; curs = curs; 
	       prev = lazy None; next = lazy None}
	    else t
	| _ ->
	    if IT.has_curs_value curs then t
	    else if at_end t then t
	    else next t (* move the cursor to the first value *)
	      
  let rec fold f acc it = 
    let acc = 
      if at_end it
      then acc
      else f acc (IT.get_curs_value it.curs)
    in
      if has_next it 
      then fold f acc (next it)
      else acc

  let iter f it = fold (fun () -> f) () it

  let get_value t = try IT.get_curs_value t.curs with _ -> failwith "get_value"
  let value t = try Some (get_value t) with _ -> None

end
