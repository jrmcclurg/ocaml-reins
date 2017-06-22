(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Cursor interface for Lists *)

module type S =
sig
  type 'a list_
    (** The underlying list type the cursor points to. *)

  type 'a cursor
      (** The type of list cursors.  A cursor can be thought of a
	  pointer into the middle of a list.  More specifically,
	  cursors point to edges between list elements, not elements
	  directly.  A cursor can be move to the left (towards the
	  front of the list) or to the right (towards the back of the
	  list) and supports updating the list at the current position
	  efficiently. *)
      
  val to_cursor : 'a list_ -> 'a cursor
    (** [to_cursor t] Create a cursor that points to the beginning of
	list [t].  Runs in O(1) time and space. 
    *)

  val from_cursor : 'a cursor -> 'a list_
    (** [from_cursor curs] Return the list that is pointed to by
	[curs].  Runs in O(n) time and O(1) stack space where n is the
	number of elements to the left of [curs].
    *)

  val at_front : 'a cursor -> bool
    (** [at_front curs] Returns true if there are no elements to the
	left of [curs].  Runs in O(1) time and stack space.
    *)

  val at_back : 'a cursor -> bool
    (** [at_end curs] Returns true if there are no elements to the
	right of [curs].  Runs in O(1) time and stack space.  *)

  val move_next : 'a cursor -> 'a cursor
    (** [move_left curs] Moves the cursor one element to the left.  If
	there are no elements to the left of [curs] (i.e., [curs]
	points to the front of the list), it raises [Failure
	"move_left"].  Runs in O(1) time and stack space. 
    *)

  val move_prev : 'a cursor -> 'a cursor
    (** [move_right curs] Moves the cursor one element to the right.
	If there are no elements to the right of [curs] (i.e., [curs]
	points to the end of the list), it raises [Failure
	"move_right"].  Runs in O(1) time and stack space. 
    *)

  val goto_front : 'a cursor -> 'a cursor
    (** [goto_front curs]  Moves the cursor to the front of the
	list.  Runs in O(n) time and O(1) stack space where n is the
	number of elements to the left of [curs].  
    *)

  val goto_back : 'a cursor -> 'a cursor
    (** [goto_back curs] Moves the cursor to the back of the list.
	Runs in O(n) time and O(1) stack space where n is the number
	of elements to the right of [curs].  *)

  val value : 'a cursor -> 'a option
    (** If the cursor currently points to an element [x], return that
	element as [Some x], otherwise return [None].  *)

  val list : 'a cursor -> 'a list_
    (** [list curs] Returns all of the elements to the right of [curs]
	as a ['a list_].  Runs in O(1) time and stack space. *)

  val replace_list : 'a list_ -> 'a cursor -> 'a cursor
    (** [replace_list l curs] Replaces the list of elements to the
	right of [curs] with [l].  Runs in O(1) time and stack
	space. *)
end

module Make : functor (L : Lists.ListSig) -> S with type 'a list_ = 'a L.t
