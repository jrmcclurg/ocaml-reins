(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Acyclic doubly linked lists 

    This module implements acyclic doubly linked lists that support
    O(1) navigation.  The running time of the rest of the operations
    depend on the argument [L].  All operations are explained assuming
    the list is visually laid out from left to right.  That is, the
    front of the list is on the left and the end of the list is on the
    right.
*)

module Make(L : Lists.ListSig) : sig
  type 'a t
      (** The type of doubly linked lists.  This type can be thought
	  of as a cursor pointing into the middle of a [L.t] list.
	  Elements to the right of [t] can be reached with [hd], [tl],
	  [pop], and [next].  Elements to the left of [t] can be
	  reached with [prev_hd], [prev_tl], [prev_pop], and
	  [prev].  *)
      
  val empty : 'a t
    (** The empty list *)
    
  val is_empty : 'a t -> bool
    (** Returns true if the list is empty.  That is, there are no
	elements to the left or right of [t].  Runs in the same time
	and stack space as [L.is_empty].  *)
    
  val at_front : 'a t -> bool
    (** [at_front t] Retruns true if there are no elements to the left
	of [t].  Runs in the same time and stack space as [L.is_empty].  *)
    
  val at_back : 'a t -> bool
    (** [at_front t] Retruns true if there are no elements to the
	right of [t].  Runs in the same time and stack space as
	[L.is_empty].  *)
    
  val length : 'a t -> int
    (** [length t] Returns the length of the entire list.  Runs in the
	same time and stack space as [L.length].  *)
    
  val next_length : 'a t -> int
    (** [next_length t] Returns the number of elements to the right of
	[t].  Runs in the same time and stack space as [L.length].  *)
    
  val prev_length : 'a t -> int
    (** [prev_length t] Returns the number of elements in front of
	[t].  Runs in the same time and stack space as [L.length].  *)

  val rev : 'a t -> 'a t
    (** [rev t] Reverse the list [t].  All elements that were in front
	of [t] are now to the right of it and vice versa.  Runs in
	O(1) time and stack space. *)

  val hd : 'a t -> 'a
    (** [hd t] Returns the element to the immediate right of [t].
	Runs in the same time and stack space as [L.hd].  If there are
	no elements to the right of [t], it raises [Failure "hd"].  *)

  val tl : 'a t -> 'a t
    (** [tl t] Return the list with the first element to the right of
	[t] removed.  Runs in the same time and stack space as [L.tl].
	If there are no elements to the right of [t], it raises
	[Failure "tl"].  *)

  val pop : 'a t -> 'a * 'a t
    (** [pop t] Equivalent to [(hd t), (tl t)] but is slightly more
	efficient.  Runs in the same time and stack space as [L.pop].
	If there are no elements to the right of [t], it raises
	[Failure "pop"].  *)

  val last : 'a t -> 'a
    (** [last t] Returns the last element the right of [t].  Runs in
	the same time and stack space as [L.last].  If there are no
	elements to the right of [t], it raises [Failure "last"].  *)

  val next : 'a t -> 'a t
    (** [next t] Advance [t] to the next element in the list.  The
	element to the right of [t] is now to the left of the result.
	Runs in the same time and stack space as the maximum of [L.hd]
	and [L.cons].  If there are no elements to the right of [t],
	it raises [Failure "next"].  *)

  val prev_hd : 'a t -> 'a
    (** [prev_hd t] Returns the element to the left of [t].  Runs in
	the same time and space as [L.hd].  If there are no element to
	the left of [t], it raises [Failure "prev_hd"].  *)

  val prev_tl : 'a t -> 'a t
    (** [prev_tl t] Return the list with the first element to the left
	of [t] removed.  Runs in the same time and stack space as
	[L.tl].  If there are no elements to the left of [t], it
	raises [Failure "prev_tl"].  *)

  val prev_pop : 'a t -> 'a * 'a t
    (** [prev_pop t] Equivalent to [(prev_hd t), (prev_tl t)] but is
	slightly more efficient.  Runs in the same time and stack
	space as [L.pop].  If there are no elements to the left of [t],
	it raises [Failure "prev_pop"].  *)

  val prev : 'a t -> 'a t
    (** [prev t] Advance [t] to the previous element in the list.  The
	element to the left of [t] is now to the right of the result.
	Runs in the same time and stack space as the maximum of [L.hd]
	and [L.cons].  If there are no elements to the left of [t], it
	raises [Failure "prev"].  *)

  val cons : 'a -> 'a t -> 'a t
    (** [cons x t] Adds [x] as the first element to the right of [t].
	Runs in the same time and stack space as [L.cons]. *)

  val prev_cons : 'a -> 'a t -> 'a t
    (** [prev_cons x t] Adds [x] as the first element to the left of [t].
	Runs in the same time and stack space as [L.cons]. *)

  val snoc : 'a -> 'a t -> 'a t
    (** [snoc x t] Adds [x] as the last element to the right of [t]
	(i.e., the last element in the entire list).  The resulting
	list otherwise has the same elements to the left of it and to
	the right of it as [t] (i.e., the position has not changed).
	Runs in the same time and stack space as [L.snoc].  *)

  val prev_snoc : 'a -> 'a t -> 'a t
    (** [snoc x t] Adds [x] as the last element to the left of t [t]
	(i.e., the first element in the entire list).  The resulting
	list otherwise has the same elements to the left of it and to
	the right of it as [t] (i.e., the position has not changed).
	Runs in the same time and stack space as [L.snoc].  *)

  val append : 'a t -> 'a t -> 'a t
    (** [append t1 t2] Append the list [t2] onto the back of [t1].
	The resulting list has the same position as [t1].  Runs in the
	O(|t2| + LA) time where LA is the running time of [L.append].
	It uses O(1 + LS) stack space where LS is the stack space
	required by [L.append].  *)

  val splice : 'a t -> 'a t -> 'a t
    (** [splice t1 t2] Splices the elements of [t1] into [t2].  The
	resulting list has the shape:

	prev_l2 @ prev_l1 @ next_l1 @ next_l2
	
	Runs in the same time and stack space as [L.append].
    *)

  val flatten : 'a t t -> 'a t
    (** [flatten l] Appends all of the elements of [l] into a new list.
	Currently ineffeciently implemented and has greater than O(n)
	running time. *)

  val from_list : 'a list -> 'a t
    (** [from_list l] Convert the standard list [l] into a {!DList.t}.
	Runs in the same time and stack space as [L.from_list]. The
	resulting cursor points to the front of the list. *)

  val to_list : 'a t -> 'a list
    (** [to_list t] Convert the DList [t] into a standard list.  Runs
	in O(|t|) time and O(1) stack space.  The position of [t] does
	not affect the order of the resulting list.  *)

  val iter : ('a -> unit) -> 'a t -> unit
    (** [iter f t] Iterates over each element in the list [t] and
	applies [f] to that element.  The elements to the right of [t]
	are visited first in order, following by the elements to the
	left of [t] in reverse order.  Runs in the same time and stack
	space as [L.iter]. *)

  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    (** [fold f acc t] Accumulates the result [acc] by applying [f acc
	x] for each element [x] in [t].  The elements to the right of
	[t] are visited first in order, following by the elements to
	the left of [t] in reverse order.  Runs in the same time and
	stack space as [L.fold]. *)

  val rev_map : ('a -> 'b) -> 'a t -> 'b t
    (** [rev_map f t] Creates a new list by applying [f] to each
	element of [t].  The resulting list is in reverse order of [t]
	and the cursor of the resulting list points to the same
	location as [t] whith the next and previous elements reversed.
	e.g., if [e == hd t], then [f(e) == prev_hd (rev_map f t)]
	Runs in the same time and stack space as [L.map].  *)

  val map : ('a -> 'b) -> 'a t -> 'b t
    (** [map f t] Creates a new list by applying [f] to each element
	of [t].  The resulting list is in the same order as [t] and
	the cursor points to the same location as [t].  e.g., if [e ==
	hd t], then [f(e) == hd (map f t)].  Runs in the same time and
	stack space as [L.map]. *)

  val to_string : ('a -> string) -> 'a t -> string
    (** [to_string to_s t] Convert the list [t] into a string using
	[to_s] to individually convert each element into a string.
	Runs in O(|t|*st) where st is the running time of [to_s] and
	uses O(ss) stack space where ss is the amount of stack
	required by [to_s].  *)

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** [compare f t1 t2] Compares the lists [t1] and [t2] using [f]
	to compare individual elements.  Returns 0 if [t1] and [t2]
	are equal (under f).  Returns [<0] if [t1] is less than [t2]
	and returns [>0] otherwise.  Runs in O(min(|t1|, |t2|)) time
	and O(1) stack space. *)

  val gen : (?size:int -> Random.State.t -> 'a) -> ?size:int -> Random.State.t -> 'a t
    (** [gen f ?size rs] Generates a random list whose length is bounded
	by [size].  Each element in the list is computed by calling [f
	?size rs].  Runs in time O([size] * ft) where ft is the running
	time of [f] and uses O(fs) stack space where fs is the stack space
	of [f].  The location of the cursor is not defined.
    *)

  include ListCursor.S with type 'a list_ = 'a t and type 'a cursor = 'a t
  (** Note that the type [cursor] is the same as [t].  Therefore all
      {!List_Cursor.S} operations can be applied directly to values of
      type DList.t *)
end
