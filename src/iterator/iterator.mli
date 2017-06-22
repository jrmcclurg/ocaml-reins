(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(* CR SW: It seems pointless to have both and ml and an mli for a definition of
   a module type.  I'd just have the ml.
*)
(** The signature for an iterator over an arbitrary collection *)
module type S = sig

  type 'a t
    (** The type of iterators.  An iterator serves as a pointer into
        the middle of a collection.  When possible, it always points
        to a valid element in the collection (skipping over any
	intermediate nodes that hold no value. *)

  type 'a elt
    (** The type of elements in the collection. *)
      
  type 'a cursor
    (** The type of the cursor that points into the collection *)
      
  type 'a collection
    (** The type of the collection *)
      
  type direction
    (** A type which guides the order of the traversal.  Different
	collections may support different directions. *)
    
  type 'a traversal =      
    | Traverse_All
	(** [Traverse_All] will visit every element in the collection. *)
    | Traverse_If of ('a -> bool)
	(** [Traverse_If f] will traverse only those elements for
	    which [f] returns true. *)
    | Traverse_While of ('a -> bool)
	(** [Traverse_While f] will traverse elements as long as [f]
	    is true. *)
	(** This type defines the traversal strategy.  It determines
	    which elements will be visited by the iterator.*)
	
  val create : direction -> 'a elt traversal -> 'a collection -> 'a t
    (** [create dir trav col] Create an iterator for the collection
	[col] using the direction and traversal given.  *)

  val from_cursor : direction -> 'a elt traversal -> 'a cursor -> 'a t
    (** [from_cursor dir trav curs] Create an iterator for the
	collection starting at the cursor [curs].  The cursor need not
	point to the beginning of the collection.  If it does point to
	an element, then this element will be the first element
	visited by the iterator.  *)

  val value : 'a t -> 'a elt option
    (** Return the element currently pointed to by the iterator.  This
	will return [None] only when the iterator has reached the end
	of the collection. *)

  val get_value : 'a t -> 'a elt
    (** Similar to {!Iterator.S.value} except it throws the exception [Failure
	"get_value"] if the iterator has reached the end of the
	collection . *)

  val at_end : 'a t -> bool
    (** Returns true if the iterator has reached the end of the
	collection as governed by the current traversal strategy. *)

  val at_beg : 'a t -> bool
    (** Returns true if the iterator is at the beginning of the
	collection as governed by the current traversal strategy.
	This is equivalent to {!Iterator.S.has_prev}.  *)

  val has_next : 'a t -> bool
    (** Returns true if there is another element in the traversal
	after the current element. *)

  val next : 'a t -> 'a t
    (** Advances the iterator to the next element in the collection.
	If the iterator is at the end of the collection, it raises
	[Failure "next"].
    *)

  val has_prev : 'a t -> bool
    (** Returns true if there is another element that occurs before
	the current element.  Equivalent to {!Iterator.S.at_beg}.  *)

  val prev : 'a t -> 'a t
    (** Advances the iterator to the previous element in the
	collection.  If the iterator is at the beginning of the
	collection, it raises [Failure "prev"].  *)

  val goto_beg : 'a t -> 'a t
    (** Advance the iterator to the beginning of the collection as
	governed by the traversal strategy *)
    
  val goto_end : 'a t -> 'a t
    (** Advance the iterator to the end of the collection as governed
	by the traversal strategy *)

  val flip : 'a t -> 'a t
    (** Reverse the direction of the iterator.  All elements that were
	previously reachable by [next] are now reachable by [prev] and
	vice versa. *)

  val iter : ('a elt -> unit) -> 'a t -> unit
    (** [iter f t] Apply [f] to each element in the collection that
	satisfies the traversal strategy.  If the iterator is not at
	the beginning of the collection, the elements reachable by
	{!Iterator.S.prev} will not be visited. *)

  val fold : ('a -> 'b elt -> 'a) -> 'a -> 'b t -> 'a
    (** [fold f acc t] Accumulates the result [acc] by applying [f acc
	x] for each element [x] in the collection that satisfies the
	traversal strategy.  If the iterator is not at the beginning
	of the collection, the elements reachable by
	{!Iterator.S.prev} will not be visited. *)

end
