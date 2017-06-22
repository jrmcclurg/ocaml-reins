(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Signatures for set ADTs. *)

(** This module represents the core functionality of Sets.  It defines
    a few extra types to abstract over exact implement details of its
    operations.  Also, it defines the elements and the set type to be
    polymorphic, although this can later be refined to a monomorphic
    type (as is done bye {!Sets.MonoSetSig}.
*)
module type Set_ =
sig
  type 'a elt_
    (** The type of elements in the set *)

  type 'a set
    (** The type of sets *)

  type ('a,'b) result_
    (** The [result_] type is used for operations that may either
	return just a result or a result a something else.  Most trees
	conform to the former, while splay trees use the latter
	(e.g. the mem function may modify the tree) *)

  val empty : 'a set
    (** The empty set *)
    
  val is_empty : 'a set -> bool
    (** Returns true if the set is empty *)

  val mem : 'a elt_ -> 'a set -> (bool,'a) result_
    (** [mem x t] Returns true if [x] is contained in the set [t].
	More precisely, there exists an element [y] in [t] such that
	[compare x y = 0].  *)
    
  val add : 'a elt_ -> 'a set -> 'a set
    (** [add x t] Return the set [t] with the element [x].  
    *)

  val singleton : 'a elt_ -> 'a set
    (** [singleton x] Return the set consisting of only the element
	[x] *)

  val remove : 'a elt_ -> 'a set -> 'a set
    (** [remove x t] Return the set [t] with the element [x] removed.
	Does {b not} raise an exception if [t] does not contain [x]. *)

  val min_elt : 'a set -> ('a elt_,'a) result_
    (** Return the smallest element in the set.  If the set is empty,
	raises [Not_found] *)

  val max_elt : 'a set -> ('a elt_,'a) result_
    (** Return the largest element in the set.  If the set is empty,
	raises [Not_found] *)

  val choose : 'a set -> ('a elt_,'a) result_
    (** Choose an arbitrary element from the set.  It is
	implementation dependent whether or not the same element is
	chosen for equal sets.  If the set is empty, it raises
	[Not_found]. *)

  val cardinal : 'a set -> int
    (** Returns the number of elements in the set.  *)

  val compare : 'a set -> 'a set -> int
    (** [compare t1 t2] Compares the sets [t1] and [t2] and returns
	[0] if they are equal.  Returns [<0] if [t1] is less than [t2]
	and [>0] otherwise.
    *)

  val equal : 'a set -> 'a set -> bool
    (** [equal t1 t2] Returns true if [t1] and [t2] contain the same
	elements.  *)

  val iter : ('a elt_ -> unit) -> 'a set -> unit
    (** [iter f t] Apply [f] to each element in list [t].  The
	elements are always visited in increasing order. *)

  val fold : ('b -> 'a elt_ -> 'b) -> 'b -> 'a set -> 'b
    (** [fold f acc t] Accumulates the result [acc] by applying [f acc
	x] for each element [x] in [t].  The elements are always
	visited in increasing order.  Note that this is a slightly
	different signature than the fold from the standard library,
	however, it is the same signature as the lists modules use. *)
    
  val union : 'a set -> 'a set -> 'a set
    (** [union t1 t2] Returns a set containing all of the elements in
	[t1] and [t2] *)
    
  val inter : 'a set -> 'a set -> 'a set
    (** [inter t1 t2] Returns a set containing only the elements
	contained in both [t1] and [t2] *)

  val diff : 'a set -> 'a set -> 'a set
    (** [diff t1 t2] Returns a set containing only the elements
	contained in [t1] and not [t2] *)

  val gen1 :
    (?size:int -> Random.State.t -> 'a elt_) ->
    ?size:int -> Random.State.t -> 'a set
    (** [gen1 f ?size rs] Generates a random set whose size is bounded
	by [size].  Each element in the set is computed by calling [f
	?size rs].  *)

  val well_formed : 'a set -> bool
    (** A predicate to test if the set is well-formed.  All sets
	exposed by this API should always be well-formed.  This is
	only useful for debugging an implementation. *)

  val of_result : ('a,'b) result_ -> 'a
    (** Returns the result part of a [result_] value.  This is only
	useful when treating a collection of sets abstractly, as most
	clients should deconstruct the values of type [result_] for
	maximal efficiency *)

  (** The cursor interface to sets *)
    
  type 'a cursor_
    (** The type of Set cursors.  A cursor can be thought of a
	pointer to a node in the middle of a tree.  Cursors support
	navigating the tree in arbitrary ways.  Depending on the
	implementation, not every node in the tree may have a value
	associated with it. *)

  val to_cursor : 'a set -> 'a cursor_
    (** Create a cursor from a tree.  The cursor initially points to
	the top of the tree. *)

  val from_cursor : 'a cursor_ -> 'a set
    (** Return the tree pointed to by the cursor.  This operation may
	require re-balancing the tree depending on the implementation.
	*)

  val at_top : 'a cursor_ -> bool
    (** Returns true if the cursor is at the top of the tree.  The
	{!Sets.Set_.move_up} operation only succeeds when this
	returns [false].  *)

  val at_left : 'a cursor_ -> bool
    (** Returns true if the cursor is at the left most element in the
	current subtree.  The {!Sets.Set_.move_down_left}
	operation only succeeds when this returns [false].  *)

  val at_right : 'a cursor_ -> bool
    (** Returns true if the cursor is at the right most element in the
	current subtree.  The {!Sets.Set_.move_down_right}
	operation only succeeds when this returns [false].  *)

  val move_up : 'a cursor_ -> 'a cursor_
    (** Move the cursor up the tree from a sibling to a parent.  If
	the cursor is already at the top of the tree (as determined by
	{!Sets.Set_.at_top}), it raises [Failure "move_up"]. *)

  val move_down_left : 'a cursor_ -> 'a cursor_
    (** Move the cursor down the tree to the left child.  If the
	cursor is already at the bottom left of the tree (as
	determined by {!Sets.Set_.at_left}), it raises [Failure
	"move_down_left"]. *)

  val move_down_right : 'a cursor_ -> 'a cursor_
    (** Move the cursor down the tree to the right child.  If the
	cursor is already at the bottom right of the tree (as
	determined by {!Sets.Set_.at_right}), it raises [Failure
	"move_down_right"]. *)

  val went_left : 'a cursor_ -> bool
    (** Returns true if the cursor points to an element that is the
	left sibling of its parent. *)

  val went_right : 'a cursor_ -> bool
    (** Returns true if the cursor points to an element that is the
	right sibling of its parent. *)

  val has_value : 'a cursor_ -> bool
    (** Returns true if the cursor points to a node that contains a
	value.  *)

  val get_value : 'a cursor_ -> 'a elt_
    (** Extracts the value from the current node.  If the node does
	not contain a value (as determined by
	{!Sets.Set_.has_value}, then it raises [Failure
	"get_value"].  *)

end

(** A {!Sets.Set_} whose elements are monomorphic (possibly
    using a custom comparison function *)
module type MonoSetSig = sig
  type t
  type elt
  type cursor
  type 'a result
    
  include Set_ with type 'a elt_ = elt
	       and type 'a set = t
	       and type 'a cursor_ = cursor
	       and type ('a,'b) result_ = 'a result

  val to_string : 'a set -> string
end

module type MonoSetSigFn = 
  functor(C : Types.Mono.Comparable) ->
    MonoSetSig with type elt = C.t

module type MonoSetSigFnStd = 
  functor(C : Types.Mono.Comparable) ->
    MonoSetSig with type elt = C.t and type 'a result = 'a

(** The same as {!Sets.MonoSetSig} except includes a [gen] function *)
module type GenSetSig = sig
  include MonoSetSig
  val gen : ?size:int -> Random.State.t -> t
  end

module type GenSetSigFn = 
  functor(C : Types.Mono.ArbitraryComparable) ->
    GenSetSig with type elt = C.t

module type GenSetSigFnStd = 
  functor(C : Types.Mono.ArbitraryComparable) ->
    GenSetSig with type elt = C.t and type 'a result = 'a

(** A {!Sets.Set_} whose elements are polymorphic. *)
module type PolySetSig = sig
  type 'a t
  type 'a cursor
  type ('a,'b) result

  include Set_ with type 'a elt_ = 'a
	       and type 'a set = 'a t
	       and type 'a cursor_ = 'a cursor
	       and type ('a,'b) result_ = ('a,'b) result

  val to_string : ('a -> string) -> 'a set -> string

end

module type PolySetSigStd = PolySetSig with type ('a,'b) result = 'a
