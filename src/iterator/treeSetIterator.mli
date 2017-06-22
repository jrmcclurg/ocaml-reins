(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** The signature for an iterator over a tree impelementing a set. *)
module type S =
sig
  
  type ordering = PreOrder | InOrder | PostOrder
      (** A [PreOrder] traversal always visits the root of the tree
	  before its children.  An [InOrder] traversal visits one
	  subtree, then the root, then the other subtree (which
	  subtree is chosen by the [direction_] type below).  Finally,
	  a [PostOrder] traversal visits the subtrees before visiting
	  the root. *)

  type direction_ = Ascending of ordering | Descending of ordering
    (** An ascending direction traversal always visits the elements in
	increasing order of the keys.  Similarly, the descending
	direction traversal visits elements in decreasing key
	order. *)

  include Iterator.S with type direction = direction_    
end

(** Create an iterator for a Set (note that this implicitly supports
    both MonoSets and PolySets).
*)
module Make :
  functor (T : Sets.Set_) ->
    S with type 'a elt = 'a T.elt_
      and type 'a cursor = 'a T.cursor_
      and type 'a collection = 'a T.set

