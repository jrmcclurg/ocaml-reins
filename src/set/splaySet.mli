(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Sets with excellent non-uniform access performance

    Splay trees are binary search trees that are balanced based on
    recently accessed elements.  They provide amortized O(log n)
    performance for tree operations ([mem], [add], [remove]), and O(n)
    amortized time for set operations.  Splay trees do not maintain
    any invariant information and are therefore very memory efficient.
    To achieve their amortized bounds, splay trees re-balance
    themselves on every tree access (e.g., [mem]).  Re-balancing
    always leaves the most recently accessed element at the root of
    the tree.  Therefore repeated access to recent elements can be
    very efficient.  However, this also means that tree operations may
    take O(n) for degenerate cases.
*)

(** This module provides an implementation of Splay trees with a
    polymorphic element type.  The implementation uses the standard
    library's polymorphic [compare] function internally and may not be
    as efficient as the {!SplaySet.MonoSet} module which allows the
    use of a more efficient comparison function.
*)
module rec PolySet : Sets.PolySetSig
  with type ('a,'b) result = 'a * 'b PolySet.t
  
(** This functor provides an implementation of Splay trees that are
    parameterized by a specific monomorphic element type.  The
    resulting module may be more efficient than its polymorphic
    counterpart, {!SplaySet.PolySet}.
*)
module rec MonoSet : functor(C: Types.Mono.Comparable) -> 
  Sets.MonoSetSig with type elt = C.t
		  and type 'a result = 'a * MonoSet(C).t

(** This functor is similar to the {!SplaySet.MonoSet} functor except
    it is parameterized by a module that also supports the [gen]
    operation.  Therefore, the resulting module is also able to
    generate number sets.
*)
module rec GenSet : functor(C: Types.Mono.ArbitraryComparable) -> 
  Sets.GenSetSig with type elt = C.t
		 and type 'a result = 'a * GenSet(C).t

