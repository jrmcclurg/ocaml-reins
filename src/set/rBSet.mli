(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Balanaced binary search tree with small memory footprint

    Redblack trees are balanced binary search trees that provide O(log
    n) [mem], [add], and [remove] tree operations and O(n) [union],
    [inter], and [diff] set operations.  They can also be more memory
    efficient than AVL trees since they only need to store 1 bit of
    information to maintain their internal invariants.  In the current
    implementation, this bit is encoded in the type constructor,
    meaning that each internal node of the tree uses one less word of
    memory than AVL trees.
*)

(** This module provides an implementation of RedBlack trees with a
    polymorphic element type.  The implementation uses the standard
    library's polymorphic [compare] function internally and may not be
    as efficient as the {!RBSet.MonoSet} module which allows the use
    of a more efficient comparison function.
*)
module PolySet : Sets.PolySetSigStd

(** This functor provides an implementation of RedBlack trees that are
    parameterized by a specific monomorphic element type.
*)
module MonoSet : Sets.MonoSetSigFnStd

(** This functor is similar to the {!RBSet.MonoSet} functor except it
    is parameterized by a module that also supports the [gen]
    operation.  Therefore, the resulting module is also able to
    generate number sets.
*)
module GenSet : Sets.GenSetSigFnStd
