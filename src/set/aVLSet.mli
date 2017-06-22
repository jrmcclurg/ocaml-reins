(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Height balanced binary search trees implementing sets

    AVL trees are balanced binary search trees with O(log n) lookup,
    add, and remove operations.  The set operations [union], [inter],
    and [diff] all take O(n) time.  However, some inputs to these
    functions will take significantly less time to process (e.g. when
    one tree is significantly smaller than the other, or when the
    trees have large number consecutive elements that do not overlap).
*)

(** This module provides an implementation of AVL trees with a
    polymorphic element type.  The implementation uses the standard
    library's polymorphic [compare] function internally and may not be
    as efficient as the {!AVLSet.MonoSet} module which allows the use
    of a more efficient comparison function.
*)
module PolySet : Sets.PolySetSigStd

(** This functor provides an implementation of AVL trees that are
    parameterized by a specific monomorphic element type.
*)
module MonoSet : Sets.MonoSetSigFnStd
  
(** This functor is similar to the {!AVLSet.MonoSet} functor except
    it is parameterized by a module that also supports the [gen]
    operation.  Therefore, the resulting module is also able to
    generate number sets.
*)
module GenSet :  Sets.GenSetSigFnStd


(** All of the module below are variations of the above modules that
    allow client code to control the performance of the AVL tree.
    Note that in most cases, the modules defined above will perform
    the best. *)

(** This functor is similar to the {!AVLSet.PolySet} module above,
    except it allows the user to specify the maximum difference
    between the heights of two subtrees at a node with [HeightDiff.v].
    The choice of this value affects the amount of effort spent
    rebalancing the tree after it has been modified in exchange for
    the cost of locating a particular element in the tree.  The
    modules {!AVLSet.PolySet1}, {!AVLSet.PolySet2}, and
    {!AVLSet.PolySet3} below instantiate this functor with the values
    1, 2, and 3 respectively.  Those modules are also defined in the
    same compilation unit as the implementation code, so the value of
    HeightDiff.v is inlined, increasing performance.
*)
module AVL_PolySet: 
  functor(HeightDiff : sig val v : int end) -> 
    Sets.PolySetSigStd

(** {!AVLSet.AVL_PolySet} instanced with HeightDiff.v = 1 *)
module PolySet1 : Sets.PolySetSigStd
  
(** {!AVLSet.AVL_PolySet} instanced with HeightDiff.v = 2 *)
module PolySet2 : Sets.PolySetSigStd
  
(** {!AVLSet.AVL_PolySet} instanced with HeightDiff.v = 3 *)
module PolySet3 : Sets.PolySetSigStd

(** This functor is similar to the {!AVLSet.MonoSet} module above,
    except it allows the user to specify the maximum difference
    between the heights of two subtrees at a node with [HeightDiff.v].
    The choice of this value affects the amount of effort spent
    rebalancing the tree after it has been modified in exchange for
    the cost of locating a particular element in the tree.  The
    modules {!AVLSet.MonoSet1}, {!AVLSet.MonoSet2}, and
    {!AVLSet.MonoSet3} below instantiate this functor with the values
    1, 2, and 3 respectively.  Those modules are also defined in the
    same compilation unit as the implementation code, so the value of
    HeightDiff.v is inlined, increasing performance.
*)
module AVL_MonoSet: 
  functor(HeightDiff : sig val v : int end) -> 
    Sets.MonoSetSigFnStd
      
(** {!AVLSet.AVL_MonoSet} instanced with HeightDiff.v = 1 *)
module MonoSet1: Sets.MonoSetSigFnStd

(** {!AVLSet.AVL_MonoSet} instanced with HeightDiff.v = 2 *)
module MonoSet2: Sets.MonoSetSigFnStd

(** {!AVLSet.AVL_MonoSet} instanced with HeightDiff.v = 3 *)
module MonoSet3: Sets.MonoSetSigFnStd


(** This functor is similar to the {!AVLSet.GenSet} module above,
    except it allows the user to specify the maximum difference
    between the heights of two subtrees at a node with [HeightDiff.v].
    The choice of this value affects the amount of effort spent
    rebalancing the tree after it has been modified in exchange for
    the cost of locating a particular element in the tree.  The
    modules {!AVLSet.GenSet1}, {!AVLSet.GenSet2}, and
    {!AVLSet.GenSet3} below instantiate this functor with the values
    1, 2, and 3 respectively.  Those modules are also defined in the
    same compilation unit as the implementation code, so the value of
    HeightDiff.v is inlined, increasing performance.
*)
module AVL_GenSet :
  functor(HeightDiff : sig val v : int end)  ->
    Sets.GenSetSigFnStd

(** {!AVLSet.AVL_GenSet} instanced with HeightDiff.v = 1 *)
module GenSet1 : Sets.GenSetSigFnStd
  
(** {!AVLSet.AVL_GenSet} instanced with HeightDiff.v = 2 *)
module GenSet2 : Sets.GenSetSigFnStd

(** {!AVLSet.AVL_GenSet} instanced with HeightDiff.v = 3 *)
module GenSet3 : Sets.GenSetSigFnStd

