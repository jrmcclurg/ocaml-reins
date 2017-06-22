(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Skew Binomial Heap.  O(1) insert, O(log n) rest *)

module MonoHeap : Heaps.MonoHeapSigFn

module GenHeap : Heaps.GenHeapSigFn

module PolyHeap : Heaps.PolyHeapSig