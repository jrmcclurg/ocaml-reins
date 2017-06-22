(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Binomial Heap.  All operations are O(log n) time. *)

module MonoHeap : Heaps.MonoHeapSigFn

module GenHeap : Heaps.GenHeapSigFn

module PolyHeap : Heaps.PolyHeapSig
