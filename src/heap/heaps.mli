(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Abstract signature for Heaps *)

module type Heap_ =
sig

  type 'a elt_

  type 'a heap

  val empty : 'a heap

  val is_empty : 'a heap -> bool

  val singleton : 'a elt_ -> 'a heap

  val insert : 'a elt_ -> 'a heap -> 'a heap

  val merge : 'a heap -> 'a heap -> 'a heap

  val find_min : 'a heap -> 'a elt_

  val delete_min : 'a heap -> 'a heap

end

module type MonoHeapSig = sig
  type t
  type elt
    
  include Heap_ with type 'a elt_ = elt
	       and type 'a heap = t

  val to_string : 'a heap -> string
end

module type MonoHeapSigFn = 
  functor(C : Types.Mono.Comparable) ->
    MonoHeapSig with type elt = C.t

module type GenHeapSig = sig
  include MonoHeapSig
  val gen : ?size:int -> Random.State.t -> t
end

module type GenHeapSigFn = 
  functor(C : Types.Mono.ArbitraryComparable) ->
    GenHeapSig with type elt = C.t

module type PolyHeapSig = sig
  type 'a t

  include Heap_ with type 'a elt_ = 'a
	       and type 'a heap = 'a t

  val to_string : ('a -> string) -> 'a heap -> string
end

