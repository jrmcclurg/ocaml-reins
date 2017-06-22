(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Efficient maps over integers *)

module MonoKeyMap : 
  Maps.MonoKeyMapSig with type key = int
			and type 'e elt = 'e
  
module GenKeyMap : 
  Maps.GenKeyMapSig with type key = int
		       and type 'e elt = 'e
  
module MonoMap : 
  functor(C : Types.Mono.Comparable) ->
    Maps.MonoMapSig with type key = int
		       and type elt = C.t

module GenMap : 
  functor(C : Types.Mono.ArbitraryComparable) ->
    Maps.GenMapSig with type key = int
		      and type elt = C.t
