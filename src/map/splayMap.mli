(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Maps with excellent non-uniform access performance *)

module rec PolyMap : 
  Maps.PolyMapSig with type ('a,'k,'v) result = 'a * ('k,'v) PolyMap.t

module rec MonoKeyMap : 
  functor(C : Types.Mono.Comparable) ->
    Maps.MonoKeyMapSig with type key = C.t
		       and type ('a,'v) result = 'a * 'v MonoKeyMap(C).t

module rec GenKeyMap : 
  functor(C : Types.Mono.ArbitraryComparable) ->
    Maps.GenKeyMapSig with type key = C.t
		   and type ('a,'v) result = 'a * 'v GenKeyMap(C).t
  
module rec MonoMap : 
  functor(K : Types.Mono.Comparable) ->
    functor(V : Types.Mono.Comparable) ->
      Maps.MonoMapSig with type key = K.t
		      and type elt = V.t
		      and type 'a result = 'a * MonoMap(K)(V).t

module rec GenMap :
  functor(K : Types.Mono.ArbitraryComparable) ->
    functor(V : Types.Mono.ArbitraryComparable) ->
      Maps.GenMapSig with type key = K.t
		     and type elt = V.t
		     and type 'a result = 'a * GenMap(K)(V).t
