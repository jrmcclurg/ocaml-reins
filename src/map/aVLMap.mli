(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Height balanced binary search trees implementing maps *)

module PolyMap : Maps.PolyMapSigStd
module MonoKeyMap : Maps.MonoKeyMapSigFnStd
module GenKeyMap : Maps.GenKeyMapSigFnStd
module MonoMap : Maps.MonoMapSigFnStd
module GenMap : Maps.GenMapSigFnStd

module AVL_PMap :
  functor(HeightDiff : sig val v : int end) -> Maps.PolyMapSig

module AVL_KeyMap :
  functor(HeightDiff : sig val v : int end) -> Maps.MonoKeyMapSigFnStd

module AVL_GenKeyMap :
  functor(HeightDiff : sig val v : int end) -> Maps.GenKeyMapSigFnStd

module AVL_Map :
  functor(HeightDiff : sig val v : int end) -> Maps.MonoMapSigFnStd

module AVL_GenMap :
  functor(HeightDiff : sig val v : int end) -> Maps.GenMapSigFnStd

module Poly1 : Maps.PolyMapSig
module Poly2 : Maps.PolyMapSig
module Poly3 : Maps.PolyMapSig

module MonoKey1 : Maps.MonoKeyMapSigFnStd
module MonoKey2 : Maps.MonoKeyMapSigFnStd
module MonoKey3 : Maps.MonoKeyMapSigFnStd

module GenKey1 : Maps.GenKeyMapSigFnStd
module GenKey2 : Maps.GenKeyMapSigFnStd
module GenKey3 : Maps.GenKeyMapSigFnStd

module Mono1 : Maps.MonoMapSigFnStd
module Mono2 : Maps.MonoMapSigFnStd
module Mono3 : Maps.MonoMapSigFnStd

module Gen1 : Maps.GenMapSigFnStd
module Gen2 : Maps.GenMapSigFnStd
module Gen3 : Maps.GenMapSigFnStd

