(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Balanaced binary search tree with small memory footprint *)

module PolyMap : Maps.PolyMapSig

module MonoKeyMap : Maps.MonoKeyMapSigFnStd

module GenKeyMap : Maps.GenKeyMapSigFnStd

module MonoMap : Maps.MonoMapSigFnStd

module GenMap : Maps.GenMapSigFnStd
