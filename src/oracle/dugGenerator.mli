(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Module to generate a random dug from a profile *)

module Make : 
  functor (DS : DugADT.S) -> 
    functor(A : Types.Mono.ArbitraryComparable) ->
sig
  val generate : DugProfile.Make(DS).t -> int
    -> ((A.t,Dug.Id.t) DS.generator, 
	(A.t,Dug.Id.t) DS.mutator, 
	(A.t,Dug.Id.t) DS.observer) Dug.t
end
