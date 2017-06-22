(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Set ADT which captures a DUG as a side effect *)

include DugADT.S
  
module Extractor : functor(A : Types.Mono.ArbitraryComparable) -> 
sig
  
  include Sets.GenSetSig with type 'a result = 'a
			 and type elt = A.t
  
  val get_dug : unit -> 
    ((elt,Dug.Id.t) generator, 
     (elt,Dug.Id.t) mutator,
     (elt,Dug.Id.t) observer) Dug.t
      
  val clear_profile : unit -> unit
end

  
module Benchmark : 
  functor(S : Sets.GenSetSig with type 'a result = 'a) -> 
sig
  val benchmark : 
    ((S.elt,Dug.Id.t) generator, 
     (S.elt,Dug.Id.t) mutator, 
     (S.elt,Dug.Id.t) observer) Dug.t
      -> float
end
