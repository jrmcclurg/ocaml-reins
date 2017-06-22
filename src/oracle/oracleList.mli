(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** List ADT which captures a DUG as a side effect *)

include DugADT.S
(*  
module Extractor : functor(A : Types.ArbitraryComparable) -> 
sig
  
  include Lists.S
  
  val get_dug : unit -> 
    ((unit,unit) generator, 
     (unit,unit) mutator,
     (unit,unit) observer) Dug.t
      
  val clear_profile : unit -> unit
end

  
module Benchmark : functor(L : Lists.S) ->
sig
  val benchmark : 
    (('a,Dug.Id.t) generator, 
     ('a,Dug.Id.t) mutator, 
     ('a,Dug.Id.t) observer) Dug.t
      -> float
end
*)
