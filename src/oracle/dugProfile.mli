(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Charactericists of a DUG *)

module Make : functor(DS : DugADT.S) -> sig

  type t = private {
      (* CDF for the different node types (not operation weights)
         i.e., union counts as 1, not 2
      *)
      gen_cdf : ((unit,unit) DS.generator * float) list;
      mut_cdf : ((unit,unit) DS.mutator * float) list;
      obs_cdf : ((unit,unit) DS.observer * float) list;

      (* ratio of generator nodes to total nodes *)
      gen_ratio : float;

      (* ratio of observations / mutations *)
      obs_mut_ratio : float;

      (* fraction of version nodes (gen or mut) that are never
	 mutated *)
      mortality : float;

      (* fraction of mutations that are persisent *)
      pmf : float;
      
      (* fraction of observations that are persisent *)
      pof : float;
    }
      
  val random_op : ('a * float) list -> 'a
    
  val to_string : t -> string
    
  val profile : 
    (('a,'b) DS.generator, ('a,'b) DS.mutator, ('a,'b) DS.observer) Dug.t
    -> t
end
