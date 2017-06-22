(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)


(** Frontend to automatic benchmarking of data structures (work in progress) *)

module type RestrictedSet = Sets.GenSetSig with type 'a result = 'a

module type ProfiledSet = sig
  include RestrictedSet
  include DugADT.S
end

(*
module Set :
  functor(S : RestrictedSet) -> 
    functor(A : Types.ArbitraryComparable with type t = S.elt) ->
      ProfiledSet with type elt = A.t
*)
