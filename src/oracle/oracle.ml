(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)


module type RestrictedSet = Sets.GenSetSig with type 'a result = 'a

module type ProfiledSet = sig
  include RestrictedSet
  include DugADT.S
end


module type OSet = sig
  include DugADT.S

  module Extractor : functor(A : Types.Mono.ArbitraryComparable) -> Sets.GenSetSig

  module BenchMark : functor(S : Sets.GenSetSig) -> sig
    val benchmark : 
      ((S.elt,Dug.Id.t) generator, 
       (S.elt,Dug.Id.t) mutator, 
       (S.elt,Dug.Id.t) observer) Dug.t
      -> float
  end
end

(*
module ExtractSet(A : MonoTypes.ArbitraryComparable) = 
struct
  module M = OracleSet.Make(A)
  include M.Extractor
end

*)
