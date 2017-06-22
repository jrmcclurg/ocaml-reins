(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Types

module type Set_ = sig
  type 'a elt_
  type 'a set
  type ('a,'b) result_

  val empty : 'a set
  val is_empty : 'a set -> bool
  val mem : 'a elt_ -> 'a set -> (bool,'a) result_
  val add : 'a elt_ -> 'a set -> 'a set
  val singleton : 'a elt_ -> 'a set
  val remove : 'a elt_ -> 'a set -> 'a set
  val min_elt : 'a set -> ('a elt_, 'a) result_
  val max_elt : 'a set -> ('a elt_, 'a) result_
  val choose : 'a set -> ('a elt_, 'a) result_
  val cardinal : 'a set -> int

  val compare : 'a set -> 'a set -> int
  val equal : 'a set -> 'a set -> bool
  val iter : ('a elt_ -> unit) -> 'a set -> unit
  val fold : ('b -> 'a elt_ -> 'b) -> 'b -> 'a set -> 'b
  val union : 'a set -> 'a set -> 'a set
  val inter : 'a set -> 'a set -> 'a set
  val diff : 'a set -> 'a set -> 'a set

  val gen1 : (?size:int -> Random.State.t -> 'a elt_) -> 
    ?size:int -> Random.State.t -> 'a set

  val well_formed : 'a set -> bool
  val of_result : ('a,'b) result_ -> 'a

  type 'a cursor_
  val to_cursor : 'a set -> 'a cursor_
  val from_cursor : 'a cursor_ -> 'a set
  val at_top : 'a cursor_ -> bool
  val at_left : 'a cursor_ -> bool
  val at_right : 'a cursor_ -> bool
  val move_up : 'a cursor_ -> 'a cursor_
  val move_down_left : 'a cursor_ -> 'a cursor_
  val move_down_right : 'a cursor_ -> 'a cursor_

  val went_left : 'a cursor_ -> bool
  val went_right : 'a cursor_ -> bool

  val has_value : 'a cursor_ -> bool
  val get_value : 'a cursor_ -> 'a elt_

(*
  val for_all : ('a elt_ -> bool) -> 'a set -> bool
  val exists : ('a elt_ -> bool) -> 'a set -> bool
  val elements : 'a set -> 'a elt_ list
  val subset : 'a set -> 'a set -> bool
  val filter : ('a elt_ -> bool) -> 'a set -> 'a set
  val partition : ('a elt_ -> bool) -> 'a set -> 'a set * 'a set
  val split : 'a elt_ -> 'a set -> 'a set * bool * 'a set
  val add_at : 'a elt_ -> cursor -> cursor
  val mem_at : 'a elt_ -> cursor -> bool
  val remove_at : 'a elt_ -> cursor -> cursor
*)

end    

module type MonoSetSig = sig
  type t
  type elt
  type cursor
  type 'a result

  include Set_
    with type 'a elt_ = elt
    and type 'a set = t
    and type 'a cursor_ = cursor
    and type ('a,'b) result_ = 'a result

  val to_string : 'a set -> string

end

module type MonoSetSigFn = 
  functor(C : Types.Mono.Comparable) ->
    MonoSetSig with type elt = C.t

module type MonoSetSigFnStd = 
  functor(C : Types.Mono.Comparable) ->
    MonoSetSig with type elt = C.t and type 'a result = 'a

module type GenSetSig = sig
  include MonoSetSig
  val gen : ?size:int -> Random.State.t -> t
end    

module type GenSetSigFn = 
  functor(C : Types.Mono.ArbitraryComparable) ->
    GenSetSig with type elt = C.t

module type GenSetSigFnStd = 
  functor(C : Types.Mono.ArbitraryComparable) ->
    GenSetSig with type elt = C.t and type 'a result = 'a

module type PolySetSig = sig
  type 'a t
  type 'a cursor
  type ('a,'b) result

  include Set_
    with type 'a elt_ = 'a
    and type 'a set = 'a t
    and type 'a cursor_ = 'a cursor
    and type ('a,'b) result_ = ('a,'b) result

  val to_string : ('a -> string) -> 'a set -> string
end

module type PolySetSigStd = PolySetSig with type ('a,'b) result = 'a
