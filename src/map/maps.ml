(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Types

module type Map_ = sig
  type 'k key_
  type 'e elt_
  type ('k,'e) map

  (* The {bool,key}_result_ types are used for operations that may
     either return just a bool (key resp.) or a bool and something
     else (key and something else resp.) .  Most trees conform to the
     former, while splay trees use the latter (e.g. the mem function
     may modify the tree) *)
  type ('a,'k,'e) result_

  val empty : ('k, 'e) map
  val is_empty : ('k, 'e) map -> bool
  val mem : 'k key_ -> ('k, 'e) map -> (bool,'k,'e) result_
  val add : 'k key_ -> 'e elt_ -> ('k, 'e) map -> ('k, 'e) map
  val singleton : 'k key_ -> 'e elt_ -> ('k, 'e) map
  val remove : 'k key_ -> ('k, 'e) map -> ('k, 'e) map
  val find : 'k key_ -> ('k,'e) map -> ('e elt_,'k,'e) result_

  val min_key : ('k, 'e) map -> ('k key_,'k,'e) result_
  val max_key : ('k, 'e) map -> ('k key_,'k,'e) result_

  val min_keyval : ('k, 'e) map -> ('k key_ * 'e elt_,'k,'e) result_
  val max_keyval : ('k, 'e) map -> ('k key_ * 'e elt_,'k,'e) result_

  val cardinal : ('k, 'e) map -> int

  val iter : ('k key_ -> 'e elt_ -> unit) -> ('k, 'e) map -> unit

  val fold : ('acc -> 'k key_ -> 'e elt_ -> 'acc) -> 'acc -> ('k, 'e) map
    -> 'acc

  val map : ('e elt_ -> 'f elt_) -> ('k, 'e) map -> ('k, 'f) map

  val mapi : ('k key_ -> 'e elt_ -> 'f elt_) -> ('k, 'e) map -> ('k, 'f) map

  val union : ('k key_ -> 'e elt_ -> 'e elt_ -> 'e elt_) 
    -> ('k, 'e) map -> ('k, 'e) map -> ('k, 'e) map

  val inter : ('k key_ -> 'e elt_ -> 'e elt_ -> 'e elt_)
    -> ('k, 'e) map -> ('k, 'e) map -> ('k, 'e) map

  val diff : ('k key_ -> 'e elt_ -> 'e elt_ -> bool) 
    -> ('k, 'e) map -> ('k, 'e) map -> ('k, 'e) map

  val well_formed : ('k, 'e) map -> bool

  val of_result : ('a,'k,'e) result_ -> 'a

  type ('k, 'e) cursor_
  val to_cursor : ('k, 'e) map -> ('k, 'e) cursor_
  val from_cursor : ('k, 'e) cursor_ -> ('k, 'e) map
  val at_top : ('k, 'e) cursor_ -> bool
  val at_left : ('k, 'e) cursor_ -> bool
  val at_right : ('k, 'e) cursor_ -> bool
  val move_up : ('k, 'e) cursor_ -> ('k, 'e) cursor_
  val move_down_left : ('k, 'e) cursor_ -> ('k, 'e) cursor_
  val move_down_right : ('k, 'e) cursor_ -> ('k, 'e) cursor_

  val went_left : ('k, 'e) cursor_ -> bool
  val went_right : ('k, 'e) cursor_ -> bool

  val has_value : ('k, 'e) cursor_ -> bool
  val get_value : ('k, 'e) cursor_ -> 'k key_ * 'e elt_

end    

module type PolyMapSig = sig
  type ('k,'e) t
  type 'k key = 'k
  type 'e elt = 'e
  type ('k,'e) cursor
  type ('a,'k,'v) result

  include Map_
    with type 'a key_ = 'a
    and type 'e elt_ = 'e
    and type ('k,'e) map = ('k,'e) t
    and type ('k,'e) cursor_ = ('k, 'e) cursor
    and type ('a,'k,'v) result_ = ('a,'k,'v) result

  val gen2 : 
    (?size:int -> Random.State.t -> 'k key_) -> 
    (?size:int -> Random.State.t -> 'e elt_) -> 
    ?size:int -> Random.State.t -> ('k, 'e) map

  val to_string : ('k -> 'e -> string) -> ('k, 'e) map -> string

  val compare : ('k -> 'k -> int) -> ('e -> 'e -> int) -> ('k,'e) t
    -> ('k,'e) t -> int

  val compare_keys : ('k -> 'k -> int) -> ('k,'e) t -> ('k,'e) t ->
  int

end

module type PolyMapSigStd = PolyMapSig with type ('a,'k,'v) result = 'a

module type MonoKeyMapSig = sig
  type 'e t
  type key
  type 'e elt = 'e
  type 'e cursor
  type ('a,'v) result

  include Map_
    with type 'k key_ = key
    and type 'e elt_ = 'e
    and type ('k,'e) map = 'e t
    and type ('k,'e) cursor_ = 'e cursor
    and type ('a,'k,'v) result_ = ('a,'v) result

  val compare_keys : 'e t -> 'e t -> int
  val compare : ('e -> 'e -> int) -> 'e t -> 'e t -> int
  val to_string : ('e -> string) -> 'e t -> string

  val gen2 : 
    (?size:int -> Random.State.t -> key) -> 
    (?size:int -> Random.State.t -> 'a) -> 
    ?size:int -> Random.State.t -> 'a t
end

module type MonoKeyMapSigStd = MonoKeyMapSig with type ('a,'v) result = 'a

module type MonoKeyMapSigFnStd = 
  functor(C : Types.Mono.Comparable) ->
    MonoKeyMapSigStd with type key = C.t

module type GenKeyMapSig = sig
  include MonoKeyMapSig
  val gen1 : (?size:int -> Random.State.t -> 'e) -> ?size:int -> 
    Random.State.t -> 'e t
end    

module type GenKeyMapSigStd = GenKeyMapSig with type ('a,'v) result = 'a

module type GenKeyMapSigFnStd = 
  functor(C : Types.Mono.ArbitraryComparable) ->
    GenKeyMapSigStd with type key = C.t

module type MonoMapSig = sig
  type t
  type key
  type elt
  type cursor
  type 'a result

  include Map_
    with type 'k key_ = key
    and type 'e elt_ = elt
    and type ('k,'e) map = t
    and type ('k,'e) cursor_ = cursor
    and type ('a,'k,'v) result_ = 'a result

  val compare_keys : t -> t -> int
  val compare : t -> t -> int
  val to_string : t -> string

  val gen2 : 
    (?size:int -> Random.State.t -> key) -> 
    (?size:int -> Random.State.t -> elt) -> 
    ?size:int -> Random.State.t -> t

end  

module type MonoMapSigFn = 
  functor(K : Types.Mono.Comparable) ->
    functor(V : Types.Mono.Comparable) ->
      MonoMapSig with type key = K.t and type elt = V.t

module type MonoMapSigFnStd = 
  functor(K : Types.Mono.Comparable) ->
    functor(V : Types.Mono.Comparable) ->
      MonoMapSig with type key = K.t and type elt = V.t
		 and type 'a result = 'a

module type GenMapSig = sig
  include MonoMapSig
  val gen : ?size:int -> Random.State.t -> t
end
  
module type GenMapSigFn = 
  functor(K : Types.Mono.ArbitraryComparable) ->
    functor(V : Types.Mono.ArbitraryComparable) ->
      GenMapSig with type key = K.t and type elt = V.t

module type GenMapSigFnStd = 
  functor(K : Types.Mono.ArbitraryComparable) ->
    functor(V : Types.Mono.ArbitraryComparable) ->
      GenMapSig with type key = K.t and type elt = V.t
		and type 'a result = 'a
