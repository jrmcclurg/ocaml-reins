(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

module type ListSig = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val length : 'a t -> int

  val rev : 'a t -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val snoc : 'a -> 'a t -> 'a t

  val hd : 'a t -> 'a
  val tl : 'a t -> 'a t
  val pop : 'a t -> 'a * 'a t
  val last : 'a t -> 'a
  val append : 'a t -> 'a t -> 'a t
  val flatten : 'a t t -> 'a t
  val from_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val rev_map : ('a -> 'b) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val to_string : ('a -> string) -> 'a t -> string
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val gen :
    (?size:int -> Random.State.t -> 'a) ->
    ?size:int -> Random.State.t -> 'a t
end

