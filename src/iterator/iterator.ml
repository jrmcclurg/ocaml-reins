(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

module type S = sig

  type 'a t
  type 'a elt
  type 'a cursor
  type 'a collection
      
  type direction
      
  type 'a traversal =
      | Traverse_All
      | Traverse_If of ('a -> bool)
      | Traverse_While of ('a -> bool)


  val create : direction -> 'a elt traversal -> 'a collection -> 'a t
  val from_cursor : direction -> 'a elt traversal -> 'a cursor -> 'a t

  val value : 'a t -> 'a elt option
  val get_value : 'a t -> 'a elt

  val at_end : 'a t -> bool
  val at_beg : 'a t -> bool

  val has_next : 'a t -> bool
  val next : 'a t -> 'a t

  val has_prev : 'a t -> bool
  val prev : 'a t -> 'a t

  val goto_beg : 'a t -> 'a t
  val goto_end : 'a t -> 'a t

  val flip : 'a t -> 'a  t

  val iter : ('a elt -> unit) -> 'a t -> unit
  val fold : ('a -> 'b elt -> 'a) -> 'a -> 'b t -> 'a

end
