(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** The signature for an iterator over a List. *)
module type S =
sig
  
  (** List iterators support only two directions.  [Left_Right]
      iterates through the list in the forward direction, visiting the
      head of the list before the tail.  [Right_Left] is the opposite.
      It iterates through all elements in the tail before visiting the
      head. *)
  type direction_ = 
    | Left_Right
    | Right_Left

  include Iterator.S with type direction = direction_
end

(** Create a list iterator from an arbitrary cursor type *)
module Make :
  functor (I : ListCursor.S) ->
    S with type 'a collection = 'a I.list_
      and type 'a cursor = 'a I.cursor
      and type 'a elt = 'a 

(** Create a list iterator for the list [L] using the standard
    List_Cursor interface for the cursor. *)
module From_List : 
  functor (L : Lists.ListSig) -> 
    S with type 'a collection = 'a L.t
      and type 'a elt = 'a
      and type 'a cursor = 'a ListCursor.Make(L).cursor



