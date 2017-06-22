(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

module type S = sig

  type direction_ = 
    | Left_Right
    | Right_Left

  include Iterator.S with type direction = direction_

end

module Base(I : ListCursor.S) = struct
  type 'a elt = 'a
  type 'a cursor = 'a I.cursor
  type 'a collection = 'a I.list_

  type direction_ = 
      | Left_Right
      | Right_Left

  type direction = direction_

  let flip_dir = function
    | Right_Left -> Left_Right
    | Left_Right -> Right_Left

  let has_curs_value curs = match I.value curs with
    | None -> false
    | Some _ -> true

  let get_curs_value curs = match I.value curs with
    | None -> assert false
    | Some x -> x

  let has_more_elements dir curs = match dir with
    | Right_Left -> not (I.at_front curs)
    | Left_Right -> not (I.at_back curs)

  let move_cursor_next_element dir curs = match dir with
    | Right_Left -> I.move_prev curs
    | Left_Right -> I.move_next curs

end


module Make(I : ListCursor.S) 
  : S with type 'a collection = 'a I.list_
      and type 'a cursor = 'a I.cursor
      and type 'a elt = 'a 
= struct
  
  (* Can't include the Base code in this module (and make it a module
     rec) since the type checker does not support instantiating
     recursive functors.  It gives "Cannot safely evaluate the
     definition of the recursively-defined module" *)

  include Base(I)
  include IteratorMixin.Mixin(Base(I))

  let create dir trav l = from_cursor dir trav (I.to_cursor l)

end

module From_List(L : Lists.ListSig) 
  : S with type 'a elt = 'a 
      and type 'a cursor = 'a ListCursor.Make(L).cursor
      and type 'a collection = 'a L.t
  = Make(ListCursor.Make(L))

