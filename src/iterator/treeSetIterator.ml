(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(* CR SW: again, the capitalization of the filename seems weird.  Why is
   "tree" lowercase but "Iterator" uppercase? *)

module type S = sig
  type  ordering = 
      | PreOrder
      | InOrder
      | PostOrder

  type direction_ = 
    | Ascending of ordering
    | Descending of ordering  

  include Iterator.S with type direction = direction_
  
end

module Base(T : Sets.Set_) = struct
  type 'a elt = 'a T.elt_
  type 'a cursor = 'a T.cursor_
  type 'a collection = 'a T.set
      
  type ordering = 
      | PreOrder  (* root, left, right *)
      | InOrder   (* left, root, right *)
      | PostOrder (* left, right, root *)
	  
  type direction_ = 
      | Ascending of ordering
      | Descending of ordering

  type direction = direction_

  (* move to the bottom, left most node in the tree*)
  let rec move_bottom_left curs = 
    if T.at_left curs then curs
    else move_bottom_left (T.move_down_left curs)

  (* move to the bottom, right most node in the tree*)
  let rec move_bottom_right curs = 
    if T.at_right curs then curs
    else move_bottom_right (T.move_down_right curs)

  (* walk up the tree looking for the last branch where we went left
  *) 
  let rec find_left curs = 
    if T.at_top curs then raise Exit
    else if T.went_left curs then curs
    else find_left (T.move_up curs)

  (* walk up the tree looking for the last branch where we went
     right *)
  let rec find_right curs = 
    if T.at_top curs then raise Exit
    else if T.went_right curs then curs
    else find_right (T.move_up curs)
      
  let rec move_inorder curs = (* left root right *)
    if T.at_right curs then T.move_up (find_left curs)
    else move_bottom_left (T.move_down_right curs)

  let rec move_inorder_rev curs = (* right root left *)
    if T.at_left curs then T.move_up (find_right curs)
    else move_bottom_right (T.move_down_left curs)

  let rec move_preorder curs = (* root left right *)
    if T.at_left curs
    then T.move_down_right (T.move_up (find_left curs))
    else T.move_down_left curs

  let rec move_preorder_rev curs = (* right left root *)
    if T.went_right curs 
    then move_bottom_right (T.move_down_left (T.move_up curs))
    else if T.went_left curs then T.move_up curs
    else raise Exit

  let rec move_postorder curs = (* left right root *)
    if T.went_left curs 
    then move_bottom_left (T.move_down_right (T.move_up curs))
    else if T.went_right curs then T.move_up curs
    else raise Exit

  let rec move_postorder_rev curs = (* root right left *)
    if T.at_right curs
    then T.move_down_left (T.move_up (find_right curs))
    else T.move_down_right curs

  let rec move_cursor_next_element dir curs = 
    let curs = match dir with
      | Ascending PreOrder   -> move_preorder curs
      | Ascending InOrder    -> move_inorder curs
      | Ascending PostOrder  -> move_postorder curs
      | Descending PreOrder  -> move_preorder_rev curs
      | Descending InOrder   -> move_inorder_rev curs
      | Descending PostOrder -> move_postorder_rev curs
    in 
      if T.has_value curs then curs 
      else move_cursor_next_element dir curs

  let has_more_elements dir curs = 
    try ignore(move_cursor_next_element dir curs); true
    with Exit -> false
      
  let flip_dir = function
    | Ascending PreOrder   -> Descending PostOrder
    | Ascending InOrder    -> Descending InOrder
    | Ascending PostOrder  -> Descending PreOrder
    | Descending PreOrder  -> Ascending PostOrder
    | Descending InOrder   -> Ascending InOrder
    | Descending PostOrder -> Ascending PreOrder

  let has_curs_value = T.has_value
  let get_curs_value = T.get_value

end

module Make(T : Sets.Set_) 
  : S with type 'a elt = 'a T.elt_
      and type 'a cursor = 'a T.cursor_
      and type 'a collection = 'a T.set
= struct

  include Base(T)
  include IteratorMixin.Mixin(Base(T))

  let has_value t = has_curs_value t.curs
  let get_value t = get_curs_value t.curs

  let create dir trav t = 
    (* create the cursor at the top of the tree *)
    let curs = T.to_cursor t in 
      (* Move the cursor to the starting location for the traversal *)
    let curs = match dir with
      | Ascending PreOrder  -> curs
      | Ascending InOrder   -> move_bottom_left curs
      | Ascending PostOrder -> move_bottom_left curs
      | Descending PreOrder  -> move_bottom_right curs
      | Descending InOrder   -> move_bottom_right curs
      | Descending PostOrder -> curs
    in
      from_cursor dir trav curs 
end

