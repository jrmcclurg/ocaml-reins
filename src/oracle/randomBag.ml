(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

include SkewBinaryList

module RCurs = ListCursor.Make(SkewBinaryList)

let add = cons

let remove x t = 
  let rec helper c = 
    if RCurs.at_back c then failwith "remove";
    match RCurs.value c with
      | None -> helper (RCurs.move_next c)
      | Some y -> 
	  if x = y then
	    let l = RCurs.list c in
	    let c = RCurs.replace_list (SkewBinaryList.tl l) c in
	      RCurs.from_cursor c
	  else helper (RCurs.move_next c)
  in
    helper (RCurs.to_cursor t)


let choose t = 
  if is_empty t then failwith "choose";
  let l = SkewBinaryList.length t in
  let idx = Random.int l in
    lookup idx t
