(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

let to_string iter pop to_s t = 
  try
    let buf = Buffer.create 32 in
    let hd,tl = pop t in
    Buffer.add_char buf '[';
      Buffer.add_string buf (to_s hd);
      iter (fun e -> 
	Buffer.add_string buf "; "; 
	Buffer.add_string buf (to_s e)
	   ) tl;
      Buffer.add_char buf ']';
      Buffer.contents buf
  with Failure(s) when s="pop" -> "[]"
