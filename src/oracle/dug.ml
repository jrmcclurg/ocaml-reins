(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)



module Id = Types.Int64

type ('a,'b,'c) kind = 
    | Generator of 'a
    | Mutator of 'b
    | Observer of 'c

type ('a,'b,'c) edge = {
    target : Id.t;
    op : ('a,'b,'c) kind;
    pos : int;
  }

type ('a,'b,'c) t = {
    mutable current_id : Id.t;
    nodes : (Id.t,('a,'b,'c) kind) Hashtbl.t;
    edges : (Id.t,('a,'b,'c) edge) Hashtbl.t;
  }

let fresh_id t = 
  t.current_id <- Id.succ t.current_id;
  t.current_id

let create () = 
  {current_id = Int64.zero;
   nodes = Hashtbl.create 127;
   edges = Hashtbl.create 229}

let clear t = 
  t.current_id <- Int64.zero;
  Hashtbl.clear t.nodes;
  Hashtbl.clear t.edges

let size t = t.current_id

let is_mutator = function
  | Mutator _ -> true
  | Generator _
  | Observer _ -> false

let is_generator = function
  | Generator _ -> true
  | Mutator _
  | Observer _ -> false

let is_observer = function
  | Observer _ -> true
  | Generator _
  | Mutator _ -> false

let _ = 
  Random.self_init ()
