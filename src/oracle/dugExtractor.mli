(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Helper module for extracting a DUG from a specific program execution *)

type 'a wrap = private { data : 'a; id : Dug.Id.t; }

val mutate : ('gen,'mut,'obs) Dug.t -> 
  (Dug.Id.t -> 'mut) -> ('a -> 'a) -> 'a wrap -> 'a wrap

val mutate2 : ('gen,'mut,'obs) Dug.t -> 
  (Dug.Id.t -> 'mut) -> ('a -> 'a -> 'a) -> 'a wrap -> 'a wrap -> 'a wrap

val observe : ('gen,'mut,'obs) Dug.t -> 
  'obs -> ('a -> 'b) -> 'a wrap -> 'b
  
val observe2 : ('gen,'mut,'obs) Dug.t -> 
  'obs -> ('a -> 'a -> 'b) -> 'a wrap -> 'a wrap -> 'b

val generate : ('gen,'mut,'obs) Dug.t -> 
  (Dug.Id.t -> 'gen) -> 'a -> 'a wrap

