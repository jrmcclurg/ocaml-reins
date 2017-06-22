(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Datatype Usage Graphs *)

module Id : Types.Integral

type ('gen,'mut,'obs) kind = 
  | Generator of 'gen
  | Mutator of 'mut
  | Observer of 'obs

type ('gen,'mut,'obs) edge = {
    target : Id.t;
    op : ('gen,'mut,'obs) kind;
    pos : int;
  }

type ('gen,'mut,'obs) t = {
    mutable current_id : Id.t;
    nodes : (Id.t,('gen,'mut,'obs) kind) Hashtbl.t;
    edges : (Id.t,('gen,'mut,'obs) edge) Hashtbl.t;
  }

val create : unit -> ('gen,'mut,'obs) t

val clear : ('gen,'mut,'obs) t -> unit

val size : ('eng,'mut,'obs) t -> Id.t

val fresh_id : ('gen,'mut,'obs) t -> Id.t

val is_mutator   : ('gen,'mut,'obs) kind -> bool
val is_generator : ('gen,'mut,'obs) kind -> bool
val is_observer  : ('gen,'mut,'obs) kind -> bool

