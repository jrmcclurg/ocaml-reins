(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

module type S = sig

  type ('v,'t) generator
      (** functions that return a container and none of its arguments
	  are containers *)

  type ('v,'t) mutator
      (** functions that return a container and at least one arg is a
	  container *)

  type ('v,'t) observer
      (** functions that do not return a container, but takes one as
	  an argument *)

  type ('v,'t) op
      (** One of {generator,mutator,observer} *)

  val op_to_string : ('v,'t) op -> string

  val coerce_gen : ('v,'t) generator -> ('v,'t) op
  val coerce_mut : ('v,'t) mutator -> ('v,'t) op
  val coerce_obs : ('v,'t) observer -> ('v,'t) op

  val classify : ('v,'t) op -> 
    (('v,'t) generator,('v,'t) mutator,('v,'t) observer) Dug.kind
  val strip : ('v,'t) op -> (unit,unit) op

  val op_dependencies : ('a,Dug.Id.t) op -> Dug.Id.t list
  val create_op : 
    (unit,unit) op -> Dug.Id.t
    -> (unit -> 'a) -> (int -> Dug.Id.t) -> ('a,Dug.Id.t) op

end

