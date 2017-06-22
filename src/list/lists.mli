(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** The signature that all lists must minimally conform to. *)

module type ListSig = sig
  type 'a t
      (** The type of the list *)

  val empty : 'a t
    (** The empty list *)
    
  val is_empty : 'a t -> bool
    (** Returns true if the list is empty *)

  val length : 'a t -> int
    (** Returns the length of the list *)

  val rev : 'a t -> 'a t
    (** Reverse the list *)

  val cons : 'a -> 'a t -> 'a t
    (** [cons x t] Add the element [x] to the front of list [t] *)
    
  val snoc : 'a -> 'a t -> 'a t
    (** [snoc x t] Add the element [x] to the end of list [t] *)

  val hd : 'a t -> 'a
    (** [hd t] Return the first element at the front of the list.  All
	lists in the Reins library raise [Failure "hd"] when applied
	to an empty list. *)

  val tl : 'a t -> 'a t
    (** [tl t] Return the list with the first element removed.  All
	lists in the Reins library raise [Failure "tl"] when applied
	to an empty list. *)

  val pop : 'a t -> 'a * 'a t
    (** Returns both the first element of the list and the remaining
	tail of the list.  All lists in the Reins library raise
	[Failure "pop"] when applied to an empty list. *)

  val last : 'a t -> 'a
    (** [last t] Returns the element at the back of the list.  All
	lists in the Reins library raise [Failure "last"] when applied
	to an empty list. *)

  val append : 'a t -> 'a t -> 'a t
    (** [append t1 t2]  Append the list [t2] onto the end of list
	[t1].  *)

  val flatten : 'a t t -> 'a t
    (** Flatten a list of lists into a single list *)
    
  val from_list : 'a list -> 'a t
    (** Create a list from a builtin list type *)

  val to_list : 'a t -> 'a list
    (** Convert the list into a builtin list type *)
    
  val iter : ('a -> unit) -> 'a t -> unit
    (** [iter f t] Apply [f] to each element in list [t]. *)

  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    (** [fold f acc t] Accumulates the result [acc] by applying [f acc
	x] for each element [x] in [t].   *)

  val rev_map : ('a -> 'b) -> 'a t -> 'b t
    (** [rev_map f t] Creates a new list by applying [f] to each
	element of [t].  The resulting list is in reverse order of
	[t].  *)

  val map : ('a -> 'b) -> 'a t -> 'b t
    (** [map f t] Creates a new list by applying [f] to each element
	of [t].  The resulting list is in the same order as [t].  *)

  val to_string : ('a -> string) -> 'a t -> string
    (** [to_string to_s t] Convert the list [t] into a string using
	[to_s] to individually convert each element into a string.
	All lists in the Reins library format the list following OCaml
	syntax.  e.g., "[x1; x2; x3]"
    *)

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** [compare f t1 t2] Compares the lists [t1] and [t2] using [f]
	to compare individual elements.  Returns 0 if [t1] and [t2]
	are equal (under f).  Returns [<0] if [t1] is less than [t2]
	and returns [>0] otherwise.  *)

  val gen :
    (?size:int -> Random.State.t -> 'a) ->
    ?size:int -> Random.State.t -> 'a t
    (** [gen f ?size rs] Generates a random list whose length is bounded
	by [size].  Each element in the list is computed by calling [f
	?size rs].
    *)

end

