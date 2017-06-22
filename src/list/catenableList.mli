(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Lists with fast concatenation.  
    
    This module implements lists with amortized O(1) [hd], [tl], and
    [append] operations.
*)

type 'a t

val empty : 'a t
  (** The empty list *)

val is_empty : 'a t -> bool
  (** returns true if the list is empty *)

val length : 'a t -> int
  (** [length t] Returns the length of the list [t].  Runs in O(n)
      time and O(1) stack space *)

val rev : 'a t -> 'a t
  (** [rev t] Returns the list [t] in reversed order.  Runs in O(n)
      time and O(1) stack space. *)

val hd : 'a t -> 'a
  (** [hd t] Return the element at the front of the list.  O(1) time and
      stack space.  If the list is empty, it raises [Failure "hd"].  *)

val tl : 'a t -> 'a t
  (** [tl t] Return the tail of the list (the list with the first element
      removed).  This operation runs in amortized O(1) time and stack
      space.  If the list is empty, it raises [Failure "tl"].  *)

val pop : 'a t -> 'a * 'a t
  (** [pop t] Equivalent to [(hd t), (tl t)] but is more efficient.
      Runs in amortized O(1) time and stack space.  If the list is
      empty, it raises [Failure "pop"].  *)

val cons : 'a -> 'a t -> 'a t
  (** [cons x t] Adds [x] onto the front of the list [t].  Runs in
      amortized O(1) time and stack space. *)

val snoc : 'a -> 'a t -> 'a t
  (** [snoc t x] Adds [x] onto the back of the list [t].  Runs in
      amortized O(1) time and stack space. *)

val last : 'a t -> 'a
  (** [last t] Returns the element at the back of the list.  If the
      list is empty, it raises [Failure "last"].  Runs in O(1) stack
      and O(n) time, but may be more efficient in some circumstances
      when t has been constructed with several concatenations.  *)

val append : 'a t -> 'a t -> 'a t
  (** [append t1 t2] Appends the list [t2] onto the back of list [t1].
      Runs in amortized O(1) time and stack space. *)

val flatten : 'a t t -> 'a t
  (** [flatten l] Appends all of the elements of [l] into a new list.
      Runs in amortized O(n) time and amortized O(1) stack space where
      n is the length of [l]. *)

val from_list : 'a list -> 'a t
  (** [from_list l] Convert the standard list l into a CatenableList.
      Runs in O(n) time and O(1) stack space where n is the number of
      elements in [l].  *)

val to_list : 'a t -> 'a list
  (** [to_list t] Convert the CatenableList [t] into a standard list.
      Runs in O(n) time and O(1) stack space where n is the number of
      elements in [t].  *)

val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f t] Iterates over each element in the list [t] in order
      and applies [f] to that element.  Runs in O(n*ft) where ft is
      the running time of [f] and uses O(fs) stack space where fs is
      the stack space required by [f].  *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold f acc t] Accumulates the result [acc] by applying [f acc
      x] for each element [x] in [t].  Runs in O(n*ft) where ft is the
      running time of [f] and uses O(fs) stack space where fs is the
      stack space required by [f]. *)

val rev_map : ('a -> 'b) -> 'a t -> 'b t
  (** [rev_map f t] Creates a new list by applying [f] to each element
      of [t].  The resulting list is in reverse order of [t].  Runs in
      O(n*ft) time where n is the number of elements in [t] and ft is
      the running time of [f].  It uses O(fs) stack space where fs is
      the stack space required by [f]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f t] Creates a new list by applying [f] to each element of
      [t].  The resulting list is in the same order as [t].  Runs in
      O(n*ft) time where n is the number of elements in [t] and ft is
      the running time of [f].  It uses O(fs) stack space where fs is
      the stack space required by [f].  This function is just as
      efficient as {!CatenableList.rev_map} (yielding a different
      ordering) and more efficient than [CatenableList.rev
      (CatenableList.rev_map t)]. *)

val to_string : ('a -> string) -> 'a t -> string
  (** [to_string to_s t] Convert the list [t] into a string using
      [to_s] to individually convert each element into a string.  Runs
      in O(n*st) where st is the running time of [to_s] and uses O(ss)
      stack space where ss is the amount of stack required by [to_s].
  *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  (** [compare f t1 t2] Compares the lists [t1] and [t2] using [f] to
      compare individual elements.  Returns 0 if [t1] and [t2] are
      equal (under f).  Returns [<0] if [t1] is less than [t2] and
      returns [>0] otherwise.  *)

val gen : (?size:int -> Random.State.t -> 'a) -> ?size:int ->
  Random.State.t -> 'a t
  (** [gen f ?size rs] Generates a random list whose length is bounded
      by [size].  Each element in the list is computed by calling [f
      ?size rs].  Runs in time O([size] * ft) where ft is the running
      time of [f] and uses O(fs) stack space where fs is the stack space
      of [f].
  *)

