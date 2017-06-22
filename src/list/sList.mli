(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

type 'a t = 'a list

val empty : 'a list
  (** The empty list.  aka [] *)

val is_empty : 'a list -> bool
  (** Returns true if the list is empty *)

val cons : 'a -> 'a list -> 'a list
  (** [cons x t] Adds [x] onto the front of the list [t].  Runs in
      O(1) time and stack space. *)

val pop : 'a list -> 'a * 'a list
  (** [pop t] equivalent to [(hd t), (tl t)] but is more efficient.
      Runs in amortized O(1) time and stack space.  If the list is
      empty, it raises [Failure "pop"].
 *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  (** [fold f acc l] Equivalent to [fold_left f acc l] *)

val snoc : 'a -> 'a list -> 'a list
  (** [snoc x t] Adds the element [x] to the back of the list [t].
      Runs in O(n) time and O(1) stack space where n is the length of
      the list.  *)

val last : 'a t -> 'a
  (** [last t] Returns the element at the back of the list.  If the
      list is empty, it raises [Failure "last"].  Runs in O(1) stack
      and O(n) time.  *)

val to_list : 'a -> 'a
  (** [to_list t]  Included for compatibility with the common ListSig
      signature.  This function does not perform any computation.
  *)

val from_list : 'a -> 'a
  (** [from_list t] Included for compatibility with the common ListSig
      signature.  This function does not perform any computation.  *)

val to_string : ('a -> string) -> 'a list -> string
  (** [to_string to_s t] Convert the list [t] into a string using
      [to_s] to individually convert each element into a string.  Runs
      in O(n*st) where st is the running time of [to_s] and uses O(ss)
      stack space where ss is the amount of stack required by [to_s].
      *)

val compare : ('a -> 'b -> int) -> 'a list -> 'b list -> int
  (** [compare f t1 t2] Compares the lists [t1] and [t2] using [f] to
      compare individual elements.  Returns 0 if [t1] and [t2] are
      equal (under f).  Returns [<0] if [t1] is less than [t2] and
      returns [>0] otherwise.  *)

val gen :
  (?size:int -> Random.State.t -> 'a) ->
  ?size:int -> Random.State.t -> 'a list
  (** [gen f ?size rs] Generates a random list whose length is bounded
      by [size].  Each element in the list is computed by calling [f
      ?size rs].  Runs in time O([size] * ft) where ft is the running
      time of [f] and uses O(fs) stack space where fs is the stack space
      of [f].
  *)


(** The following are all implemented in the standard library *)


val length : 'a list -> int
val hd : 'a list -> 'a
val tl : 'a list -> 'a list
val nth : 'a list -> int -> 'a
val rev : 'a list -> 'a list
val append : 'a list -> 'a list -> 'a list
val rev_append : 'a list -> 'a list -> 'a list
val concat : 'a list list -> 'a list
val flatten : 'a list list -> 'a list
val iter : ('a -> unit) -> 'a list -> unit
val map : ('a -> 'b) -> 'a list -> 'b list
val rev_map : ('a -> 'b) -> 'a list -> 'b list
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
val for_all : ('a -> bool) -> 'a list -> bool
val exists : ('a -> bool) -> 'a list -> bool
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val mem : 'a -> 'a list -> bool
val memq : 'a -> 'a list -> bool
val find : ('a -> bool) -> 'a list -> 'a
val filter : ('a -> bool) -> 'a list -> 'a list
val find_all : ('a -> bool) -> 'a list -> 'a list
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
val assoc : 'a -> ('a * 'b) list -> 'b
val assq : 'a -> ('a * 'b) list -> 'b
val mem_assoc : 'a -> ('a * 'b) list -> bool
val mem_assq : 'a -> ('a * 'b) list -> bool
val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
val split : ('a * 'b) list -> 'a list * 'b list
val combine : 'a list -> 'b list -> ('a * 'b) list
val sort : ('a -> 'a -> int) -> 'a list -> 'a list
val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
