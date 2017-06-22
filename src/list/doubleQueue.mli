(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Double ended queues *)

type 'a t
  (** The type of double ended queues.  Access to both the front and
      the back of the queue take amortized O(1) time.  *)

val empty : 'a t
  (** The empty queue *)

val is_empty : 'a t -> bool
  (** Returns true is the queue is empty *)

val hd : 'a t -> 'a
  (** [hd q] Return the element at the front of the queue.  If the
      queue is empty, it raises [Failure "hd"] *)

val tl : 'a t -> 'a t
  (** [tl t] Return the queue [t] with the element at the front of the
      queue removed.  Runs in O(1) time and stack space.  If the queue
      is empty, it raises [Failure "tl"].  *)

val pop : 'a t -> 'a * 'a t
  (** [pop t] Equivalent to [(hd t), (tl t)] but is more efficient.
      Runs in O(1) time and stack space.  If the queue is empty, it
      raises [Failure "pop"].  *)

val cons : 'a -> 'a t -> 'a t
  (** [cons x t] Adds [x] to the front of queue [t] so that a
      subsequent call to [hd] returns [x].  Runs in O(1) time and
      stack space. *)

val hd_back : 'a t -> 'a
  (** [hd_back q] Return the element at the back of the queue.  If the
      queue is empty, it raises [Failure "hd_back"].  Runs in
      amortized O(1) time and O(1) stack space. *)

val tl_back : 'a t -> 'a t
  (** [tl t] Return the queue [t] with the element at the back of the
      queue removed.  Runs in amortized O(1) time and O(1) stack
      space.  If the queue is empty, it raises [Failure "tl_back"].
  *)

val pop_back : 'a t -> 'a t * 'a
  (** [pop_back t] Equivalent to [(hd_back t), (tl_back t)] but is
      more efficient.  Runs in amortized O(1) time and O(1) stack
      space.  If the queue is empty, it raises [Failure "pop_back"].
  *)

val cons_back : 'a -> 'a t -> 'a t
  (** [cons_back x t] Adds [x] to the back of queue [t] so that a
      subsequent call to [hd_back] returns [x].  Runs in O(1) time and
      stack space. *)

val snoc : 'a -> 'a t -> 'a t
  (** [snoc x t] is an alias for {!DoubleQueue.cons_back} [x t],
      adding [x] to the back of [t].  *)

val last : 'a t -> 'a
  (** [last q] is an alias for [hd_back q] *)

val enqueue : 'a -> 'a t -> 'a t
  (** [enqueue x t] is an alias for {!DoubleQueue.cons_back} [x t],
      adding [x] to the back of [t]. *)

val dequeue : 'a t -> 'a * 'a t
  (** [dequeue x t] is an alias for {!DoubleQueue.hd} [x t], removing
      the first element from the front of [t]. *)

val length : 'a t -> int
  (** [length t] Returns the number of elements in the queue [t] *)

val rev : 'a t -> 'a t
  (** [rev t] Reverses the order of the queue [t].  e.g., [hd t ==
      hd_back (rev t)] *)

val append : 'a t -> 'a t -> 'a t
  (** [append t1 t2] Appends all of the elements in queue [t2] onto
      the back of [t1].  That is, in the resulting queue,
      {!DoubleQueue.hd} returns the first element of [t1] and
      {!DoubleQueue.hd_back} returns the last element of [t2].  Runs
      in O(n+m) time where n and m are the number of elements in [t1]
      and [t2] respectively.  *)

val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f t] Iterates over each element in the queue [t] in order
      and applies [f] to that element.  Runs in O(n*ft) where ft is
      the running time of [f] and uses O(fs) stack space where fs is
      the stack space required by [f].  *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold f acc t] Accumulates the result [acc] by applying [f acc
      x] for each element [x] in [t].  Runs in O(n*ft) where ft is the
      running time of [f] and uses O(fs) stack space where fs is the
      stack space required by [f]. *)

val rev_map : ('a -> 'b) -> 'a t -> 'b t
  (** [rev_map f t] Creates a new queue by applying [f] to each element
      of [t].  The resulting queue is in reverse order of [t].  Runs in
      O(n*ft) time where n is the number of elements in [t] and ft is
      the running time of [f].  It uses O(fs) stack space where fs is
      the stack space required by [f]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f t] Creates a new queue by applying [f] to each element of
      [t].  The resulting queue is in the same order as [t].  Runs in
      O(n*ft) time where n is the number of elements in [t] and ft is
      the running time of [f].  It uses O(fs) stack space where fs is
      the stack space required by [f].  This function is just as
      efficient as {!DoubleQueue.rev_map} (yielding a different
      ordering) and more efficient than [DoubleQueue.rev
      (DoubleQueue.rev_map t)]. *)

val to_list : 'a t -> 'a list
  (** [to_list t] Convert the DoubleQueue [t] into a standard list.
      Runs in O(n) time and O(1) stack space where n is the number of
      elements in [t].  The resulting list has the same ordering as
      [t].  That is, [DoubleQueue.hd t == List.hd (DoubleQueue.to_list
      t)]. *)

val from_list : 'a list -> 'a t
  (** [from_list l] Convert the standard list l into a DoubleQueue.t.
      Runs in O(n) time and O(1) stack space where n is the number of
      elements in [l].  The resulting queue has the same order as the
      original list.  That is [List.hd l == DoubleQueue.hd
      (DoubleQueue.from_list l)].  *)

val flatten : 'a t t -> 'a t
  (** [flatten l] Appends all of the elements of [l] into a new queue.
      The current implementation is not very efficient and runs in
      greater than O(n) time uses a O(n) stack space.  *)

val to_string : ('a -> string) -> 'a t -> string
  (** [to_string to_s t] Convert the queue [t] into a string using
      [to_s] to individually convert each element into a string.  Runs
      in O(n*st) where st is the running time of [to_s] and uses O(ss)
      stack space where ss is the amount of stack required by [to_s].
  *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  (** [compare f t1 t2] Compares the queues [t1] and [t2] using [f] to
      compare individual elements.  Returns 0 if [t1] and [t2] are
      equal (under f).  Returns [<0] if [t1] is less than [t2] and
      returns [>0] otherwise.  *)

val gen : (?size:int -> Random.State.t -> 'a) -> ?size:int -> Random.State.t -> 'a t
  (** [gen f ?size rs] Generates a random queue whose length is
      bounded by [size].  Each element in the queue is computed by
      calling [f ?size rs].  Runs in time O([size] * ft) where ft is
      the running time of [f] and uses O(fs) stack space where fs is
      the stack space of [f].  *)

