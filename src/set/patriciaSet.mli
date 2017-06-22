(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Efficient sets of integers 

    Patricia trees are balanced binary search trees whose elements are
    integers.  These trees can be very efficient since navigating the
    each tree uses only specific bits of the elements value.  They
    have O(w) worst case running time for the [mem], [add], [remove]
    where w is the number of bits in an integer, but typically run in
    O(log n) time for most inputs.  Because, Patricia trees never need
    to be re-balanced, [union], [inter], and [diff] can be much faster
    than ordinary balanced trees, but still may take O(n+m) in the
    worst case.
*)

(** This module implements sets with integer keys *)
module MonoSet : Sets.MonoSetSig with type elt = int
				 and type 'a result = 'a
  

(** Same as the {!PatriciaSet.MonoSet} module, except it also provides
    the [gen] function.
*)
module GenSet : Sets.GenSetSig with type elt = int
			       and type 'a result = 'a

