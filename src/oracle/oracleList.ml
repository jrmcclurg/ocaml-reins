(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)


type ('v,'t) generator = [
  | `Empty of 't
  | `Gen of 't
  | `From_list of 't * 'v list
]

type ('v,'t) mutator = [
  | `Rev of 't * 't
  | `Cons of 't * 'v * 't
  | `Snoc of 't * 'v * 't 
  | `Tl of 't * 't
  | `Append of 't * 't * 't
  | `Rev_map of 't * 't
  | `Map of 't * 't
]

type ('v,'t) observer = [
  | `Is_empty of 't
  | `Length of 't
  | `Hd of 't
  | `To_string of 't
  | `To_list of 't
  | `Compare of 't * 't
  | `Iter of 't
  | `Fold of 't
]

(*
  val pop : 'a t -> 'a * 'a t
  val flatten : 'a t t -> 'a t

  val rev_map : ('a -> 'b) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
*)


type ('v,'t) op = [
  | ('v,'t) generator
  | ('v,'t) mutator
  | ('v,'t) observer
]

let coerce_gen x = (x :> ('v,'t) op)
let coerce_mut x = (x :> ('v,'t) op)
let coerce_obs x = (x :> ('v,'t) op)

let classify = function
  | #generator as o -> Dug.Generator o
  | #mutator as o -> Dug.Mutator o
  | #observer as o -> Dug.Observer o

let op_to_string : ('v,'t) op -> string = function
  | `Empty _ -> "empty"
  | `Gen _ -> "gen"
  | `From_list _ -> "from_list"
  | `Rev _ -> "rev"
  | `Cons _ -> "cons"
  | `Snoc _ -> "snoc"
  | `Tl _ -> "tl"
  | `Append _ -> "append"
  | `Is_empty _ -> "is_empty"
  | `Length _ -> "length"
  | `Hd _ -> "hd"
  | `To_string _ -> "to_string"
  | `To_list _ -> "to_list"
  | `Compare _ -> "compare"
  | `Iter _ -> "iter"
  | `Fold _ -> "fold"
  | `Rev_map _ -> "rev_map"
  | `Map _ -> "map"
    
let op_dependencies : ('a,Dug.Id.t) op -> Dug.Id.t list = function
  | `Empty _
  | `Gen _
  | `From_list _ -> []
  | `Rev(_,t)
  | `Cons(_,_,t)
  | `Snoc(_,_,t) 
  | `Tl(_,t)
  | `Is_empty t
  | `Length t
  | `Hd t
  | `Rev_map(_,t)
  | `Map(_,t)
  | `Iter t
  | `Fold t
  | `To_list t
  | `To_string t  -> [t]
  | `Append(_,t1,t2)
  | `Compare(t1,t2) -> [t1;t2]

let create_op uop id elt_f t_f = match uop with
  | `Empty _ -> `Empty (id)
  | `Gen _ -> `Gen(id)
  | `From_list _ -> assert false
  | `Rev _ -> `Rev(id,t_f 0)
  | `Cons _ -> `Cons(id,elt_f(),t_f 0)
  | `Snoc _ -> `Snoc(id,elt_f(),t_f 0)
  | `Tl _ -> `Tl(id,t_f 0)
  | `Append _ -> `Append(id,t_f 0, t_f 1)
  | `Is_empty _ -> `Is_empty(t_f 0)
  | `Length _ -> `Length(t_f 0)
  | `Hd _ -> `Hd(t_f 0)
  | `Iter _ -> `Iter(t_f 0)
  | `Fold _ -> `Fold(t_f 0)
  | `To_list _ -> `To_list(t_f 0)
  | `To_string _ -> `To_string(t_f 0)
  | `Compare _ -> `Compare(t_f 0,t_f 1)
  | `Rev_map _ -> `Rev_map(id,t_f 0)
  | `Map _ -> `Map(id,t_f 0)

let strip op = assert false (*create_op op () (fun () -> ()) (fun _ -> ())*)

(*
module Extractor(A : MonoTypes.ArbitraryComparable) = struct
  let graph = Dug.create ()
  let clear_profile () = Dug.clear graph
  let get_dug () = graph
  module L = SList

  type 'a t = 'a L.t Dug_extractor.wrap

  module DE = Dug_extractor

  let empty = DE.generate graph (`Empty ()) L.empty
  let gen rs = DE.generate graph (`Empty ()) L.empty (*FIXME*)
    
  let is_empty t = DE.observe graph (`Is_empty ()) L.is_empty t
  let length t =  DE.observe graph (`Length ()) L.length t
  let hd t =  DE.observe graph (`Hd ()) L.hd t

  (** TODO *)
  let from_list l = empty
  let flatten t = empty
  let pop t = assert false

  let iter f t = DE.observe graph (`Iter ()) (L.iter f) t
  let fold f acc t = DE.observe graph (`Fold ()) (L.fold f acc) t
  let to_list t = DE.observe graph (`To_list ()) L.to_list t
  let to_string t =  DE.observe graph (`To_string ()) L.to_string t
  let compare f t1 t2 =  DE.observe2 graph (`Compare((),())) (L.compare f) t1 t2

  let rev t = DE.mutate graph (`Rev((),())) L.rev t
  let cons x t= DE.mutate graph (`Cons((),(),())) (L.cons x) t
  let snoc x t = DE.mutate graph (`Snoc((),(),())) (L.snoc x) t
  let tl t = DE.mutate graph (`Tl((),())) L.tl t
  let append t1 t2 = DE.mutate2 graph (`Append((),(),())) L.append t1 t2

  let rev_map f t = DE.mutate graph (`Rev_map((),())) (L.rev_map f) t
  let map f t = DE.mutate graph (`Map((),())) (L.map f) t
    
end


module Benchmark = struct
end
*)
