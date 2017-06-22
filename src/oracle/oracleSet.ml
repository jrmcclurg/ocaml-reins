(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Types
open Dug

type nodeid = Dug.Id.t

type ('v,'t) generator = [
| `Empty of 't
| `Singleton of 't * 'v
| `Gen1 of 't
| `Gen of 't
| `From_cursor of 't
]

type ('v,'t) mutator = [
| `Add of 't * 'v * 't
| `Remove of 't * 'v * 't
| `Union of 't * 't * 't
| `Inter of 't * 't * 't
| `Diff of 't * 't * 't
]

type ('v,'t) observer = [
  | `Min_elt of 't
  | `Max_elt of 't
  | `Choose of 't
  | `Is_empty of 't
  | `Mem of 'v * 't
  | `Equal of 't * 't
  | `Well_formed of 't
  | `Cardinal of 't
  | `Compare of 't * 't
  | `To_string of 't
  | `Fold of 't
  | `Iter of 't
]

(* the type of all set operations *)
type ('v,'t) op = [
    ('v,'t) generator
  | ('v,'t) mutator
  | ('v,'t) observer 
]

let op_to_string = function
  | `Min_elt _ -> "min_elt"
  | `Max_elt _ -> "max_elt"
  | `Choose _ -> "choose"
  | `Is_empty _ -> "is_empty"
  | `Mem _ -> "mem"
  | `Equal _ -> "equal"
  | `Well_formed _ -> "well_formed"
  | `Cardinal _ -> "cardinal"
  | `Compare _ -> "compare"
  | `Empty _ -> "empty"
  | `Singleton _ -> "singleton"
  | `Add _ -> "add"
  | `Remove _ -> "remove"
  | `Union _ -> "union"
  | `Inter _ -> "inter"
  | `Diff _ -> "diff"
  | `Gen1 _ -> "gen1"
  | `Gen _ -> "gen"
  | `From_cursor _ -> "from_cursor"
  | `To_string _ -> "to_string"
  | `Iter _ -> "iter"
  | `Fold _ -> "fold"

let classify = function
  | #generator as o -> Dug.Generator o
  | #mutator as o -> Dug.Mutator o
  | #observer as o -> Dug.Observer o

let op_dependencies : ('a,Dug.Id.t) op -> Dug.Id.t list = function
  | #generator -> []

  | `Min_elt t
  | `Max_elt t
  | `Choose t
  | `Is_empty t
  | `Mem(_,t)
  | `Well_formed t
  | `Cardinal t
  | `Add(_,_,t)
  | `Remove(_,_,t)
  | `Iter t
  | `Fold t
  | `To_string t -> [t]

  | `Equal(t1,t2)
  | `Compare(t1,t2)
  | `Union(_,t1,t2)
  | `Inter(_,t1,t2)
  | `Diff(_,t1,t2) -> [t1;t2]

let coerce_gen x = (x :> ('a,'b) op)
let coerce_mut x = (x :> ('a,'b) op)
let coerce_obs x = (x :> ('a,'b) op)
  
let create_op uop id elt_f t_f = 
  match uop with
    | `Min_elt _ -> `Min_elt (t_f 0)
    | `Max_elt _ -> `Max_elt (t_f 0)
    | `Choose _ -> `Choose (t_f 0)
    | `Is_empty _ -> `Is_empty (t_f 0)
    | `Mem _ -> `Mem(elt_f (), t_f 0)
    | `Equal _ -> `Equal(t_f 0,t_f 1)
    | `Well_formed _ -> `Well_formed(t_f 0)
    | `Cardinal _ -> `Cardinal(t_f 0)
    | `Compare _ -> `Compare(t_f 0,t_f 1)
    | `Empty _ -> `Empty(id)
    | `Singleton _ -> `Singleton(id,elt_f ())
    | `Add _ -> `Add(id,elt_f (),t_f 0)
    | `Remove _ -> `Remove(id,elt_f (), t_f 0)
    | `Union _ -> `Union(id,t_f 0,t_f 1)
    | `Inter _ -> `Inter(id,t_f 0,t_f 1)
    | `Diff _ -> `Diff(id,t_f 0,t_f 1)
    | `Gen1 _ -> `Gen1(id)
    | `Gen _ -> `Gen(id)
    | `From_cursor _ -> `From_cursor(id)
    | `Iter _ -> `Iter(t_f 0)
    | `Fold _ -> `Fold(t_f 0)
    | `To_string _ -> `To_string(t_f 0)

let strip (op : ('a,'b) op) : (unit,unit) op = 
  create_op op () (fun () -> ()) (fun _ -> ())


(**********************************************************)


module Extractor(A : Mono.ArbitraryComparable) : sig
  include Sets.GenSetSig with type 'a result = 'a
			 and type elt = A.t
  val get_dug : unit -> 
    ((elt,Dug.Id.t) generator, 
     (elt,Dug.Id.t) mutator,
     (elt,Dug.Id.t) observer) Dug.t
  val clear_profile : unit -> unit
end = struct
  module S = AVLSet.MonoSet(A)
    
  let graph = Dug.create ()
    
  let clear_profile () = Dug.clear graph
    
  type 'a result = 'a S.result
  type t = S.t DugExtractor.wrap
  type 'a set = t
  type cursor = S.cursor
  type 'a cursor_ = 'a S.cursor_
  type elt = S.elt
  type 'a elt_ = 'a S.elt_
  type ('a,'b) result_ = ('a,'b) S.result_

  (* since we are storing the element and set types as a side effect
     of these operations, OCaml is unable to generalize the
     polymorphic form of these types, so we need to provide explicit
     specializations (we don't actually use the parameter anyway) *)
  type uelt = unit S.elt_
  type uset = unit S.set
  type 'a ures = ('a,unit) S.result_
  type ut = uset DugExtractor.wrap

  module DE = DugExtractor

  let empty : t = DE.generate graph (fun t -> `Empty t) S.empty
  let singleton (x:uelt) : ut = 
    DE.generate graph (fun t -> `Singleton(t,x)) (S.singleton x)

  let is_empty t = DE.observe graph (`Is_empty t.DE.id) S.is_empty t

  let mem (x:uelt) (t:ut) : bool ures = 
    DE.observe graph (`Mem(x,t.DE.id)) (S.mem x) t

  let add (x:uelt) (t:ut) : ut = 
    DE.mutate graph (fun r -> `Add(r,x,t.DE.id)) (S.add x) t

  let remove (x:uelt) (t:ut) : ut =
    DE.mutate graph (fun r -> `Remove(r,x,t.DE.id)) (S.remove x) t

  let min_elt t = DE.observe graph (`Min_elt t.DE.id) S.min_elt t

  let max_elt t = DE.observe graph (`Max_elt t.DE.id) S.max_elt t

  let choose t = DE.observe graph (`Choose t.DE.id) S.choose t

  let cardinal t = DE.observe graph (`Cardinal t.DE.id) S.cardinal t

  let compare t1 t2 = DE.observe2 graph (`Compare(t1.DE.id,t2.DE.id)) S.compare t1 t2

  let equal t1 t2 = DE.observe2 graph (`Equal(t1.DE.id, t2.DE.id)) S.equal t1 t2

  let iter f t = DE.observe graph (`Iter t.DE.id) (S.iter f) t
  let fold f acc t = DE.observe graph (`Fold t.DE.id) (S.fold f acc) t

  let union t1 t2 = DE.mutate2 graph (fun r -> `Union(r,t1.DE.id,t2.DE.id)) S.union t1 t2
  let inter t1 t2 = DE.mutate2 graph (fun r -> `Inter(r,t1.DE.id,t2.DE.id)) S.inter t1 t2
  let diff  t1 t2 = DE.mutate2 graph (fun r -> `Diff (r,t1.DE.id,t2.DE.id)) S.diff t1 t2

  (* OCaml can't generalize the return type of f ('a elt_) even though
     'a isn't used.  It doesn't seem to notice if I annotate it and
     explicitly instantiate the variable either... so we'll just always
     generate an empty container for now. (at least until I address
     adding HOF's to this framework in a more general way)
  *)
  let gen1 f ?size rs : unit S.set DE.wrap = empty
    (*DE.generate graph (`Gen1(f,rs)) (S.gen1 f ?size rs)*)

  let gen ?size rs : unit S.set DE.wrap = empty
    (*DE.generate graph (`Gen1(f,rs)) (S.gen1 f ?size rs)*)

  let well_formed t = 
    DE.observe graph (`Well_formed t.DE.id) S.well_formed t

  let of_result = S.of_result

  let to_cursor t = S.to_cursor t.DE.data

  (*
    let from_cursor (c : unit S.cursor_) : unit S.set DE.wrap = 
    DE.generate graph (fun i -> `From_cursor(i,c)) (S.from_cursor c)
  *)  
  let from_cursor c = empty
    (* these don't invole type t at all *)
  let at_top = S.at_top
  let at_left = S.at_left
  let at_right = S.at_right
  let move_up = S.move_up
  let move_down_left = S.move_down_left
  let move_down_right = S.move_down_right
  let went_left = S.went_left
  let went_right = S.went_right
  let has_value = S.has_value
  let get_value = S.get_value

  let to_string t = DE.observe graph ( `To_string t.DE.id) S.to_string t

  let get_dug () : 
      ((S.elt,Dug.Id.t) generator,
       (S.elt,Dug.Id.t) mutator,
       (S.elt,Dug.Id.t) observer) Dug.t 
      = graph

end


(**********************************************************)


module Benchmark(S : Sets.GenSetSig with type 'a result = 'a) = struct

  module VarMap = Map.Make(Dug.Id)

  type env = S.t VarMap.t
      
  let empty_env = VarMap.empty
    
  let eval_rs = Random.State.make_self_init ()
  let eval_t env op = 
    let id,t = match op with
      | `Empty id -> id,S.empty
      | `Singleton(id,x) -> id,S.singleton x
      | `Add(id,x,t) -> id,S.add x (VarMap.find t env)
      | `Remove(id,x,t) -> id,S.remove x (VarMap.find t env)
      | `Union(id,t1,t2) -> id,S.union (VarMap.find t1 env) (VarMap.find t2 env)
      | `Inter(id,t1,t2) -> id,S.inter (VarMap.find t1 env) (VarMap.find t2 env)
      | `Diff(id,t1,t2) -> id,S.diff (VarMap.find t1 env) (VarMap.find t2 env)
      | `Gen1(id) -> id, S.empty (*(S.gen1 A.gen eval_rs)*)
      | `Gen(id) -> id, (S.gen eval_rs)
      | `From_cursor(id) -> id,S.empty
    in 
      VarMap.add id t env
	
  let rec eval_obs env = function
    | `Min_elt t -> ignore(S.min_elt (VarMap.find t env))
    | `Max_elt t -> ignore(S.max_elt (VarMap.find t env))
    | `Choose t -> ignore(S.choose (VarMap.find t env))
    | `Is_empty t -> ignore(S.is_empty (VarMap.find t env))
    | `Mem(x,t) -> ignore(S.mem x (VarMap.find t env))
    | `Equal(t1,t2) -> ignore(S.equal (VarMap.find t1 env) (VarMap.find t2 env))
    | `Well_formed t -> ignore(S.well_formed (VarMap.find t env))
    | `Cardinal t -> ignore(S.cardinal (VarMap.find t env))
    | `Compare(t1,t2) -> ignore(S.compare (VarMap.find t1 env) (VarMap.find t2 env))
    | `To_string t -> ignore(S.to_string (VarMap.find t env))
    | `Iter t -> ignore(S.iter (fun _ -> ()) (VarMap.find t env))
    | `Fold t -> ignore(S.fold (fun _ _ -> ()) () (VarMap.find t env))
	
  let eval_op env op = match op with
    | #generator as o -> eval_t env o
    | #mutator as o -> eval_t env o
    | #observer as o -> eval_obs env o; env
	
  let dug_to_list dug = 
    let rec helper id acc = 
      if Dug.Id.compare id Dug.Id.zero <= 0 then acc 
      else
	let op = match Hashtbl.find dug.nodes id with
	  | Generator o -> coerce_gen o
	  | Mutator o -> coerce_mut o
	  | Observer o -> coerce_obs o
	in helper (Dug.Id.pred id) (op :: acc)
    in helper dug.current_id []
	 
  let benchmark dug = 
    let lst = dug_to_list dug in
    let start = Unix.gettimeofday () in
    let _ = List.fold_left eval_op empty_env lst in
    let fin = Unix.gettimeofday() in
      fin -. start

end

(*
  module type ResS = Sets.GenSet with type 'a result = 'a

  module Make_Is_Set
  (S : ResS)
  (A : ArbitraryComparable with type t = S.elt)
  : ResS
  = Make(S)(A)


  Dug_set:
  module Profile(A)
  module Benchmark(HOSet)(A)

*)
