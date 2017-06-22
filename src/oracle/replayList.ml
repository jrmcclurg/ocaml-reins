(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

type ('arg,'list,'listlist,'slist) op = [
  | `Is_empty of 'list
	
  | `Length of 'list
	
  | `Rev of 'list
      
  | `Cons of 'arg * 'list
      
  | `Snoc of 'arg * 'list
      
  | `Hd of 'list
      
  | `Tl of 'list
      
  | `Pop of 'list
      
  | `Append of 'list * 'list
      
  | `Flatten of 'listlist
      
  | `From_list of 'slist
      
  | `To_list of 'list
      
  | `Iter of 'list
      
  | `Fold of 'list
      
  | `Rev_map of 'list
      
  | `Map of 'list
      
  | `To_string of ('arg -> string) * 'list
      
  | `Compare of ('arg -> 'arg -> int) * 'list * 'list
      
  | `Gen of (?size:int -> Random.State.t -> 'arg) * int option * Random.State.t
  ]


module Replay(L : Lists.ListSig) = struct
  let eval = function
    | `Is_empty t -> ignore(L.is_empty t)
    | `Length t -> ignore(L.length t)
    | `Rev t -> ignore(L.rev t)
    | `Cons(x,t) -> ignore(L.cons x t)
    | `Snoc(x,t) -> ignore(L.snoc x t)
    | `Hd t -> ignore(L.hd t)
    | `Tl t -> ignore(L.tl t)
    | `Pop t -> ignore(L.pop t)
    | `Append(t1, t2) -> ignore(L.append t1 t2)
    | `Flatten t -> ignore(L.flatten t)
    | `From_list l -> ignore(L.from_list l)
    | `To_list t -> ignore(L.to_list t)
    | `Iter t -> ignore(L.iter (fun _ -> ()) t)
    | `Fold t -> ignore(L.fold (fun () _ -> ()) () t)
    | `Rev_map t -> ignore(L.rev_map (fun x -> x) t)
    | `Map t -> ignore(L.map (fun x -> x) t)
    | `To_string(to_s, t) -> ignore(L.to_string to_s t)
    | `Compare(f, t1, t2) -> ignore(L.compare f t1 t2)
    | `Gen(f, size, rs) -> ignore(L.gen f ~size:size rs)
end
  
let replay history = match history with
  | [] -> ()
  | _ -> 
      let lst = List.rev history in
      let module RL = Replay(SList) in
	List.iter RL.eval lst

	

module Make(L : Lists.ListSig)(A : sig type t end) = struct

  let __history : (A.t,A.t L.t, A.t L.t L.t, A.t list) op list ref = ref []
    
  let __save x = 
    __history := x :: !__history

(*  let _ = at_exit (fun () -> replay !__history)*)

  type t = A.t L.t

  let empty = L.empty
    
  let is_empty (t:t) = __save (`Is_empty t);L.is_empty t

  let length (t:t) = __save (`Length t); L.length t

  let rev (t:t) = __save (`Rev t);L.rev t

  let cons x (t:t) = __save (`Cons(x,t)); L.cons x t
    
  let snoc x (t:t) = __save (`Snoc(x,t)); L.snoc x t

  let hd (t:t) = __save (`Hd t); L.hd t

  let tl (t:t) = __save (`Tl t); L.tl t

  let pop (t:t) = __save (`Pop t); L.pop t

  let append (t1:t) (t2:t) = __save (`Append(t1,t2)); L.append t1 t2

  let flatten (t:t L.t) = __save (`Flatten t); L.flatten t

  let from_list (l:A.t list) = __save (`From_list l); L.from_list l

  let to_list (t:t) = __save (`To_list t); L.to_list t

  let iter f (t:t) = __save (`Iter t); L.iter f t

  let fold f acc (t:t) = __save (`Fold t); L.fold f acc t

  let rev_map f (t:t) = __save (`Rev_map t); L.rev_map f t

  let map (f:A.t->'a) (t:t) = __save (`Map t); L.map f t

  let to_string to_s (t:t) = __save (`To_string(to_s,t)); L.to_string to_s t

  let compare (f:A.t->A.t->int) t1 t2 = __save (`Compare(f,t1,t2)); L.compare f t1 t2

  let gen f ?size rs : t = __save (`Gen(f,size,Random.State.copy rs)); L.gen f ?size rs
end
