(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

module Make(L : Lists.ListSig) = struct
  type 'a dlist = 'a L.t * 'a L.t 

  type 'a t = 'a dlist
      
  let empty = L.empty, L.empty

  let at_front (p,_) = L.is_empty p
    
  let at_back (_,n) = L.is_empty n

  let is_empty l = at_front l && at_back l

  let next_length (_,n) = L.length n
  let prev_length (p,_) = L.length p

  let length (p,n) = (L.length p) + (L.length n)

  let rev (p,n) = (n,p)

  (*
    [splice l1 l2]
    prev_l2 :: prev_l1 :: next_l1 :: next_l2
  *)
  let splice (sp,sn) (p,n) = (L.append sp p), (L.append sn n)

  let rec next (p,n) = 
    if L.is_empty n then failwith "next"
    else let x,n' = L.pop n in (L.cons x p),n'

  let rec prev (p,n) = 
    if L.is_empty p then failwith "prev"
    else let x,p' = L.pop p in p',(L.cons x n)

  let cons x (p,n) = p, (L.cons x n)
    
  let prev_cons x (p,n) =(L.cons x p), n

  let hd (_,n) = 
    try fst (L.pop n) with Failure "pop" -> failwith "hd"

  let value (_,n) = if L.is_empty n then None else Some (L.hd n)

  let prev_hd (p,_) = 
    try fst (L.pop p) with Failure "pop" -> failwith "prev_hd"

  let tl (p,n) = 
    try let tl = snd (L.pop n) in (p,tl) 
    with Failure "pop" -> failwith "tl"

  let prev_tl (p,n) = 
    try let tl = snd (L.pop p) in (tl,n)
    with Failure "pop" -> failwith "prev_tl"
      
  let pop (p,n) = let h,tl = L.pop n in h, (p,tl)

  let prev_pop (p,n) = 
    try let h,tl = L.pop p in h, (tl,n)
    with Failure "pop" -> failwith "prev_pop"

  let rec goto_front l = 
    if at_front l then l else goto_front (prev l)

  let rec goto_back l = 
    if at_back l then l else goto_back (next l)

  (* stay at same position in l1 and tack l2 onto the end *)
  let append l1 l2 = splice l1 (goto_front l2)

  let snoc x (p,n) = p, (L.snoc x n)

  let last (p,n) = (L.last n)

  let prev_snoc x (p,n) =(L.snoc x p), n

  let rec fold1 f acc l = 
    if L.is_empty l then acc 
    else 
      let x,tl = L.pop l in
	fold1 f (f acc x) tl

  let fold f acc (p,n) = 
    fold1 f (fold1 f acc n) p

  let iter f l = fold (fun () -> f) () l

  let map f (p,n) = 
    let n' = L.map f n in
    let p' = L.map f p in
      (p',n')

  (* If we applied rev_map to the front and back list, we would still
     have to reverse them again.  So we might as well just use this
     simple version (since our rev is O(1)) in the hopes that L
     provides an efficient 'map' (and no worse the L.rev L.rev_map)*)
  let rev_map f l = rev (map f l)

  let flatten ll = 
    let dl = fold (fun acc l -> splice l (goto_back acc)) empty (goto_front ll) in
      goto_back dl

  let from_list l = L.empty, (L.from_list l)

  let to_list dl = 
    let dl' = goto_back dl in
      fold (fun acc x -> x::acc) [] dl'

  let rec to_string to_s t = ListCommon.to_string iter pop to_s (goto_front t)

  let rec compare c x y = 
    let x = goto_front x in
    let y = goto_front y in
      match at_back x, at_back y with
	| true,true -> 0
	| false,true -> 1
	| true,false -> -1
	| false,false -> 
	    let hx,tx = pop x in
	    let hy,ty = pop y in
	      match c hx hy with
		| 0 -> compare c tx ty
		| v -> v

  let gen agen ?(size=50) rs = 
    (L.gen agen ~size:(size/2) rs), (L.gen agen ~size:(size/2) rs)
      

  type 'a list_ = 'a t
  type 'a cursor = 'a dlist
  let to_cursor x = x
  let from_cursor x = x

  let current = hd
  let move_prev = prev
  let move_next = next

  let list x = x
  let replace_list (p1,n1) (p2,_) = 
    let n2 = L.append p1 n1 in
      (p2,n2)

end

