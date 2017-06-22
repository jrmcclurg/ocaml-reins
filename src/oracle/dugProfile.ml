(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Printf
open Dug

module Make(DS : DugADT.S) = struct

  type t = {
      gen_cdf : ((unit,unit) DS.generator * float) list;
      mut_cdf : ((unit,unit) DS.mutator * float) list;
      obs_cdf : ((unit,unit) DS.observer * float) list;
      gen_ratio : float;
      obs_mut_ratio : float;
      mortality : float;
      pmf : float;
      pof : float;
    }

  let random_op cdf =
    let prob = Random.float 1.0 in
    let rec helper = function
      | [] -> assert false
      | (op,_)::[] -> op
      | (op,c)::tl -> if c >= prob then op else helper tl
    in 
    let res = helper cdf in
(*      Printf.eprintf "random: %f -> %d\n" prob (pv_tag res);*)
      res
      
  let to_string t = 
    let gen_s x = DS.op_to_string (DS.coerce_gen x) in
    let mut_s x = DS.op_to_string (DS.coerce_mut x) in
    let obs_s x = DS.op_to_string (DS.coerce_obs x) in
    let buf = Buffer.create 127 in
    let f to_s (op,weight) = 
      Buffer.add_string buf (sprintf "  %s %f\n" (to_s op) weight)
    in
      Buffer.add_string buf "{\n";
      Buffer.add_string buf " gen cdf:\n";
      List.iter (f gen_s) t.gen_cdf;
      Buffer.add_string buf " mut cdf:\n";
      List.iter (f mut_s) t.mut_cdf;
      Buffer.add_string buf " obs cdf:\n";
      List.iter (f obs_s) t.obs_cdf;
      Buffer.add_string buf
	(sprintf " gen_ratio: %f\n obs/mut: %f\n mort: %f\n pmf: %f\n pof: %f\n}"
	    t.gen_ratio t.obs_mut_ratio t.mortality t.pmf t.pof);
      Buffer.contents buf

  type ('a,'b,'c) profile_data = {
      gen_nodes : ('a,Int64.t) Hashtbl.t;
      mut_nodes : ('b,Int64.t) Hashtbl.t;
      obs_nodes : ('c,Int64.t) Hashtbl.t;
      gen_weights : ('a,Int64.t) Hashtbl.t;
      mut_weights : ('b,Int64.t) Hashtbl.t;
      obs_weights : ('c,Int64.t) Hashtbl.t;
      (* nodes that are never mutated (only non-observer nodes apply) *)
      mutable mortality_count : Int64.t;
      mutable pmf_count : Int64.t;
      mutable pof_count : Int64.t;
    }

  let empty_profile () = {
      gen_nodes = Hashtbl.create 127;
      mut_nodes = Hashtbl.create 127;
      obs_nodes = Hashtbl.create 127;
      gen_weights = Hashtbl.create 127;
      mut_weights = Hashtbl.create 127;
      obs_weights = Hashtbl.create 127;
      mortality_count = Int64.zero;
      pmf_count = Int64.zero;
      pof_count = Int64.zero;
    }

  let incr_tbl tbl op = 
    let old = 
      try match Hashtbl.find_all tbl op with
	| [] -> Int64.zero
	| [x] -> x
	| _ -> assert false
      with Not_found -> Int64.zero 
    in
      Hashtbl.replace tbl op (Int64.succ old)
	
  let rec after_true f = function
    | [] -> []
    | hd::tl -> if f hd then tl else after_true f tl

  let count_persistent f edges = 
    let after_mut = after_true (fun x -> is_mutator x.op) edges in
      List.fold_left (fun acc x -> if f x.op then Int64.succ acc else acc)
	Int64.zero after_mut

  let rec update_weights t pd edges = match edges with 
    | [] -> ()
    | e::tl -> 
	begin match e.op with
	  | Generator _ -> assert false
	  | Mutator op ->  
	      let op = match DS.classify (DS.strip (DS.coerce_mut op)) with
		| Mutator o -> o | _ -> assert false
	      in incr_tbl pd.mut_weights op
	  | Observer op -> 
	      let op = match DS.classify (DS.strip (DS.coerce_obs op)) with
		| Observer o -> o | _ -> assert false
	      in incr_tbl pd.obs_weights op
	end;
	update_weights t pd tl

  let rec profile_node t pd id = 
    if Id.compare id t.current_id > 0 then ()
    else
      let kind = Hashtbl.find t.nodes id in
      let edges = Hashtbl.find_all t.edges id in
      let edges = List.rev edges (* fifo order the edges *) in
      let update_mortality () = 
	if not (List.exists (fun x -> is_mutator x.op) edges)
	then pd.mortality_count <- Int64.succ pd.mortality_count
      in
      let update_persistents () = 
	let ocount = count_persistent is_observer edges in
	let mcount = count_persistent is_mutator edges in
	  pd.pof_count <- Int64.add pd.pof_count ocount;
	  pd.pmf_count <- Int64.add pd.pmf_count mcount
      in
	update_weights t pd edges;
	update_persistents ();

	(* TODO: clean this up! *)
	begin match kind with
	  | Generator op -> 
	      let op = match DS.classify (DS.strip (DS.coerce_gen op)) with
		| Generator o -> o | _ -> assert false
	      in
		incr_tbl pd.gen_weights op;
		update_mortality ();
		incr_tbl pd.gen_nodes op;

	  | Mutator op -> 
	      let op = match DS.classify (DS.strip (DS.coerce_mut op)) with
		| Mutator o -> o | _ -> assert false
	      in
		update_mortality ();
		incr_tbl pd.mut_nodes op;
	  | Observer op -> 
	      let op = match DS.classify (DS.strip (DS.coerce_obs op)) with
		| Observer o -> o | _ -> assert false
	      in
		assert (List.length edges = 0);
		incr_tbl pd.obs_nodes op;
	end;
	profile_node t pd (Dug.Id.succ id)

  let sum_tbl tbl = 
    Hashtbl.fold (fun k v acc -> Int64.add v acc) tbl Int64.zero

  let build_weights totf tbl = 
    let tbl' = Hashtbl.create (Hashtbl.length tbl) in
      Hashtbl.iter (fun k v -> Hashtbl.add tbl' k ((Int64.to_float v) /. totf)) tbl;
      Hashtbl.find tbl'

  let build_cdf pdf tbl = 
    let lst = Hashtbl.fold (fun op _ acc -> op::acc) tbl [] in
      (* forace a deterministic (but arbitrary) ordering *)
    let lst = List.sort Pervasives.compare lst in
    let _,l = List.fold_left
      (fun (c,acc) op ->
	let c' = c +. (pdf op) in
	  c', ((op,c') :: acc)
      ) (0.0,[]) lst
    in List.rev l

  let build_profile pd = 
    (*    let tot_gen_weights = Int64.to_float (sum_tbl pd.gen_weights) in*)
    let tot_mut_weights = Int64.to_float (sum_tbl pd.mut_weights) in
    let tot_obs_weights = Int64.to_float (sum_tbl pd.obs_weights) in
    let tot_gen_nodes = Int64.to_float (sum_tbl pd.gen_nodes) in
    let tot_mut_nodes = Int64.to_float (sum_tbl pd.mut_nodes) in
    let tot_obs_nodes = Int64.to_float (sum_tbl pd.obs_nodes) in
    let gen_f = build_weights tot_gen_nodes pd.gen_nodes in
    let mut_f = build_weights tot_mut_nodes pd.mut_nodes in
    let obs_f = build_weights tot_obs_nodes pd.obs_nodes in
      {
       gen_cdf = build_cdf gen_f pd.gen_nodes;
       mut_cdf = build_cdf mut_f pd.mut_nodes;
       obs_cdf = build_cdf obs_f pd.obs_nodes;

       gen_ratio =
	  tot_gen_nodes /. (tot_gen_nodes +. tot_mut_nodes +. tot_obs_nodes);

       obs_mut_ratio = tot_obs_nodes /. tot_mut_nodes;

       mortality = 
	  (Int64.to_float pd.mortality_count) /. 
	    (tot_gen_nodes +. tot_mut_nodes);

       pmf = (Int64.to_float pd.pmf_count) /. tot_mut_weights;

       pof = (Int64.to_float pd.pof_count) /.  tot_obs_weights;
      }

  let profile t = 
    let pd = empty_profile () in
      profile_node t pd Dug.Id.one;
      build_profile pd

end
