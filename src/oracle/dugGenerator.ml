(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)


module Make(DS : DugADT.S)(A : Types.Mono.ArbitraryComparable) = struct

  module DP = DugProfile.Make(DS)

  type t = {
    dug : ((A.t,Dug.Id.t) DS.generator,
	   (A.t,Dug.Id.t) DS.mutator,
	   (A.t,Dug.Id.t) DS.observer) Dug.t;
    frontier : (Dug.Id.t * (unit,unit) DS.op list) RandomBag.t;
    planned_size : Dug.Id.t;
    profile : DP.t;
    max_id : Dug.Id.t;
  }

  let frontier_min = 2
  let frontier_max = 10000

  let rec factf n = if n <= 1. then 1. else n *. (factf (n -. 1.))

  (* The regular poisson function *)
  let poisson' lambda k = 
    (exp ~-.lambda) *. (lambda ** (float k)) /. (factf (float k))
      
  (* simple brute force inversion of the poisson function. *)
  let rec find_poisson l guess p cump = 
    let cump' = cump +. (poisson' l guess) in
      if cump' > p then guess
      else find_poisson l (guess+1) p cump' 
	
  (* Randomally select an integer K whose valule is taken from the
     poisson distribution with mean l.  That is, P(K=x) =
     poisson(l,x). *)
  let poisson l = 
    let p = Random.float 1.0 in 
      match classify_float l with
	| FP_nan
	| FP_infinite -> 0
	| _ -> find_poisson l 0 p 0.

  let fresh_id t = Dug.fresh_id t.dug

  let expected_mutations t = 
    let p = t.profile in
      (1. -. p.DP.mortality) *. p.DP.pmf /. (1. -. p.DP.pmf)

  let chance p = (Random.float 1.0) < p
    
  let num_mutations t = 
    let p = t.profile in
      if chance p.DP.mortality 
      then 0
      else 1 + (poisson (p.DP.pmf /. (1. -. p.DP.pmf)))

  let rec loop n f acc =
    if n <= 0 then acc
    else loop (n-1) f (f acc)

  (* Combine two lists of length m and n respectively by choosing an
     element from lst1 with probability m/(m+n) and from lst2 with
     probability n/(m+n) *)
  let mix_lists lst1 lst2 = 
    let rec helper lst1 len1 lst2 len2 acc = match len1,len2 with
      | 0,0 -> acc
      | 0,_ -> List.rev_append lst2 acc
      | _,0 -> List.rev_append lst1 acc
      | _ -> 
	  let tot = len1 + len2 in
	    (* random will be 0..tot-1, so >= len1 is *)
	    if (Random.int tot) < len1
	    then match lst1 with [] -> assert false
	      | hd::tl -> helper tl (len1-1) lst2 len2 (hd::acc)
	    else match lst2 with [] -> assert false
	      | hd::tl -> helper lst1 len1 tl (len2-1) (hd::acc)
    in
      helper lst1 (List.length lst1) lst2 (List.length lst2) []

  let num_observations num_muts t = 
    num_muts *. (t.profile.DP.obs_mut_ratio)

  let exec_plan num coerce cdf = 
    loop num (fun acc -> (coerce (DP.random_op cdf))::acc) []

  let tot_muts = ref 0 
  let mut_times = ref 0
  let max_muts = ref 0

  let tot_obs = ref 0 
  let obs_times = ref 0
    (*
  let _ = at_exit
    (fun () -> 
      Printf.printf "avg mutations: %f (%d)\n"
	((float !tot_muts) /. (float !mut_times)) !max_muts;

      Printf.printf "avg observers: %f\n"
	((float !tot_obs) /. (float !obs_times))
    )
    *)

  let plan t : (unit,unit) DS.op list = 
    let p = t.profile in
    let num_muts = num_mutations t in
    let muts = exec_plan num_muts DS.coerce_mut p.DP.mut_cdf in
      incr mut_times;
      tot_muts := !tot_muts + num_muts;
      max_muts := max !max_muts num_muts;

    let numf_obs = num_observations (*(expected_mutations t)*) (float num_muts) t in

    let num_p_obs = poisson (numf_obs *. p.DP.pof) in
    let pers_obs = exec_plan num_p_obs DS.coerce_obs p.DP.obs_cdf in
      
    let num_e_obs = poisson (numf_obs *. (1. -. p.DP.pof)) in
    let emph_obs = exec_plan num_e_obs DS.coerce_obs p.DP.obs_cdf in
      incr obs_times;
      tot_obs := !tot_obs + num_p_obs + num_e_obs;

(*      Printf.printf "muts: %d(exp: %f) obs: %f\n" 
	num_muts (expected_mutations t) numf_obs;*)
      match muts with
	| [] -> emph_obs @ pers_obs
	    (* force the persistent operations, to be persisten by
	       placing a mutation first *)
	| hd::tl -> emph_obs @ (hd :: (mix_lists tl pers_obs))

  let update_frontier id future frontier = match future with
    | [] -> frontier
    | _ -> RandomBag.add (id,future) frontier

  let create_node id op future t = 
    Hashtbl.replace t.dug.Dug.nodes id (DS.classify op);
    let frontier = update_frontier id future t.frontier in
      {t with frontier = frontier}

  let rs = Random.State.make_self_init()

  let expand_frontier t = 
(*    Printf.printf "expand!!!!!!!!!!!!!!!!!!!!!\n";*)
    let id = fresh_id t in
    let gen = DP.random_op t.profile.DP.gen_cdf in
    let d_op = DS.coerce_gen gen in
    let op = DS.create_op d_op id (fun () -> A.gen rs) (fun _ -> assert false) in
      create_node id op (plan t) t
      
  let shrink_frontier t = 
(*    Printf.printf "shrink---------------------\n";*)
    let n = RandomBag.choose t.frontier in
      {t with frontier = RandomBag.remove n t.frontier}
      
  let create_random_node t = 
    let ((pred_id,future) as idf) = RandomBag.choose t.frontier in
    let t = {t with frontier = RandomBag.remove idf t.frontier} in
    let next_op = List.hd future in
    let id = fresh_id t in
      (* create a temp table to store what objects where taken from
	 the frontier and placed in which position.  We do this so we can
	 later add then to the edge table of the dug.  We can't do it
	 here, since we need a (Id.t op), not a (unit op) [which next_op
	 is] *)
    let tbl = Hashtbl.create 11 in
    let get_pos i = 
      let arg_id = if i = 0 then pred_id else fst(RandomBag.choose t.frontier) in
	Hashtbl.add tbl i arg_id;
	arg_id
    in
    let op = DS.create_op next_op id (fun () -> A.gen rs) get_pos in
    let () = Hashtbl.iter
      (fun pos src_id ->
	let e = {Dug.target = id; op = DS.classify op; pos = pos} in
	  Hashtbl.add t.dug.Dug.edges src_id e;
      ) tbl
    in
      (* Don't plan a future for an observer node *)
    let new_plan = if Dug.is_observer (DS.classify op) then [] else plan t in
    let t = create_node id op new_plan t in
    let frontier = update_frontier pred_id (List.tl future) t.frontier in
      {t with frontier = frontier}

(* TODO: alternatively, produce all of the generator nodes first, then
   build up dug until the sum of the futures + current size is the
   target size, then just run out the futures.  
*)
  let rec generate_nodes t = 
    let front_size = (RandomBag.length t.frontier) in
      if Dug.Id.compare t.dug.Dug.current_id t.max_id >= 0 then t
      else if front_size < frontier_min then
	generate_nodes (expand_frontier t)
      else if front_size > frontier_max then
	generate_nodes (shrink_frontier t)
      else if chance t.profile.DP.gen_ratio then
	generate_nodes (expand_frontier t) (* add a gen *)
      else
	generate_nodes (create_random_node t) (* add a mut or obs *)

  let generate p size = 
    let t =
      {frontier = RandomBag.empty;
       dug = Dug.create ();
       profile = p;
       max_id = Dug.Id.of_int size;
       planned_size = Dug.Id.zero;
      }
    in
    let t = generate_nodes t in
      Printf.eprintf "%d nodes left in frontier\n" 
	(RandomBag.length t.frontier);
      t.dug

end

