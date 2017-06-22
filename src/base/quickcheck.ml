(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Printf
open Types

module type Law = sig
  module Arg : Types.Mono.Arbitrary
  val desc : string
  val law : Arg.t -> bool
end

module type Config = sig
  val num_iterations : int
  val size_arg : int option
  val max_trivial_percentage : float
end

exception Trivial
  
module Check(Conf : Config)(L : Law) = struct
  let max_trivs = 
    int_of_float ((float Conf.num_iterations) *. 
		    Conf.max_trivial_percentage)
    
  let fail_exn iter e arg = 
    let msg = sprintf "Test <%s> raised exception after %d tries.\nInput was %s\n  Exception was %s\n"
      L.desc iter (L.Arg.to_string arg) (Printexc.to_string e)
    in failwith msg

  let fail_test iter arg = 
    let msg = sprintf "Test <%s> failed after %d tries.\nInput was %s\n"
      L.desc iter (L.Arg.to_string arg) 
    in failwith msg

  let fail_trivial trivs n = 
    let msg = sprintf "Test <%s> could not be tested due to excessive trivial input.  %d trivial inputs and %d non-trivial inputs were tried\n"
      L.desc trivs n
    in failwith msg

  let test rs = 
    let rec loop trivs n : unit = 
      if trivs >= max_trivs then fail_trivial trivs n
      else if n >= Conf.num_iterations then () 
      else
	let arg = match Conf.size_arg with
	  | None -> L.Arg.gen  rs
	  | Some s -> L.Arg.gen ~size:s rs 
	in
	  try let res = 
	    try L.law arg 
	    with Trivial -> raise Trivial | e -> fail_exn n e arg
	  in
	    if res then loop trivs (n+1) else fail_test n arg
	  with Trivial -> loop (trivs+1) n
    in loop 0 1

  let desc = L.desc
end
  
