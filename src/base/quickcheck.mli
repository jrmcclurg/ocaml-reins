(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Random Testing Framework

    This module implements a random testing framework based on
    Claessen and Hughes's QuickCheck library for Haskell.  

 *)

exception Trivial

module type Law =
sig
  module Arg : Types.Mono.Arbitrary
    (** A value of type Arg.t will be randomly generated and passed to
	the law function below. *)

  val desc : string
    (** Description of the test.  This value is simply stored in the
	result of the Check functor below for easy access by a test
	driver. 
    *)

  val law : Arg.t -> bool
    (** The function that implements the law.  The function should
	return [true] when the law holds for the input and [false] if
	the law does not hold.  It may also raise the exception
	{!Trivial} if the law only trivially applies to the input, in
	which case a new input will be attempted.
    *)
end

module type Config = 
sig
  val num_iterations : int 
    (** This value determines how many inputs will be passed to the
	{!Law.law} function.  Values that are signaled to be trivial
	are not counted.
    *)

  val size_arg : int option
    (** This value is passed as the option size paramter to the
	function {!Types.Mono.Arbitrary.gen} when generating input
	for a law.
    *)

  val max_trivial_percentage : float 
    (** This value determines how many inputs are allowed to be
	classified as trivial before giving up and classifying the law
	as failed.  The value should be in the range \[0\.0,1\.0)
    *)

end


module Check : 
  functor (Conf : Config) -> 
    functor (L : Law) -> sig
      val desc : string
	(** A copy of the test description supplied by the Law
	    module *)
	
      val test : Random.State.t -> unit
	(** The function which executes the series random tests on law
	    [L] *)
    end
