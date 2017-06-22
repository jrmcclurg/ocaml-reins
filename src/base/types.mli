(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(** Various modules and functors used by Reins *)

(** Signatures/functors for modules with parameterized (polymorphic)
    types. *)
module Poly :
sig
  module type Equatable =
  sig
    type 'a t
    val equal : 'a t -> 'a t -> bool 
  end

  module type Comparable =
  sig
    type 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val to_string : ('a -> string) -> 'a t -> string
  end

  module type Hashable =
  sig
    include Equatable
    val hash : 'a t -> int
  end

  module type Arbitrary =
  sig
    type 'a t
    val gen :
      (?size:int -> Random.State.t -> 'a) ->
      ?size:int -> Random.State.t -> 'a t
    val to_string : ('a -> string) -> 'a t -> string
  end

  module type ArbitraryComparable =
  sig
    include Arbitrary
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  end

  module ComposeComparable :
    functor (A : Comparable) ->
      functor (B : Comparable) ->
  sig
    type 'a t = 'a B.t A.t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val to_string : ('a -> string) -> 'a t -> string
  end

  module ComposeGen :
    functor (A : Arbitrary) ->
      functor (B : Arbitrary) ->
  sig
    type 'a t = 'a B.t A.t
    val gen :
      (?size:int -> Random.State.t -> 'a) ->
      ?size:int -> Random.State.t -> 'a t
    val to_string : ('a -> string) -> 'a t -> string
  end

  module ComposeGenComparable :
    functor (A : ArbitraryComparable) ->
      functor (B : ArbitraryComparable) ->
  sig
    type 'a t = 'a B.t A.t
    val gen :
      (?size:int -> Random.State.t -> 'a) ->
      ?size:int -> Random.State.t -> 'a t
    val to_string : ('a -> string) -> 'a t -> string
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  end
    
  (** This module can be used to "close" a series of functors to
      produce a module with a parameterized type.  For example,
      [module CC = ComposeComparable
      module L = CC(CC(List)(Option))(Close)]
      creates a module with type
      [type 'a t = 'a list option]
 *)
  module Close :
  sig
    type 'a t = 'a
    val to_string : ('a -> 'b) -> 'a -> 'b
    val compare : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
  end
end

(** Signatures/functors for modules with unparameterized
    (monomorphic) types *)
module Mono :
sig
  module type Equatable = 
  sig
    type t
    val equal : t -> t -> bool 
  end

  module type Comparable =
  sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string 
  end

  module type Hashable =
  sig
    include Equatable
    val hash : t -> int 
  end

  module type Arbitrary =
  sig
    type t
    val gen : ?size:int -> Random.State.t -> t
    val to_string : t -> string
  end

  module type ArbitraryComparable =
  sig
    include Arbitrary
    val compare : t -> t -> int
  end

  module ComposeComparable :
    functor (P : Poly.Comparable) ->
      functor (M : Comparable) ->
  sig
    type t = M.t P.t
    val compare : t -> t -> int
    val to_string : t -> string
  end

  module ComposeGen :
    functor (P : Poly.Arbitrary) ->
      functor (M : Arbitrary) ->
  sig
    type t = M.t P.t
    val gen : ?size:int -> Random.State.t -> t
    val to_string : t -> string
  end

  module ComposeGenComparable :
    functor (P : Poly.ArbitraryComparable) ->
      functor (M : ArbitraryComparable) ->
  sig
    type t = M.t P.t
    val gen : ?size:int -> Random.State.t -> t
    val to_string : t -> string
    val compare : t -> t -> int
  end

  module ComparablePair :
    functor (M1 : Comparable) ->
      functor (M2 : Comparable) ->
  sig
    type t = M1.t * M2.t
    val compare : t -> t -> int
    val to_string : t -> string
  end

  module Comparable3Tuple :
    functor (M1 : Comparable) ->
      functor (M2 : Comparable) ->
        functor (M3 : Comparable) ->
  sig
    type t = M1.t * M2.t * M3.t
    val compare : t -> t -> int
    val to_string : t -> string
  end

  module GenPair :
    functor (A : Arbitrary) ->
      functor (B : Arbitrary) ->
  sig
    type t = A.t * B.t
    val gen : ?size:int -> Random.State.t -> t
    val to_string : t -> string
  end

  module Gen3Tuple :
    functor (A : Arbitrary) ->
      functor (B : Arbitrary) ->
        functor (C : Arbitrary) ->
  sig
    type t = A.t * B.t * C.t
    val gen : ?size:int -> Random.State.t -> t
    val to_string : t -> string
  end
end

module type Integral =
sig
  type t
  val zero : t
  val one : t
  val minus_one : t
  val abs : t -> t
  val neg : t -> t
  val succ : t -> t
  val pred : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val logand : t -> t -> t
  val lognot : t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
  val compare : t -> t -> int
  val of_int : int -> t
  val to_int : t -> int
  val of_float : float -> t
  val to_float : t -> float
  val to_string : t -> string
  val of_string : string -> t
end

module Int :
sig
  type t = int
  val zero : int
  val one : int
  val minus_one : int
  val abs : int -> int
  val neg : int -> int
  val succ : int -> int
  val pred : int -> int
  val add : int -> int -> int
  val sub : int -> int -> int
  val mul : int -> int -> int
  val div : int -> int -> int
  val rem : int -> int -> int
  val logxor : int -> int -> int
  val logand : int -> int -> int
  val lognot : int -> int
  val logor : int -> int -> int
  val shift_left : int -> int -> int
  val shift_right : int -> int -> int
  val shift_right_logical : int -> int -> int
  val of_int : 'a -> 'a
  val to_int : 'a -> 'a
  val of_float : float -> int
  val to_float : int -> float
  val of_string : string -> int
  val compare : int -> int -> int
  val equal : int -> int -> bool
  val hash : 'a -> 'a
  val to_string : int -> string
  val gen : ?size:int -> Random.State.t -> int
end

module Float :
sig
  type t = float
  val compare : float -> float -> int
  val equal : float -> float -> bool
  val hash : 'a -> int
  val gen : ?size:int -> Random.State.t -> float
  val to_string : float -> string
end

module Bool :
sig
  type t = bool
  val compare : bool -> bool -> int
  val equal : bool -> bool -> bool
  val hash : 'a -> int
  val gen : ?size:'a -> Random.State.t -> bool
  val to_string : bool -> string
end

module Char :
sig
  type t = char
  val compare : char -> char -> int
  val equal : char -> char -> bool
  val hash : 'a -> int
  val gen : ?size:int -> Random.State.t -> char
  val to_string : char -> string
end

module Int32 :
sig
  val zero : int32
  val one : int32
  val minus_one : int32
  external neg : int32 -> int32 = "%int32_neg"
  external add : int32 -> int32 -> int32 = "%int32_add"
  external sub : int32 -> int32 -> int32 = "%int32_sub"
  external mul : int32 -> int32 -> int32 = "%int32_mul"
  external div : int32 -> int32 -> int32 = "%int32_div"
  external rem : int32 -> int32 -> int32 = "%int32_mod"
  val succ : int32 -> int32
  val pred : int32 -> int32
  val abs : int32 -> int32
  val max_int : int32
  val min_int : int32
  external logand : int32 -> int32 -> int32 = "%int32_and"
  external logor : int32 -> int32 -> int32 = "%int32_or"
  external logxor : int32 -> int32 -> int32 = "%int32_xor"
  val lognot : int32 -> int32
  external shift_left : int32 -> int -> int32 = "%int32_lsl"
  external shift_right : int32 -> int -> int32 = "%int32_asr"
  external shift_right_logical : int32 -> int -> int32 = "%int32_lsr"
  external of_int : int -> int32 = "%int32_of_int"
  external to_int : int32 -> int = "%int32_to_int"
  external of_float : float -> int32 = "caml_int32_of_float"
  "caml_int32_of_float_unboxed" [@@unboxed] [@@noalloc]
  external to_float : int32 -> float = "caml_int32_to_float"
  "caml_int32_to_float_unboxed" [@@unboxed] [@@noalloc]
  external of_string : string -> int32 = "caml_int32_of_string"
  val to_string : int32 -> string
  external bits_of_float : float -> int32 = "caml_int32_bits_of_float"
  "caml_int32_bits_of_float_unboxed" [@@unboxed] [@@noalloc]
  external float_of_bits : int32 -> float = "caml_int32_float_of_bits"
  "caml_int32_float_of_bits_unboxed" [@@unboxed] [@@noalloc]
  type t = int32
  val compare : t -> t -> int
  external format : string -> int32 -> string = "caml_int32_format"
  val equal : t -> t -> bool
  val hash : 'a -> int
  val gen : ?size:'a -> Random.State.t -> Int32.t
end

module Int64 :
sig
  val zero : int64
  val one : int64
  val minus_one : int64
  external neg : int64 -> int64 = "%int64_neg"
  external add : int64 -> int64 -> int64 = "%int64_add"
  external sub : int64 -> int64 -> int64 = "%int64_sub"
  external mul : int64 -> int64 -> int64 = "%int64_mul"
  external div : int64 -> int64 -> int64 = "%int64_div"
  external rem : int64 -> int64 -> int64 = "%int64_mod"
  val succ : int64 -> int64
  val pred : int64 -> int64
  val abs : int64 -> int64
  val max_int : int64
  val min_int : int64
  external logand : int64 -> int64 -> int64 = "%int64_and"
  external logor : int64 -> int64 -> int64 = "%int64_or"
  external logxor : int64 -> int64 -> int64 = "%int64_xor"
  val lognot : int64 -> int64
  external shift_left : int64 -> int -> int64 = "%int64_lsl"
  external shift_right : int64 -> int -> int64 = "%int64_asr"
  external shift_right_logical : int64 -> int -> int64 = "%int64_lsr"
  external of_int : int -> int64 = "%int64_of_int"
  external to_int : int64 -> int = "%int64_to_int"
  external of_float : float -> int64 = "caml_int64_of_float"
  "caml_int64_of_float_unboxed" [@@unboxed] [@@noalloc]
  external to_float : int64 -> float = "caml_int64_to_float"
  "caml_int64_to_float_unboxed" [@@unboxed] [@@noalloc]
  external of_int32 : int32 -> int64 = "%int64_of_int32"
  external to_int32 : int64 -> int32 = "%int64_to_int32"
  external of_nativeint : nativeint -> int64 = "%int64_of_nativeint"
  external to_nativeint : int64 -> nativeint = "%int64_to_nativeint"
  external of_string : string -> int64 = "caml_int64_of_string"
  val to_string : int64 -> string
  external bits_of_float : float -> int64 = "caml_int64_bits_of_float"
  "caml_int64_bits_of_float_unboxed" [@@unboxed] [@@noalloc]
  external float_of_bits : int64 -> float = "caml_int64_float_of_bits"
  "caml_int64_float_of_bits_unboxed" [@@unboxed] [@@noalloc]
  type t = int64
  val compare : t -> t -> int
  external format : string -> int64 -> string = "caml_int64_format"
  val equal : t -> t -> bool
  val hash : 'a -> int
  val gen : ?size:'a -> Random.State.t -> Int64.t
end

module Nativeint :
sig
  val zero : nativeint
  val one : nativeint
  val minus_one : nativeint
  external neg : nativeint -> nativeint = "%nativeint_neg"
  external add : nativeint -> nativeint -> nativeint = "%nativeint_add"
  external sub : nativeint -> nativeint -> nativeint = "%nativeint_sub"
  external mul : nativeint -> nativeint -> nativeint = "%nativeint_mul"
  external div : nativeint -> nativeint -> nativeint = "%nativeint_div"
  external rem : nativeint -> nativeint -> nativeint = "%nativeint_mod"
  val succ : nativeint -> nativeint
  val pred : nativeint -> nativeint
  val abs : nativeint -> nativeint
  val size : int
  val max_int : nativeint
  val min_int : nativeint
  external logand : nativeint -> nativeint -> nativeint = "%nativeint_and"
  external logor : nativeint -> nativeint -> nativeint = "%nativeint_or"
  external logxor : nativeint -> nativeint -> nativeint = "%nativeint_xor"
  val lognot : nativeint -> nativeint
  external shift_left : nativeint -> int -> nativeint = "%nativeint_lsl"
  external shift_right : nativeint -> int -> nativeint = "%nativeint_asr"
  external shift_right_logical : nativeint -> int -> nativeint
    = "%nativeint_lsr"
  external of_int : int -> nativeint = "%nativeint_of_int"
  external to_int : nativeint -> int = "%nativeint_to_int"
  external of_float : float -> nativeint = "caml_nativeint_of_float"
  "caml_nativeint_of_float_unboxed" [@@unboxed] [@@noalloc]
  external to_float : nativeint -> float = "caml_nativeint_to_float"
  "caml_nativeint_to_float_unboxed" [@@unboxed] [@@noalloc]
  external of_int32 : int32 -> nativeint = "%nativeint_of_int32"
  external to_int32 : nativeint -> int32 = "%nativeint_to_int32"
  external of_string : string -> nativeint = "caml_nativeint_of_string"
  val to_string : nativeint -> string
  type t = nativeint
  val compare : t -> t -> int
  external format : string -> nativeint -> string = "caml_nativeint_format"
  val equal : t -> t -> bool
  val hash : 'a -> int
  val gen : ?size:'a -> Random.State.t -> Nativeint.t
end

module Big_int :
sig
  type big_int = Big_int.big_int
  val zero_big_int : big_int
  val unit_big_int : big_int
  val minus_big_int : big_int -> big_int
  val abs_big_int : big_int -> big_int
  val add_big_int : big_int -> big_int -> big_int
  val succ_big_int : big_int -> big_int
  val add_int_big_int : int -> big_int -> big_int
  val sub_big_int : big_int -> big_int -> big_int
  val pred_big_int : big_int -> big_int
  val mult_big_int : big_int -> big_int -> big_int
  val mult_int_big_int : int -> big_int -> big_int
  val square_big_int : big_int -> big_int
  val sqrt_big_int : big_int -> big_int
  val quomod_big_int : big_int -> big_int -> big_int * big_int
  val div_big_int : big_int -> big_int -> big_int
  val mod_big_int : big_int -> big_int -> big_int
  val gcd_big_int : big_int -> big_int -> big_int
  val power_int_positive_int : int -> int -> big_int
  val power_big_int_positive_int : big_int -> int -> big_int
  val power_int_positive_big_int : int -> big_int -> big_int
  val power_big_int_positive_big_int : big_int -> big_int -> big_int
  val sign_big_int : big_int -> int
  val compare_big_int : big_int -> big_int -> int
  val eq_big_int : big_int -> big_int -> bool
  val le_big_int : big_int -> big_int -> bool
  val ge_big_int : big_int -> big_int -> bool
  val lt_big_int : big_int -> big_int -> bool
  val gt_big_int : big_int -> big_int -> bool
  val max_big_int : big_int -> big_int -> big_int
  val min_big_int : big_int -> big_int -> big_int
  val num_digits_big_int : big_int -> int
  val string_of_big_int : big_int -> string
  val big_int_of_string : string -> big_int
  val big_int_of_int : int -> big_int
  val is_int_big_int : big_int -> bool
  val int_of_big_int : big_int -> int
  val float_of_big_int : big_int -> float
  val nat_of_big_int : big_int -> Nat.nat
  val big_int_of_nat : Nat.nat -> big_int
  val base_power_big_int : int -> int -> big_int -> big_int
  val sys_big_int_of_string : string -> int -> int -> big_int
  val round_futur_last_digit : string -> int -> int -> bool
  val approx_big_int : int -> big_int -> string
  type t = big_int
  val equal : big_int -> big_int -> bool
  val compare : big_int -> big_int -> bool
  val hash : 'a -> int
  val gen : ?size:'a -> Random.State.t -> Big_int.big_int
  val zero : big_int
  val one : big_int
  val minus_one : big_int
  val abs : big_int -> big_int
  val neg : big_int -> big_int
  val succ : big_int -> big_int
  val pred : big_int -> big_int
  val add : big_int -> big_int -> big_int
  val sub : big_int -> big_int -> big_int
  val mul : big_int -> big_int -> big_int
  val div : big_int -> big_int -> big_int
  val rem : big_int -> big_int -> big_int
  val of_int : 'a -> int -> big_int
  val to_int : 'a -> big_int -> int
  val of_float : float -> big_int
  val to_float : big_int -> float
  val to_string : big_int -> string
  val of_string : string -> big_int
end

module Ratio :
sig
  type ratio = Ratio.ratio
  val null_denominator : ratio -> bool
  val numerator_ratio : ratio -> Big_int.big_int
  val denominator_ratio : ratio -> Big_int.big_int
  val sign_ratio : ratio -> int
  val normalize_ratio : ratio -> ratio
  val cautious_normalize_ratio : ratio -> ratio
  val cautious_normalize_ratio_when_printing : ratio -> ratio
  val create_ratio : Big_int.big_int -> Big_int.big_int -> ratio
  val create_normalized_ratio : Big_int.big_int -> Big_int.big_int -> ratio
  val is_normalized_ratio : ratio -> bool
  val report_sign_ratio : ratio -> Big_int.big_int -> Big_int.big_int
  val abs_ratio : ratio -> ratio
  val is_integer_ratio : ratio -> bool
  val add_ratio : ratio -> ratio -> ratio
  val minus_ratio : ratio -> ratio
  val add_int_ratio : int -> ratio -> ratio
  val add_big_int_ratio : Big_int.big_int -> ratio -> ratio
  val sub_ratio : ratio -> ratio -> ratio
  val mult_ratio : ratio -> ratio -> ratio
  val mult_int_ratio : int -> ratio -> ratio
  val mult_big_int_ratio : Big_int.big_int -> ratio -> ratio
  val square_ratio : ratio -> ratio
  val inverse_ratio : ratio -> ratio
  val div_ratio : ratio -> ratio -> ratio
  val integer_ratio : ratio -> Big_int.big_int
  val floor_ratio : ratio -> Big_int.big_int
  val round_ratio : ratio -> Big_int.big_int
  val ceiling_ratio : ratio -> Big_int.big_int
  val eq_ratio : ratio -> ratio -> bool
  val compare_ratio : ratio -> ratio -> int
  val lt_ratio : ratio -> ratio -> bool
  val le_ratio : ratio -> ratio -> bool
  val gt_ratio : ratio -> ratio -> bool
  val ge_ratio : ratio -> ratio -> bool
  val max_ratio : ratio -> ratio -> ratio
  val min_ratio : ratio -> ratio -> ratio
  val eq_big_int_ratio : Big_int.big_int -> ratio -> bool
  val compare_big_int_ratio : Big_int.big_int -> ratio -> int
  val lt_big_int_ratio : Big_int.big_int -> ratio -> bool
  val le_big_int_ratio : Big_int.big_int -> ratio -> bool
  val gt_big_int_ratio : Big_int.big_int -> ratio -> bool
  val ge_big_int_ratio : Big_int.big_int -> ratio -> bool
  val int_of_ratio : ratio -> int
  val ratio_of_int : int -> ratio
  val ratio_of_nat : Nat.nat -> ratio
  val nat_of_ratio : ratio -> Nat.nat
  val ratio_of_big_int : Big_int.big_int -> ratio
  val big_int_of_ratio : ratio -> Big_int.big_int
  val div_int_ratio : int -> ratio -> ratio
  val div_ratio_int : ratio -> int -> ratio
  val div_big_int_ratio : Big_int.big_int -> ratio -> ratio
  val div_ratio_big_int : ratio -> Big_int.big_int -> ratio
  val approx_ratio_fix : int -> ratio -> string
  val approx_ratio_exp : int -> ratio -> string
  val float_of_rational_string : ratio -> string
  val string_of_ratio : ratio -> string
  val ratio_of_string : string -> ratio
  val float_of_ratio : ratio -> float
  val power_ratio_positive_int : ratio -> int -> ratio
  val power_ratio_positive_big_int : ratio -> Big_int.big_int -> ratio
  val equal : 'a -> 'a -> bool
  val hash : 'a -> int
  val gen : ?size:'a -> Random.State.t -> Ratio.ratio
end

module Complex :
sig
  type t = Complex.t = { re : float; im : float; }
  val zero : t
  val one : t
  val i : t
  val neg : t -> t
  val conj : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val inv : t -> t
  val div : t -> t -> t
  val sqrt : t -> t
  val norm2 : t -> float
  val norm : t -> float
  val arg : t -> float
  val polar : float -> float -> t
  val exp : t -> t
  val log : t -> t
  val pow : t -> t -> t
  val equal : 'a -> 'a -> bool
  val hash : 'a -> int
  val gen : ?size:'a -> Random.State.t -> Complex.t
end

module String :
sig
  external length : string -> int = "%string_length"
  external get : string -> int -> char = "%string_safe_get"
  external set : string -> int -> char -> unit = "%string_safe_set"
  external create : int -> string = "caml_create_string"
  val make : int -> char -> string
  val copy : string -> string
  val sub : string -> int -> int -> string
  val fill : string -> int -> int -> char -> unit
  val blit : string -> int -> string -> int -> int -> unit
  val concat : string -> string list -> string
  val iter : (char -> unit) -> string -> unit
  val escaped : string -> string
  val index : string -> char -> int
  val rindex : string -> char -> int
  val index_from : string -> int -> char -> int
  val rindex_from : string -> int -> char -> int
  val contains : string -> char -> bool
  val contains_from : string -> int -> char -> bool
  val rcontains_from : string -> int -> char -> bool
  val uppercase : string -> string
  val lowercase : string -> string
  val capitalize : string -> string
  val uncapitalize : string -> string
  type t = string
  val compare : t -> t -> int
  external unsafe_get : string -> int -> char = "%string_unsafe_get"
  external unsafe_set : string -> int -> char -> unit
    = "%string_unsafe_set"
  external unsafe_blit : string -> int -> string -> int -> int -> unit
    = "caml_blit_string" [@@noalloc]
  external unsafe_fill : string -> int -> int -> char -> unit
    = "caml_fill_string" [@@noalloc]
  val equal : t -> t -> bool
  val hash : 'a -> int
  val gen : ?size:int -> Random.State.t -> string
  val to_string : 'a -> 'a
end

module Option :
sig
  type 'a t = 'a option
  val compare : ('a -> 'b -> int) -> 'a option -> 'b option -> int
  val equal : 'a option -> 'a option -> bool
  val gen :
    (?size:int -> Random.State.t -> 'a) ->
    ?size:int -> Random.State.t -> 'a option
  val to_string : ('a -> string) -> 'a option -> string
end
