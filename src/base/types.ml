(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

(* combinator for composing compare functions *)
let cmp2 c1 f a1 a2 = match c1 with
  | 0 -> f a1 a2
  | _ -> c1

module Poly = struct

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
    val gen : (?size:int -> Random.State.t -> 'a) -> ?size:int -> 
      Random.State.t -> 'a t
    val to_string : ('a -> string) -> 'a t -> string
  end
    
  module type ArbitraryComparable = 
  sig
    include Arbitrary
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  end

  module ComposeComparable (A : Comparable) (B : Comparable) : 
    Comparable with type 'a t = 'a B.t A.t = 
  struct
    type 'a t = 'a B.t A.t
    let compare f = A.compare (B.compare f)
    let to_string f = A.to_string (B.to_string f)
  end

  module ComposeGen (A : Arbitrary) (B : Arbitrary) :
    Arbitrary with type 'a t = 'a B.t A.t = struct
      type 'a t = 'a B.t A.t
      let to_string to_s t = A.to_string (B.to_string to_s) t
      let gen (gen1: ?size:int -> Random.State.t -> 'a) ?size rs =
	A.gen (B.gen gen1) ?size rs
    end
    
  module ComposeGenComparable
    (A : ArbitraryComparable) 
    (B : ArbitraryComparable) 
    : ArbitraryComparable with type 'a t = 'a B.t A.t = struct
      include ComposeGen(A)(B) 
      let compare f x y = A.compare (B.compare f) x y
    end

  (* This module allows you to close the Compose* functors. *)
  module Close = struct
    type 'a t = 'a
    let to_string to_s t = to_s t
    let compare cmp t1 t2 = cmp t1 t2
  end

end

module Mono = struct
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
    
  module ComposeComparable (P : Poly.Comparable) (M : Comparable) 
    : Comparable with type t = M.t P.t = 
  struct
    type t = M.t P.t
    let compare x y = P.compare M.compare x y
    let to_string t = P.to_string M.to_string t
  end

  module ComposeGen (P : Poly.Arbitrary) (M : Arbitrary) :
    Arbitrary with type t = M.t P.t = struct
      type t = M.t P.t
      let to_string t = P.to_string M.to_string t
      let gen ?size rs = P.gen M.gen ?size rs
    end
    
  module ComposeGenComparable
    (P : Poly.ArbitraryComparable) 
    (M : ArbitraryComparable) 
    : ArbitraryComparable with type t = M.t P.t = struct
      include ComposeGen(P)(M) 
      let compare x y = P.compare M.compare x y
    end

  module ComparablePair(M1 : Comparable)(M2 : Comparable) 
    : Comparable with type t = M1.t * M2.t = 
  struct
    type t = M1.t * M2.t
    let compare (x1,x2) (y1,y2) = 
      cmp2 (M1.compare x1 y1) M2.compare x2 y2
	
    let to_string (a,b) = 
      Printf.sprintf "(%s, %s)" (M1.to_string a) (M2.to_string b)
  end
    
  module Comparable3Tuple(M1 : Comparable)(M2 : Comparable)(M3 : Comparable) :
    Comparable with type t = M1.t * M2.t * M3.t = 
  struct
    type t = M1.t * M2.t * M3.t
    let compare (x1,x2,x3) (y1,y2,y3) = 
      cmp2 (cmp2 (M1.compare x1 y1) M2.compare x2 y2) M3.compare x3 y3
	
    let to_string (a,b,c) = 
      Printf.sprintf "(%s, %s, %s)"
	(M1.to_string a) (M2.to_string b) (M3.to_string c)
  end

  module GenPair(A : Arbitrary)(B : Arbitrary) :
    Arbitrary with type t = A.t * B.t =
  struct
    type t = A.t * B.t
    let gen ?size r = A.gen ?size r, B.gen ?size r
    let to_string (a,b) = 
      Printf.sprintf "(%s, %s)" (A.to_string a) (B.to_string b)
  end
    
  module Gen3Tuple(A : Arbitrary)(B : Arbitrary)(C : Arbitrary) :
    Arbitrary with type t = A.t * B.t * C.t =
  struct
    type t = (A.t * B.t * C.t)
    let gen ?size r = A.gen ?size r, B.gen ?size r, C.gen ?size r
    let to_string (a,b,c) =
      Printf.sprintf "(%s, %s, %s)"
      (A.to_string a) (B.to_string b) (C.to_string c)
  end

end

(** Base Types *)
module type Integral = sig
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

module Int = struct
  type t = int
  let zero = 0
  let one = 1
  let minus_one = -1

  let abs = Pervasives.abs
  let neg = ( ~- )
  let succ = Pervasives.succ
  let pred = Pervasives.pred

  let add = (+)
  let sub = (-)
  let mul = ( * )
  let div = ( / )
  let rem x y = x mod y

  let logxor x y = x lxor y
  let logand x y = x land y
  let lognot x = lnot x
  let logor x y = x lor y

  let shift_left x y = x lsl y
  let shift_right x y = x asr y
  let shift_right_logical x y = x lsr y
  let of_int x = x
  let to_int x = x
  let of_float = Pervasives.int_of_float
  let to_float = Pervasives.float_of_int
  let to_string = Pervasives.string_of_int
  let of_string = Pervasives.int_of_string

  let compare (x:int) (y:int) = Pervasives.compare x y
  let equal x y = (compare x y) = 0
  let hash x = x
  let to_string x = string_of_int x
  let gen ?(size=max_int) r = 
    let nsize = Nativeint.of_int size in
    let rand = Random.State.nativeint r nsize in
      Nativeint.to_int rand
end

module Float = struct
  type t = float
  let compare (x:float) (y:float) = compare x y
  let equal x y = (compare x y) = 0
  let hash x = Hashtbl.hash x
  let gen ?(size=max_int) r = Random.State.float r (float size)
  let to_string = string_of_float
end

module Bool = struct
  type t = bool
  let compare (x:bool) (y:bool) = compare x y
  let equal x y = (compare x y) = 0
  let hash x = Hashtbl.hash x
  let gen ?size r = Random.State.bool r
  let to_string = string_of_bool
end

module Char = struct
  type t = char
  let compare (x:char) (y:char) = compare x y
  let equal x y = (compare x y) = 0
  let hash x = Hashtbl.hash x
  let gen ?(size=256) r = Char.chr (Random.State.int r (size mod 256))
  let to_string c = String.make 1 c
end

module Int32 = struct
  include Int32
  let equal x y = (compare x y) = 0
  let hash x = Hashtbl.hash x
  let gen ?size r = Random.State.int32 r Int32.max_int
end

module Int64 = struct
  include Int64 
  let equal x y = (compare x y) = 0
  let hash x = Hashtbl.hash x
  let gen ?size r = Random.State.int64 r Int64.max_int
end

module Nativeint = struct
  include Nativeint
  let equal x y = (compare x y) = 0
  let hash x = Hashtbl.hash x
  let gen ?size r = Random.State.nativeint r Nativeint.max_int
end

module Big_int = struct
  include Big_int
  type t = big_int
  let equal x y = (compare_big_int x y) = 0
  let compare = eq_big_int
  let hash x = Hashtbl.hash x
  let gen ?size r = 
    Big_int.big_int_of_string
      (Int64.to_string (Random.State.int64 r Int64.max_int))

  let zero = zero_big_int
  let one = unit_big_int
  let minus_one = minus_big_int one

  let abs = abs_big_int
  let neg = minus_big_int
  let succ = succ_big_int
  let pred = pred_big_int

  let add = add_big_int
  let sub = sub_big_int
  let mul = mult_big_int
  let div = div_big_int
  let rem = mod_big_int

(*
  let logxor = ( lxor )
  let logand = ( land )
  let lognot = ( lnot )
  let logor = ( lor )

  let shift_left = ( lsl )
  let shift_right = ( asr )
  let shift_right_logical = ( lsr )
*)

  let of_int x = big_int_of_int
  let to_int x = int_of_big_int
  let of_float f = big_int_of_string (string_of_float (floor f))
  let to_float = float_of_big_int
  let to_string = string_of_big_int
  let of_string = big_int_of_string

end

module Ratio = struct
  include Ratio
  let equal x y = (compare x y) = 0
  let hash x = Hashtbl.hash x
  let gen ?size r = Ratio.create_ratio (Big_int.gen r) (Big_int.gen r)
end

module Complex = struct
  include Complex
  let equal x y = (compare x y) = 0
  let hash x = Hashtbl.hash x
  let gen ?size r = {Complex.re = Float.gen r; im = Float.gen r}
end

module String = struct
  include String
  let equal x y = (compare x y) = 0
  let hash x = Hashtbl.hash x
  let gen ?(size=100) rs = 
    let len = (Random.State.int rs size) mod Sys.max_string_length in
    let s = String.create len in
      for i = 0 to (len-1) do
	s.[i] <- Char.gen ~size:size rs
      done;
      s
  let to_string x = x
end

let _ = 
  let module Test1 = (Int : Integral) in
  let module Test2 = (Int32 : Integral) in
  let module Test3 = (Int64 : Integral) in
  let module Test4 = (Nativeint : Integral) in
(*  let module Test5 = (Big_int : Integral) in
    missing logical ops... :-(
*)
    ()


module Option = struct
  type 'a t = 'a option

  let compare cmp x y = match x,y with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some a, Some b -> cmp a b

  let equal x y = (compare Pervasives.compare x y) = 0

  let gen (gen:?size:int -> Random.State.t -> 'a) ?size r : 'a option = 
    if Random.State.bool r 
    then None
    else Some (gen ?size r)

  let to_string to_s = function
    | None -> "None"
    | Some x -> "Some " ^ (to_s x)
end


(* CR SW: There's some room for code sharing, both at the module type level and
   at the functor level.  First, there is a technique by which one can get
   multiple interface inheritance.  This would allow you to mix and match all of
   the various signatures (comparable, hashable, equality, ...) to write down
   an explicit signature that describes a module that meets some subset of them.

   The trick is to *not* use "t" when defining "abstract" signatures
   (characterizing some aspect of behavior), and instead to use the name of the
   of the behavior as the name of the type.  For example, one could do:

module type MonoEquatable = sig
  type equatable
  val equal : equatable -> equatable -> bool
end

module type MonoHashable = sig
  type hashable
  val hash : hashable -> int
end

   Then, whenever you have a type t in some signature that you want to have
   a particaular behavior, you do "include Behavior with type behavior = t".
   For example, for a monotype that supports equal and hash, you could do.

module type Z = sig
  type t
  include MonoEquatable with type equatable = t
  include MonoHashable with type hashable = t
end

*)


