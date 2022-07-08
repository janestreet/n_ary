(*_ This file is automatically generated.  Do not edit it directly. *)

(** Defines a variant type with 3 unary constructors. We refer to each constructor as a
    "case". Cases are numbered consecutively starting at zero. *)

open! Base

type ('a0, 'a1, 'a2) t =
  | Case0 of 'a0
  | Case1 of 'a1
  | Case2 of 'a2
[@@deriving compare, equal, hash, sexp_of]

(** Constructors *)

val case0 : 'a -> ('a, _, _) t
val case1 : 'a -> (_, 'a, _) t
val case2 : 'a -> (_, _, 'a) t

(** Predicates *)

val is_case0 : _ t -> bool
val is_case1 : _ t -> bool
val is_case2 : _ t -> bool

(** Accessors *)

val get_case0 : ('a, _, _) t -> 'a option
val get_case1 : (_, 'a, _) t -> 'a option
val get_case2 : (_, _, 'a) t -> 'a option

(** Accessors that raise *)

val get_case0_exn : ('a, _, _) t -> 'a
val get_case1_exn : (_, 'a, _) t -> 'a
val get_case2_exn : (_, _, 'a) t -> 'a

(** Map all cases *)
val map
  :  ('a0, 'a1, 'a2) t
  -> f0:('a0 -> 'b0)
  -> f1:('a1 -> 'b1)
  -> f2:('a2 -> 'b2)
  -> ('b0, 'b1, 'b2) t

(** Map individual cases *)

val map_case0 : ('b, 'a1, 'a2) t -> f:('b -> 'c) -> ('c, 'a1, 'a2) t
val map_case1 : ('a0, 'b, 'a2) t -> f:('b -> 'c) -> ('a0, 'c, 'a2) t
val map_case2 : ('a0, 'a1, 'b) t -> f:('b -> 'c) -> ('a0, 'a1, 'c) t
