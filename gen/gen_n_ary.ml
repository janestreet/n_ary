open! Base

module Size = struct
  type t = { n : int } [@@unboxed]

  let min = 2
  let max = 16
  let of_int n = { n }
  let to_string { n } = Int.to_string n
  let all = List.range min max ~start:`inclusive ~stop:`inclusive |> List.map ~f:of_int
  let for_each { n } ~f = List.map (List.range 0 n) ~f:(fun i -> f (Int.to_string i))
end

module Kind = struct
  type t =
    (* n-ary types *)
    | Enum
    | Tuple
    | Variant
    (* types with n-ary operations *)
    | List
  [@@deriving enumerate]

  let to_file_prefix = function
    | Enum -> "enum"
    | Tuple -> "tuple"
    | Variant -> "variant"
    | List -> "list"
  ;;

  let to_module_prefix = function
    | Enum -> "Enum"
    | Tuple -> "Tuple"
    | Variant -> "Variant"
    | List -> "List"
  ;;
end

module Suffix = struct
  type t =
    | MLI
    | ML
  [@@deriving enumerate]

  let to_dotted_string = function
    | MLI -> ".mli"
    | ML -> ".ml"
  ;;
end

module File = struct
  type t =
    | Jbuild
    | N_ary
    | Submodule of
        { kind : Kind.t
        ; size : Size.t
        ; suffix : Suffix.t
        }
  [@@deriving enumerate]

  let to_file_name = function
    | Jbuild -> "jbuild"
    | N_ary -> "n_ary.ml"
    | Submodule { kind; size; suffix } ->
      [ Kind.to_file_prefix kind; Size.to_string size; Suffix.to_dotted_string suffix ]
      |> String.concat
  ;;
end

let case i = [%string "Case%{i}"]
let alpha i = [%string "'a%{i}"]
let beta i = [%string "'b%{i}"]
let parens s = [%string "(%{s})"]
let params tys = tys |> String.concat ~sep:", " |> parens
let product tys = tys |> String.concat ~sep:" * " |> parens
let arrow tys = tys |> String.concat ~sep:" -> "
let list tys = List.map tys ~f:(fun ty -> [%string "%{ty} list"])
let alt pats = pats |> String.concat ~sep:" | " |> parens
let tuple exprs = exprs |> String.concat ~sep:", " |> parens
let spaced = String.concat ~sep:" "
let lined = String.concat ~sep:"\n"

let jbuild_contents () =
  let gen_nary_exe = "../gen/gen_n_ary.exe" in
  let apply_style = "%{root}/bin/apply-style" in
  let action =
    [ [%string {|%{gen_nary_exe} %{"%"}{target}|}]
    ; [%string {|%{apply_style} %{"%"}{target} -directory-config jbuild -in-place|}]
    ]
    |> lined
  in
  let rules =
    List.map File.all ~f:(fun file ->
      let file_name = File.to_file_name file in
      let deps =
        match (file : File.t) with
        | Jbuild -> [%string "%{gen_nary_exe} %{apply_style}"]
        | _ -> [%string "%{gen_nary_exe} %{apply_style} jbuild"]
      in
      [%string
        {|
          (rule
           ((targets (%{file_name}))
            (deps (%{deps}))
            (action "
%{action}
")
            (:public_release_only (mode fallback))
))
        |}])
    |> spaced
  in
  [%string
    {|
(library
  ((name n_ary)
   (public_name n_ary)
   (libraries (base))
   (preprocess (pps (ppx_compare ppx_enumerate ppx_hash ppx_sexp_conv ppx_sexp_message)))
   (js_of_ocaml ())
   (no_ocaml_extensions true)))
%{rules}
  |}]
  |> String.lstrip
;;

let n_ary_contents () =
  List.concat_map Kind.all ~f:(fun kind ->
    [%string {|
(** %{Kind.to_module_prefix kind} modules *)
|}]
    :: List.map Size.all ~f:(fun size ->
      let name = Kind.to_module_prefix kind ^ Size.to_string size in
      [%string "module %{name} = %{name}"]))
  |> lined
;;

module type S = sig
  val mli : Size.t -> string
  val ml : Size.t -> string
end

module Enum : S = struct
  let ty size = Size.for_each size ~f:(fun i -> [%string "| %{case i}"]) |> spaced

  let mli size =
    let n = Size.to_string size in
    let constants = Size.for_each size ~f:(fun i -> [%string "val case%{i} : t"]) in
    let predicates =
      Size.for_each size ~f:(fun i -> [%string "val is_case%{i} : t -> bool"])
    in
    [%string
      {|
(** Defines an enumeration type with %{n} constructors. We refer to each constructor as a
    "case". Cases and their respective [int] values are numbered consecutively starting at
    zero. *)

open! Base

type t = %{ty size}
[@@deriving compare, enumerate, equal, hash, sexp_of]

(** Constants **)

%{lined constants}

(** Predicates *)

%{lined predicates}

(** Conversions to and from [int] **)

val to_int : t -> int
val of_int : int -> t option
val of_int_exn : int -> t
|}]
  ;;

  let ml size =
    let n = Size.to_string size in
    let to_int_clauses =
      Size.for_each size ~f:(fun i -> [%string "| %{case i} -> %{i}"])
    in
    let of_int_clauses =
      Size.for_each size ~f:(fun i -> [%string "| %{i} -> Some %{case i}"])
    in
    let of_int_exn_clauses =
      Size.for_each size ~f:(fun i -> [%string "| %{i} -> %{case i}"])
    in
    let constants =
      Size.for_each size ~f:(fun i -> [%string "let case%{i} = %{case i}"])
    in
    let predicates =
      Size.for_each size ~f:(fun i ->
        [%string "let is_case%{i} = function %{case i} -> true | _ -> false"])
    in
    [%string
      {|
open! Base

type t = %{ty size}
[@@deriving compare, enumerate, equal, hash, sexp_of]

(* constants *)

%{lined constants}

(* predicates *)

%{lined predicates}

(* int conversion *)

let to_int = function %{spaced to_int_clauses}
let of_int = function %{spaced of_int_clauses} | _ -> None

let of_int_exn = function
  %{spaced of_int_exn_clauses}
  | n -> raise_s [%message "N_ary.Enum%{n}.of_int_exn: invalid input" ~_:(n : int)]
|}]
  ;;
end

module Tuple : S = struct
  let mli size =
    let n = Size.to_string size in
    let alphas = Size.for_each size ~f:alpha in
    let betas = Size.for_each size ~f:beta in
    let alpha_at i =
      Size.for_each size ~f:(fun j -> if String.equal i j then "'a" else "_")
    in
    let alphas_except_at i =
      Size.for_each size ~f:(fun j -> if String.equal i j then "_" else alpha j)
    in
    let getters =
      Size.for_each size ~f:(fun i ->
        [%string "val part%{i} : %{params (alpha_at i)} t -> 'a"])
    in
    let setters =
      Size.for_each size ~f:(fun i ->
        [%string
          "val set_part%{i} : %{params (alphas_except_at i)} t -> %{alpha i} -> \
           %{params alphas} t"])
    in
    let fns =
      Size.for_each size ~f:(fun i -> [%string "f%{i}:(%{alpha i} -> %{beta i})"])
    in
    let map_parts =
      Size.for_each size ~f:(fun i ->
        let with_ s =
          Size.for_each size ~f:(fun j -> if String.equal i j then s else alpha j)
        in
        let with_beta = params (with_ "'b") in
        let with_gamma = params (with_ "'c") in
        [%string "val map_part%{i} : %{with_beta} t -> f:('b -> 'c) -> %{with_gamma} t"])
    in
    [%string
      {|
(** Defines a tuple type containing %{n} values. We refer to each value as a "part". Parts
    are numbered consecutively beginning at zero. *)

open! Base

type %{params alphas} t = %{product alphas}
[@@deriving compare, equal, hash, sexp_of]

(** Constructor *)
val create : %{arrow alphas} -> %{params alphas} t

(** Accessors *)

%{lined getters}

(** Functional update *)

%{lined setters}

(** Map all parts *)
val map : %{params alphas} t -> %{arrow fns} -> %{params betas} t

(** Map individual parts *)

%{lined map_parts}
|}]
  ;;

  let ml size =
    let alphas = Size.for_each size ~f:alpha in
    let xs = Size.for_each size ~f:(fun i -> [%string "x%{i}"]) in
    let fxs = Size.for_each size ~f:(fun i -> [%string "f%{i} x%{i}"]) in
    let getters =
      Size.for_each size ~f:(fun i ->
        let pats =
          Size.for_each size ~f:(fun j -> if String.equal i j then "x" else "_")
        in
        [%string "let part%{i} %{tuple pats} = x"])
    in
    let setters =
      Size.for_each size ~f:(fun i ->
        let pats =
          Size.for_each size ~f:(fun j ->
            if String.equal i j then "_" else [%string "x%{j}"])
        in
        let exps =
          Size.for_each size ~f:(fun j ->
            if String.equal i j then "y" else [%string "x%{j}"])
        in
        [%string "let set_part%{i} %{tuple pats} y = %{tuple exps}"])
    in
    let fs = Size.for_each size ~f:(fun i -> [%string "~f%{i}"]) in
    let map_parts =
      Size.for_each size ~f:(fun i ->
        let with_fx =
          Size.for_each size ~f:(fun j ->
            if String.equal i j then [%string "f x%{j}"] else [%string "x%{j}"])
        in
        [%string "let map_part%{i} %{tuple xs} ~f = %{tuple with_fx}"])
    in
    [%string
      {|
open! Base

type %{params alphas} t = %{product alphas}
[@@deriving compare, equal, hash, sexp_of]

(* constructor *)

let create %{spaced xs} = %{tuple xs}

(* accessors *)

%{lined getters}

(* functional update *)

%{lined setters}

(* map all parts *)

let map %{tuple xs} %{spaced fs} = %{tuple fxs}

(* map each part *)

%{lined map_parts}
|}]
  ;;
end

module Variant : S = struct
  let mli size =
    let n = Size.to_string size in
    let alphas = Size.for_each size ~f:alpha in
    let betas = Size.for_each size ~f:beta in
    let alpha_at i =
      Size.for_each size ~f:(fun j -> if String.equal i j then "'a" else "_")
    in
    let ty = Size.for_each size ~f:(fun i -> [%string "| %{case i} of %{alpha i}"]) in
    let cases =
      Size.for_each size ~f:(fun i ->
        [%string "val case%{i} : 'a -> %{params (alpha_at i)} t"])
    in
    let predicates =
      Size.for_each size ~f:(fun i -> [%string "val is_case%{i} : _ t -> bool"])
    in
    let gets =
      Size.for_each size ~f:(fun i ->
        [%string "val get_case%{i} : %{params (alpha_at i)} t -> 'a option"])
    in
    let get_exns =
      Size.for_each size ~f:(fun i ->
        [%string "val get_case%{i}_exn : %{params (alpha_at i)} t -> 'a"])
    in
    let fns =
      Size.for_each size ~f:(fun i -> [%string "f%{i}:(%{alpha i} -> %{beta i})"])
    in
    let map_cases =
      Size.for_each size ~f:(fun i ->
        let with_ s =
          Size.for_each size ~f:(fun j -> if String.equal i j then s else alpha j)
        in
        let with_beta = params (with_ "'b") in
        let with_gamma = params (with_ "'c") in
        [%string "val map_case%{i} : %{with_beta} t -> f:('b -> 'c) -> %{with_gamma} t"])
    in
    [%string
      {|
(** Defines a variant type with %{n} unary constructors. We refer to each constructor as a
    "case". Cases are numbered consecutively starting at zero. *)

open! Base

type %{params alphas} t = %{spaced ty}
[@@deriving compare, equal, hash, sexp_of]

(** Constructors *)

%{lined cases}

(** Predicates *)

%{lined predicates}

(** Accessors *)

%{lined gets}

(** Accessors that raise *)

%{lined get_exns}

(** Map all cases *)
val map : %{params alphas} t -> %{arrow fns} -> %{params betas} t

(** Map individual cases *)

%{lined map_cases}
|}]
  ;;

  let ml size =
    let n = Size.to_string size in
    let alphas = Size.for_each size ~f:alpha in
    let ty = Size.for_each size ~f:(fun i -> [%string "| %{case i} of %{alpha i}"]) in
    let cases =
      Size.for_each size ~f:(fun i -> [%string "let case%{i} x = %{case i} x"])
    in
    let predicates =
      Size.for_each size ~f:(fun i ->
        [%string "let is_case%{i} = function %{case i} _ -> true | _ -> false"])
    in
    let gets =
      Size.for_each size ~f:(fun i ->
        [%string "let get_case%{i} = function %{case i} x -> Some x | _ -> None"])
    in
    let get_exns =
      Size.for_each size ~f:(fun i ->
        [%string
          {|
let get_case%{i}_exn = function
  | %{case i} x -> x
  | _ -> raise_s [%message "N_ary.Variant%{n}.get_case%{i}_exn: invalid input"]
|}])
    in
    let fns = Size.for_each size ~f:(fun i -> [%string "~f%{i}"]) in
    let map_clauses =
      Size.for_each size ~f:(fun i ->
        [%string "| %{case i} x%{i} -> %{case i} (f%{i} x%{i})"])
    in
    let map_cases =
      Size.for_each size ~f:(fun i ->
        let other_cases =
          Size.for_each size ~f:(fun j ->
            if String.equal i j then None else Some [%string "%{case j} _"])
          |> List.filter_opt
        in
        [%string
          {|
let map_case%{i} t ~f =
  match t with
  | %{case i} x -> %{case i} (f x)
  | %{alt other_cases} as t -> t
|}])
    in
    [%string
      {|
open! Base

type %{params alphas} t = %{spaced ty}
[@@deriving compare, equal, hash, sexp_of]

(* constructors *)

%{lined cases}

(* predicates *)

%{lined predicates}

(* accessors *)

%{lined gets}

(* raising accessors *)

%{lined get_exns}

(* map all cases *)

let map t %{spaced fns} = match t with %{spaced map_clauses}

(* map each case *)

%{lined map_cases}
|}]
  ;;
end

module List_n_ary : S = struct
  let mli size =
    let n = Size.to_string size in
    let n_alphas = Size.for_each size ~f:(fun _ -> "'a") in
    let alphas = Size.for_each size ~f:alpha in
    let betas = Size.for_each size ~f:beta in
    let underscores = Size.for_each size ~f:(Fn.const "_") in
    [%string
      {|
(** Defines list operations that produce or consume %{n} lists at once. *)

open! Base

(** {2 Partitioning Lists}

    These functions produce many lists from one by assigning each result
    from the input to one of %{n} buckets. *)

(** Transforms an input list into a tuple of lists. The Nth list is the elements of the
    input for which [f] returns [CaseN], in the order they occur in the input. *)
val partition_enum : 'a list -> f:('a -> Enum%{n}.t) -> %{product (list n_alphas)}

(** Like [partition_enum]. [f] is also passed the index of the current list item. *)
val partition_enumi : 'a list -> f:(int -> 'a -> Enum%{n}.t) -> %{product (list n_alphas)}

(** Transforms an input list into a tuple of lists. [f] is applied to every element of
    the input. For every result of the form [CaseN x], the Nth output list contains [x]
    in the same order that the inputs occurred. *)
val partition_map
  : 'a list -> f:('a -> %{params betas} Variant%{n}.t) -> %{product (list betas)}

(** Like [partition_map]. [f] is also passed the index of the current list item. *)
val partition_mapi
  : 'a list -> f:(int -> 'a -> %{params betas} Variant%{n}.t) -> %{product (list betas)}

(** {2 Unzipping Lists}

    These functions produces many lists from one by taking each part from a tuple
    into a separate list. *)

(** Transform a list of %{n}-tuples into a tuple of lists. *)
val unzip : %{product alphas} list -> %{product (list alphas)}

(** {2 Zipping Lists}

    These functions produce one list from many by processing multiple corresponding
    elements at once to produce a single result. *)

(** {3 Returning Unequal Lengths}

    These functions detect unequal lengths before they begin processing list elements,
    and return either [Ok _] or [Unequal_lengths]. *)

(** Transform %{n} lists of equal length into a single list of %{n}-tuples.  Returns
    [Unequal_lengths] if the input lists don't all have the same length. *)
val zip : %{arrow (list alphas)} -> %{product alphas} list List.Or_unequal_lengths.t

(** Map a function over %{n} lists simultaneously. If the input lists don't all have the
    same length, returns [Unequal_lengths] without calling [f] on any elements. *)
val map
  :  %{arrow (list alphas)}
  -> f:(%{arrow alphas} -> 'b)
  -> 'b list List.Or_unequal_lengths.t

(** Like [map]. [f] is also passed the index of the current list item. *)
val mapi
  :  %{arrow (list alphas)}
  -> f:(int -> %{arrow alphas} -> 'b)
  -> 'b list List.Or_unequal_lengths.t

(** Like [map]. Ignores the result; used for side-effecting functions. *)
val iter
  :  %{arrow (list alphas)}
  -> f:(%{arrow alphas} -> unit)
  -> unit List.Or_unequal_lengths.t

(** Like [iter]. [f] is also passed the index of the current list item. *)
val iteri
  :  %{arrow (list alphas)}
  -> f:(int -> %{arrow alphas} -> unit)
  -> unit List.Or_unequal_lengths.t

(** Returns [Ok ()] if the input lists all have the same length. Returns
    [Unequal_lengths] otherwise. *)
val check_equal_lengths : %{arrow (list underscores)} -> unit List.Or_unequal_lengths.t

(** {3 Raising on Unequal Lengths}

    These functions begin processing list elements immediately. If one or more lists
    end before the others, they raise an exception. *)

(** Like [zip], but raises if the input lists don't all have the same length. *)
val zip_exn : %{arrow (list alphas)} -> %{product alphas} list

(** Like [map]. Raises if the input lists don't all have the same length. May call [f]
    before raising. *)
val map_exn
  :  %{arrow (list alphas)}
  -> f:(%{arrow alphas} -> 'b)
  -> 'b list

(** Like [map_exn]. [f] is also passed the index of the current list item. *)
val mapi_exn
  :  %{arrow (list alphas)}
  -> f:(int -> %{arrow alphas} -> 'b)
  -> 'b list

(** Like [iter]. Raises if the input lists don't all have the same length.
    May call [f] before raising. *)
val iter_exn
  :  %{arrow (list alphas)}
  -> f:(%{arrow alphas} -> unit)
  -> unit

(** Like [iter_exn]. [f] is also passed the index of the current list item. *)
val iteri_exn
  :  %{arrow (list alphas)}
  -> f:(int -> %{arrow alphas} -> unit)
  -> unit
|}]
  ;;

  let ml size =
    let n = Size.to_string size in
    let nils = Size.for_each size ~f:(fun _ -> "[]") in
    let conses = Size.for_each size ~f:(fun i -> [%string "(x%{i} :: l%{i})"]) in
    let cons_l_pats = Size.for_each size ~f:(fun i -> [%string "(_ :: l%{i})"]) in
    let xs = Size.for_each size ~f:(fun i -> [%string "x%{i}"]) in
    let ls = Size.for_each size ~f:(fun i -> [%string "l%{i}"]) in
    let partition_mapi_clauses =
      Size.for_each size ~f:(fun i ->
        [%string
          "| %{case i} y -> Tuple%{n}.set_part%{i} acc (y :: Tuple%{n}.part%{i} acc)"])
    in
    let f_revs = Size.for_each size ~f:(fun i -> [%string "~f%{i}:List.rev"]) in
    let enumi_clauses =
      Size.for_each size ~f:(fun i -> [%string "| %{case i} -> %{case i} x"]) |> lined
    in
    [%string
      {|
open! Base

let partition_mapi list ~f =
  List.foldi
    list
    ~init:%{tuple nils}
    ~f:(fun i acc x ->
      match (f i x : _ Variant%{n}.t) with
      %{lined partition_mapi_clauses})
  |> Tuple%{n}.map %{spaced f_revs}
;;

let partition_map list ~f = partition_mapi list ~f:(fun _ x -> f x)

let partition_enumi list ~f =
  partition_mapi list ~f:(fun i x ->
    match (f i x : Enum%{n}.t) with
    %{enumi_clauses})
;;

let partition_enum list ~f = partition_enumi list ~f:(fun _ x -> f x)

let unzip list =
  List.fold_right
    list
    ~init:%{tuple nils}
    ~f:(fun %{tuple xs} %{tuple ls} -> %{tuple conses})
;;

let zip_exn =
  let rec zip_loop %{spaced ls} acc =
    match %{tuple ls} with
    | %{tuple nils} -> List.rev acc
    | %{tuple conses} -> zip_loop %{spaced ls} (%{tuple xs} :: acc)
    | _ -> raise_s [%message "N_ary.List%{n}.zip_exn: lists have unequal lengths"]
  in
  fun %{spaced ls} -> zip_loop %{spaced ls} []
;;

let map_exn =
  let rec map_loop %{spaced ls} ~f acc =
    match %{tuple ls} with
    | %{tuple nils} -> List.rev acc
    | %{tuple conses} -> map_loop %{spaced ls} ~f (f %{spaced xs} :: acc)
    | _ -> raise_s [%message "N_ary.List%{n}.map_exn: lists have unequal lengths"]
  in
  fun %{spaced ls} ~f -> map_loop %{spaced ls} ~f []
;;

let mapi_exn =
  let rec mapi_loop %{spaced ls} ~f i acc =
    match %{tuple ls} with
    | %{tuple nils} -> List.rev acc
    | %{tuple conses} -> mapi_loop %{spaced ls} ~f (i+1) (f i %{spaced xs} :: acc)
    | _ -> raise_s [%message "N_ary.List%{n}.mapi_exn: lists have unequal lengths"]
  in
  fun %{spaced ls} ~f -> mapi_loop %{spaced ls} ~f 0 []
;;

let iter_exn =
  let rec iter_loop %{spaced ls} ~f =
    match %{tuple ls} with
    | %{tuple nils} -> ()
    | %{tuple conses} -> f %{spaced xs}; iter_loop %{spaced ls} ~f
    | _ -> raise_s [%message "N_ary.List%{n}.iter_exn: lists have unequal lengths"]
  in
  fun %{spaced ls} ~f -> iter_loop %{spaced ls} ~f
;;

let iteri_exn =
  let rec iteri_loop %{spaced ls} ~f i =
    match %{tuple ls} with
    | %{tuple nils} -> ()
    | %{tuple conses} -> f i %{spaced xs}; iteri_loop %{spaced ls} ~f (i+1)
    | _ -> raise_s [%message "N_ary.List%{n}.iteri_exn: lists have unequal lengths"]
  in
  fun %{spaced ls} ~f -> iteri_loop %{spaced ls} ~f 0
;;

let rec check_equal_lengths %{spaced ls} =
  match %{tuple ls} with
  | %{tuple nils} -> List.Or_unequal_lengths.Ok ()
  | %{tuple cons_l_pats} -> check_equal_lengths %{spaced ls}
  | _ -> List.Or_unequal_lengths.Unequal_lengths
;;

let zip %{spaced ls} =
  match check_equal_lengths %{spaced ls} with
  | Unequal_lengths as x -> x
  | Ok () -> Ok (zip_exn %{spaced ls})
;;

let map %{spaced ls} ~f =
  match check_equal_lengths %{spaced ls} with
  | Unequal_lengths as x -> x
  | Ok () -> Ok (map_exn %{spaced ls} ~f)
;;

let mapi %{spaced ls} ~f =
  match check_equal_lengths %{spaced ls} with
  | Unequal_lengths as x -> x
  | Ok () -> Ok (mapi_exn %{spaced ls} ~f)
;;

let iter %{spaced ls} ~f =
  match check_equal_lengths %{spaced ls} with
  | Unequal_lengths as x -> x
  | Ok () -> Ok (iter_exn %{spaced ls} ~f)
;;

let iteri %{spaced ls} ~f =
  match check_equal_lengths %{spaced ls} with
  | Unequal_lengths as x -> x
  | Ok () -> Ok (iteri_exn %{spaced ls} ~f)
;;
|}]
  ;;
end

let comment ~file message =
  match (file : File.t) with
  | Jbuild -> [%string "#| %{message} |#"]
  | Submodule { suffix = MLI; _ } -> [%string "(*_ %{message} *)"]
  | Submodule { suffix = ML; _ } | N_ary -> [%string "(* %{message} *)"]
;;

let header = {|This file is automatically generated.  Do not edit it directly.|}

let choose kind : (module S) =
  match (kind : Kind.t) with
  | Enum -> (module Enum)
  | Tuple -> (module Tuple)
  | Variant -> (module Variant)
  | List -> (module List_n_ary)
;;

let file_contents file =
  let body =
    match (file : File.t) with
    | Jbuild -> jbuild_contents ()
    | N_ary -> n_ary_contents ()
    | Submodule { kind; size; suffix } ->
      let (module M) = choose kind in
      (match suffix with
       | MLI -> M.mli size
       | ML -> M.ml size)
  in
  lined [ comment ~file header; body ]
;;

let print_usage () =
  let self = Stdlib.Sys.executable_name |> String.split ~on:'/' |> List.last_exn in
  let filename_descriptions =
    List.filter_map File.all ~f:(fun file ->
      match file with
      | Jbuild | N_ary -> Some ("  " ^ File.to_file_name file)
      | Submodule { size = { n = 2 }; suffix = ML; kind = _ } ->
        let filename = File.to_file_name file in
        let generic =
          filename
          |> String.substr_replace_first ~pattern:"2" ~with_:"${n}"
          |> String.substr_replace_first ~pattern:".ml" ~with_:".{ml,mli}"
          |> Printf.sprintf "%-20s"
        in
        let lo = Int.to_string Size.min in
        let hi = Int.to_string Size.max in
        Some [%string "  %{generic} # for n={%{lo}..%{hi}}"]
      | Submodule _ -> None)
  in
  Stdio.print_string
    [%string
      {|Usage:

  %{self} FILENAME

Here, FILENAME is one of:

%{lined filename_descriptions}
|}]
;;

let print_usage_and_fail sexp =
  print_usage ();
  Stdio.print_endline "";
  Stdio.print_s sexp;
  Stdlib.exit 1
;;

let () =
  match Array.to_list (Sys.get_argv ()) with
  | [] | [ _ ] | _ :: _ :: _ :: _ ->
    print_usage_and_fail (Atom "wrong number of arguments")
  | [ _; ("help" | "-h" | "-help" | "--help") ] ->
    (* Help! Not an error, but not writing out a file either. *)
    print_usage ()
  | [ _; file_name ] ->
    (match
       List.find File.all ~f:(fun file -> String.equal file_name (File.to_file_name file))
     with
     | None ->
       print_usage_and_fail
         (Sexp.message "unrecognized file name" [ "file name", sexp_of_string file_name ])
     | Some file ->
       (* Success! *)
       Stdio.Out_channel.write_all file_name ~data:(file_contents file))
;;
