(* See comment in [bench_path.ml]. *)

open! Core

type t = File_path.Part.t
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp, sexp_grammar]

let arg_type = File_path.Part.arg_type
let dot = File_path.Part.dot
let dot_dot = File_path.Part.dot_dot

include (
  File_path.Part :
  sig
  @@ portable
    include
      Identifiable.S
      with type t := t
       and type comparator_witness = File_path.Part.comparator_witness
  end)

let%bench_fun "equal =" =
  let x = Sys.opaque_identity (of_string "foo") in
  let y = Sys.opaque_identity (of_string "foo") in
  fun () -> equal x y
;;

let%bench_fun "equal <>" =
  let x = Sys.opaque_identity dot in
  let y = Sys.opaque_identity (of_string "foo") in
  fun () -> equal x y
;;

let%bench_fun "compare =" =
  let x = Sys.opaque_identity dot in
  let y = Sys.opaque_identity dot in
  fun () -> compare x y
;;

let%bench_fun "compare <" =
  let x = Sys.opaque_identity dot in
  let y = Sys.opaque_identity dot_dot in
  fun () -> compare x y
;;

let%bench_fun "compare >" =
  let x = Sys.opaque_identity dot_dot in
  let y = Sys.opaque_identity dot in
  fun () -> compare x y
;;

let%bench_fun "of_string" =
  let string = Sys.opaque_identity "foobie" in
  fun () -> of_string string
;;

let%bench_fun "to_string" =
  let t = Sys.opaque_identity (of_string "foobie") in
  fun () -> to_string t
;;

let%bench_fun "t_of_sexp" =
  let string = Sys.opaque_identity (Sexp.Atom "foobie") in
  fun () -> t_of_sexp string
;;

let%bench_fun "sexp_of_t" =
  let t = Sys.opaque_identity (of_string "foobie") in
  fun () -> sexp_of_t t
;;

module Expert = struct
  let unchecked_of_canonical_string = File_path.Part.Expert.unchecked_of_canonical_string

  let%bench_fun "unchecked_of_canonical_string" =
    let string = Sys.opaque_identity "foobie" in
    fun () -> unchecked_of_canonical_string string
  ;;
end

let invariant = File_path.Part.invariant

let%bench_fun "invariant" =
  let t = Sys.opaque_identity (of_string "foobie") in
  fun () -> invariant t
;;

let append_to_basename_exn = File_path.Part.append_to_basename_exn

let%bench_fun "append_to_basename_exn, empty" =
  let t = Sys.opaque_identity (of_string "foo") in
  let suffix = Sys.opaque_identity "" in
  fun () -> append_to_basename_exn t suffix
;;

let%bench_fun "append_to_basename_exn, nonempty" =
  let t = Sys.opaque_identity (of_string "foo") in
  let suffix = Sys.opaque_identity ".bar" in
  fun () -> append_to_basename_exn t suffix
;;
