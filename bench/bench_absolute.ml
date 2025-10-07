(* See comment in [bench_path.ml]. *)

open! Core

type t = File_path.Absolute.t
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp, sexp_grammar]

let arg_type = File_path.Absolute.arg_type
let root = File_path.Absolute.root

include (
  File_path.Absolute :
  sig
  @@ portable
    include
      Identifiable.S
      with type t := t
       and type comparator_witness = File_path.Absolute.comparator_witness
  end)

let%bench_fun "equal =" =
  let x = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let y = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> equal x y
;;

let%bench_fun "equal <>" =
  let x = Sys.opaque_identity root in
  let y = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> equal x y
;;

let%bench_fun "compare =" =
  let x = Sys.opaque_identity root in
  let y = Sys.opaque_identity root in
  fun () -> compare x y
;;

let%bench_fun "compare <" =
  let x = Sys.opaque_identity root in
  let y = Sys.opaque_identity (of_string "/.") in
  fun () -> compare x y
;;

let%bench_fun "compare >" =
  let x = Sys.opaque_identity (of_string "/.") in
  let y = Sys.opaque_identity root in
  fun () -> compare x y
;;

let%bench_fun "of_string, canonical" =
  let string = Sys.opaque_identity "/foo/bar/baz" in
  fun () -> of_string string
;;

let%bench_fun "of_string, non-canonical" =
  let string = Sys.opaque_identity "/foo//bar/baz/" in
  fun () -> of_string string
;;

let%bench_fun "to_string" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> to_string t
;;

let%bench_fun "t_of_sexp, canonical" =
  let string = Sys.opaque_identity (Sexp.Atom "/foo/bar/baz") in
  fun () -> t_of_sexp string
;;

let%bench_fun "t_of_sexp, non-canonical" =
  let string = Sys.opaque_identity (Sexp.Atom "/foo//bar/baz/") in
  fun () -> t_of_sexp string
;;

let%bench_fun "sexp_of_t" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> sexp_of_t t
;;

module Expert = struct
  let unchecked_of_canonical_string =
    File_path.Absolute.Expert.unchecked_of_canonical_string
  ;;

  let%bench_fun "unchecked_of_canonical_string" =
    let string = Sys.opaque_identity "/foo/bar/baz" in
    fun () -> unchecked_of_canonical_string string
  ;;
end

let invariant = File_path.Absolute.invariant

let%bench_fun "invariant" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> invariant t
;;

let basename = File_path.Absolute.basename

let%bench_fun "basename, some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> basename t
;;

let%bench_fun "basename, none" =
  let t = Sys.opaque_identity root in
  fun () -> basename t
;;

let basename_exn = File_path.Absolute.basename_exn

let%bench_fun "basename_exn" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> basename_exn t
;;

let basename_or_error = File_path.Absolute.basename_or_error

let%bench_fun "basename_or_error" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> basename_or_error t
;;

let basename_defaulting_to_dot = File_path.Absolute.basename_defaulting_to_dot

let%bench_fun "basename_defaulting_to_dot, name" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> basename_defaulting_to_dot t
;;

let%bench_fun "basename_defaulting_to_dot, dot" =
  let t = Sys.opaque_identity root in
  fun () -> basename_defaulting_to_dot t
;;

let dirname = File_path.Absolute.dirname

let%bench_fun "dirname, some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> dirname t
;;

let%bench_fun "dirname, none" =
  let t = Sys.opaque_identity root in
  fun () -> dirname t
;;

let dirname_exn = File_path.Absolute.dirname_exn

let%bench_fun "dirname_exn" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> dirname_exn t
;;

let dirname_or_error = File_path.Absolute.dirname_or_error

let%bench_fun "dirname_or_error" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> dirname_or_error t
;;

let dirname_defaulting_to_root = File_path.Absolute.dirname_defaulting_to_root

let%bench_fun "dirname_defaulting_to_root, dir" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> dirname_defaulting_to_root t
;;

let%bench_fun "dirname_defaulting_to_root, root" =
  let t = Sys.opaque_identity root in
  fun () -> dirname_defaulting_to_root t
;;

let dirname_and_basename = File_path.Absolute.dirname_and_basename

let%bench_fun "dirname_and_basename, some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> dirname_and_basename t
;;

let%bench_fun "dirname_and_basename, none" =
  let t = Sys.opaque_identity root in
  fun () -> dirname_and_basename t
;;

let append_to_basename_exn = File_path.Absolute.append_to_basename_exn

let%bench_fun "append_to_basename_exn, empty" =
  let t = Sys.opaque_identity (of_string "/foo/bar") in
  let suffix = Sys.opaque_identity "" in
  fun () -> append_to_basename_exn t suffix
;;

let%bench_fun "append_to_basename_exn, nonempty" =
  let t = Sys.opaque_identity (of_string "/foo/bar") in
  let suffix = Sys.opaque_identity ".baz" in
  fun () -> append_to_basename_exn t suffix
;;

let append_part = File_path.Absolute.append_part

let%bench_fun "append_part, root" =
  let t = Sys.opaque_identity root in
  let suffix = Sys.opaque_identity (File_path.Part.of_string "foo") in
  fun () -> append_part t suffix
;;

let%bench_fun "append_part, dir" =
  let t = Sys.opaque_identity (of_string "/foo/bar") in
  let suffix = Sys.opaque_identity (File_path.Part.of_string "baz") in
  fun () -> append_part t suffix
;;

let is_prefix = File_path.Absolute.is_prefix

let%bench_fun "is_prefix, true" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> is_prefix t ~prefix
;;

let%bench_fun "is_prefix, false" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foobie") in
  fun () -> is_prefix t ~prefix
;;

let chop_prefix = File_path.Absolute.chop_prefix

let%bench_fun "chop_prefix, some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> chop_prefix t ~prefix
;;

let%bench_fun "chop_prefix, none" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foobie") in
  fun () -> chop_prefix t ~prefix
;;

let chop_prefix_exn = File_path.Absolute.chop_prefix_exn

let%bench_fun "chop_prefix_exn" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> chop_prefix_exn t ~prefix
;;

let chop_prefix_or_error = File_path.Absolute.chop_prefix_or_error

let%bench_fun "chop_prefix_or_error" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> chop_prefix_or_error t ~prefix
;;

let is_suffix = File_path.Absolute.is_suffix

let%bench_fun "is_suffix, true" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> is_suffix t ~suffix
;;

let%bench_fun "is_suffix, false" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> is_suffix t ~suffix
;;

let chop_suffix = File_path.Absolute.chop_suffix

let%bench_fun "chop_suffix, some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix t ~suffix
;;

let%bench_fun "chop_suffix, none" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> chop_suffix t ~suffix
;;

let chop_suffix_exn = File_path.Absolute.chop_suffix_exn

let%bench_fun "chop_suffix_exn" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix_exn t ~suffix
;;

let chop_suffix_or_error = File_path.Absolute.chop_suffix_or_error

let%bench_fun "chop_suffix_or_error" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix_or_error t ~suffix
;;

let chop_suffix_if_exists = File_path.Absolute.chop_suffix_if_exists

let%bench_fun "chop_suffix_if_exists" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix_if_exists t ~suffix
;;

let append = File_path.Absolute.append

let%bench_fun "append, root" =
  let prefix = Sys.opaque_identity root in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> append prefix suffix
;;

let%bench_fun "append, dir" =
  let prefix = Sys.opaque_identity (of_string "/foo") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> append prefix suffix
;;

let number_of_parts = File_path.Absolute.number_of_parts

let%bench_fun "number_of_parts, root" =
  let t = Sys.opaque_identity root in
  fun () -> number_of_parts t
;;

let%bench_fun "number_of_parts, compound" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> number_of_parts t
;;

let to_parts = File_path.Absolute.to_parts

let%bench_fun "to_parts, root" =
  let t = Sys.opaque_identity root in
  fun () -> to_parts t
;;

let%bench_fun "to_parts, dir" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> to_parts t
;;

let of_parts = File_path.Absolute.of_parts

let%bench_fun "of_parts" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> of_parts parts
;;

let simplify_dot = File_path.Absolute.simplify_dot

let%bench_fun "simplify_dot, unchanged" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> simplify_dot t
;;

let%bench_fun "simplify_dot, changed" =
  let t = Sys.opaque_identity (of_string "/foo/./bar/baz/.") in
  fun () -> simplify_dot t
;;

let simplify_dot_and_dot_dot_naively = File_path.Absolute.simplify_dot_and_dot_dot_naively

let%bench_fun "simplify_dot_and_dot_dot_naively, unchanged" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> simplify_dot_and_dot_dot_naively t
;;

let%bench_fun "simplify_dot_and_dot_dot_naively, changed" =
  let t = Sys.opaque_identity (of_string "/foo/quux/../bar/baz/.") in
  fun () -> simplify_dot_and_dot_dot_naively t
;;
