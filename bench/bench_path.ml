(* The [File_path] library has been constructed carefully with respect to performance,
   especially allocation. We benchmark (nearly) every function so we can check features
   for performance improvements / regressions.

   We constrain these modules to the interface of [File_path] and its submodules to make
   sure we have benchmarked all appropriate bindings. Whenever a new binding in
   [File_path] forces us to add a new binding here, we should add a new benchmark unless
   there is a pressing reason not to.

   For ppx-derived and functor-generated bindings, we test only a few functions. Otherwise
   we test every function. Where there are multiple cases, we try to benchmark all
   meaningfully different paths that do not raise.

   We define all benchmarks using [let%bench_fun], and bind all function arguments outside
   the final closure, wrapped in [Sys.opaque_identity]. This guarantees that computing the
   argument is not part of what we time, and that the closure cannot be specialized to the
   argument value. *)

open! Core

type t = File_path.t
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp, sexp_grammar]

let arg_type = File_path.arg_type
let root = File_path.root
let dot = File_path.dot
let dot_dot = File_path.dot_dot

include (
  File_path :
  sig
  @@ portable
    include
      Identifiable.S
      with type t := t
       and type comparator_witness = File_path.comparator_witness
  end)

let%bench_fun "equal =" =
  let x = Sys.opaque_identity (of_string "foo/bar/baz") in
  let y = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> equal x y
;;

let%bench_fun "equal <>" =
  let x = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let y = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> equal x y
;;

let%bench_fun "compare =" =
  let x = Sys.opaque_identity root in
  let y = Sys.opaque_identity root in
  fun () -> compare x y
;;

let%bench_fun "compare <" =
  let x = Sys.opaque_identity root in
  let y = Sys.opaque_identity dot_dot in
  fun () -> compare x y
;;

let%bench_fun "compare >" =
  let x = Sys.opaque_identity dot_dot in
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
  let unchecked_of_canonical_string = File_path.Expert.unchecked_of_canonical_string

  let%bench_fun "unchecked_of_canonical_string" =
    let string = Sys.opaque_identity "/foo/bar/baz" in
    fun () -> unchecked_of_canonical_string string
  ;;
end

let invariant = File_path.invariant

let%bench_fun "invariant" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> invariant t
;;

let is_relative = File_path.is_relative

let%bench_fun "is_relative" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> is_relative t
;;

let is_absolute = File_path.is_absolute

let%bench_fun "is_absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> is_absolute t
;;

let to_absolute = File_path.to_absolute

let%bench_fun "to_absolute, some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> to_absolute t
;;

let%bench_fun "to_absolute, none" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> to_absolute t
;;

let to_relative = File_path.to_relative

let%bench_fun "to_relative, some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> to_relative t
;;

let%bench_fun "to_relative, none" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> to_relative t
;;

let to_absolute_exn = File_path.to_absolute_exn

let%bench_fun "to_absolute_exn" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> to_absolute_exn t
;;

let to_relative_exn = File_path.to_relative_exn

let%bench_fun "to_relative_exn" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> to_relative_exn t
;;

let to_absolute_or_error = File_path.to_absolute_or_error

let%bench_fun "to_absolute_or_error" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> to_absolute_or_error t
;;

let to_relative_or_error = File_path.to_relative_or_error

let%bench_fun "to_relative_or_error" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> to_relative_or_error t
;;

let of_absolute = File_path.of_absolute

let%bench_fun "of_absolute" =
  let absolute = Sys.opaque_identity (File_path.Absolute.of_string "/foo/bar/baz") in
  fun () -> of_absolute absolute
;;

let of_relative = File_path.of_relative

let%bench_fun "of_relative" =
  let relative = Sys.opaque_identity (File_path.Relative.of_string "foo/bar/baz") in
  fun () -> of_relative relative
;;

let of_part_relative = File_path.of_part_relative

let%bench_fun "of_part_relative" =
  let part = Sys.opaque_identity (File_path.Part.of_string "foo") in
  fun () -> of_part_relative part
;;

let basename = File_path.basename

let%bench_fun "basename, absolute some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> basename t
;;

let%bench_fun "basename, absolute none" =
  let t = Sys.opaque_identity root in
  fun () -> basename t
;;

let%bench_fun "basename, relative some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> basename t
;;

let basename_exn = File_path.basename_exn

let%bench_fun "basename_exn, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> basename_exn t
;;

let%bench_fun "basename_exn, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> basename_exn t
;;

let basename_or_error = File_path.basename_or_error

let%bench_fun "basename_or_error, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> basename_or_error t
;;

let%bench_fun "basename_or_error, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> basename_or_error t
;;

let basename_defaulting_to_dot = File_path.basename_defaulting_to_dot

let%bench_fun "basename_defaulting_to_dot, absolute name" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> basename_defaulting_to_dot t
;;

let%bench_fun "basename_defaulting_to_dot, absolute dot" =
  let t = Sys.opaque_identity root in
  fun () -> basename_defaulting_to_dot t
;;

let%bench_fun "basename_defaulting_to_dot, relative name" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> basename_defaulting_to_dot t
;;

let dirname = File_path.dirname

let%bench_fun "dirname, absolute some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> dirname t
;;

let%bench_fun "dirname, absolute none" =
  let t = Sys.opaque_identity root in
  fun () -> dirname t
;;

let%bench_fun "dirname, relative some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> dirname t
;;

let%bench_fun "dirname, relative none" =
  let t = Sys.opaque_identity (of_string "foo.bar.baz") in
  fun () -> dirname t
;;

let dirname_exn = File_path.dirname_exn

let%bench_fun "dirname_exn, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> dirname_exn t
;;

let%bench_fun "dirname_exn, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> dirname_exn t
;;

let dirname_or_error = File_path.dirname_or_error

let%bench_fun "dirname_or_error, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> dirname_or_error t
;;

let%bench_fun "dirname_or_error, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> dirname_or_error t
;;

let dirname_defaulting_to_dot_or_root = File_path.dirname_defaulting_to_dot_or_root

let%bench_fun "dirname_defaulting_to_dot_or_root, absolute dir" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> dirname_defaulting_to_dot_or_root t
;;

let%bench_fun "dirname_defaulting_to_dot_or_root, absolute root" =
  let t = Sys.opaque_identity root in
  fun () -> dirname_defaulting_to_dot_or_root t
;;

let%bench_fun "dirname_defaulting_to_dot_or_root, relative dir" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> dirname_defaulting_to_dot_or_root t
;;

let%bench_fun "dirname_defaulting_to_dot_or_root, relative dot" =
  let t = Sys.opaque_identity (of_string "foo.bar.baz") in
  fun () -> dirname_defaulting_to_dot_or_root t
;;

let dirname_and_basename = File_path.dirname_and_basename

let%bench_fun "dirname_and_basename, absolute some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> dirname_and_basename t
;;

let%bench_fun "dirname_and_basename, absolute none" =
  let t = Sys.opaque_identity root in
  fun () -> dirname_and_basename t
;;

let%bench_fun "dirname_and_basename, relative some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> dirname_and_basename t
;;

let%bench_fun "dirname_and_basename, relative none" =
  let t = Sys.opaque_identity (of_string "foo.bar.baz") in
  fun () -> dirname_and_basename t
;;

let append_to_basename_exn = File_path.append_to_basename_exn

let%bench_fun "append_to_basename_exn, absolute, empty" =
  let t = Sys.opaque_identity (of_string "/foo/bar") in
  let suffix = Sys.opaque_identity "" in
  fun () -> append_to_basename_exn t suffix
;;

let%bench_fun "append_to_basename_exn, absolute, nonempty" =
  let t = Sys.opaque_identity (of_string "/foo/bar") in
  let suffix = Sys.opaque_identity ".baz" in
  fun () -> append_to_basename_exn t suffix
;;

let%bench_fun "append_to_basename_exn, relative, empty" =
  let t = Sys.opaque_identity (of_string "foo/bar") in
  let suffix = Sys.opaque_identity "" in
  fun () -> append_to_basename_exn t suffix
;;

let%bench_fun "append_to_basename_exn, relative, nonempty" =
  let t = Sys.opaque_identity (of_string "foo/bar") in
  let suffix = Sys.opaque_identity ".baz" in
  fun () -> append_to_basename_exn t suffix
;;

let append_part = File_path.append_part

let%bench_fun "append_part, root" =
  let t = Sys.opaque_identity root in
  let suffix = Sys.opaque_identity (File_path.Part.of_string "foo") in
  fun () -> append_part t suffix
;;

let%bench_fun "append_part, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar") in
  let suffix = Sys.opaque_identity (File_path.Part.of_string "baz") in
  fun () -> append_part t suffix
;;

let%bench_fun "append_part, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar") in
  let suffix = Sys.opaque_identity (File_path.Part.of_string "baz") in
  fun () -> append_part t suffix
;;

let is_prefix = File_path.is_prefix

let%bench_fun "is_prefix, absolute true" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> is_prefix t ~prefix
;;

let%bench_fun "is_prefix, absolute false" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foobie") in
  fun () -> is_prefix t ~prefix
;;

let%bench_fun "is_prefix, relative true" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> is_prefix t ~prefix
;;

let%bench_fun "is_prefix, relative false" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foobie") in
  fun () -> is_prefix t ~prefix
;;

let chop_prefix = File_path.chop_prefix

let%bench_fun "chop_prefix, absolute some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> chop_prefix t ~prefix
;;

let%bench_fun "chop_prefix, absolute none" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foobie") in
  fun () -> chop_prefix t ~prefix
;;

let%bench_fun "chop_prefix, relative some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> chop_prefix t ~prefix
;;

let%bench_fun "chop_prefix, relative none" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foobie") in
  fun () -> chop_prefix t ~prefix
;;

let chop_prefix_exn = File_path.chop_prefix_exn

let%bench_fun "chop_prefix_exn, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> chop_prefix_exn t ~prefix
;;

let%bench_fun "chop_prefix_exn, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> chop_prefix_exn t ~prefix
;;

let chop_prefix_or_error = File_path.chop_prefix_or_error

let%bench_fun "chop_prefix_or_error, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> chop_prefix_or_error t ~prefix
;;

let%bench_fun "chop_prefix_or_error, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> chop_prefix_or_error t ~prefix
;;

let chop_prefix_if_exists = File_path.chop_prefix_if_exists

let%bench_fun "chop_prefix_if_exists, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> chop_prefix_if_exists t ~prefix
;;

let%bench_fun "chop_prefix_if_exists, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> chop_prefix_if_exists t ~prefix
;;

let is_suffix = File_path.is_suffix

let%bench_fun "is_suffix, absolute true" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> is_suffix t ~suffix
;;

let%bench_fun "is_suffix, absolute false" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> is_suffix t ~suffix
;;

let%bench_fun "is_suffix, relative true" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> is_suffix t ~suffix
;;

let%bench_fun "is_suffix, relative false" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> is_suffix t ~suffix
;;

let chop_suffix = File_path.chop_suffix

let%bench_fun "chop_suffix, absolute some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix t ~suffix
;;

let%bench_fun "chop_suffix, absolute none" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> chop_suffix t ~suffix
;;

let%bench_fun "chop_suffix, relative some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix t ~suffix
;;

let%bench_fun "chop_suffix, relative none" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> chop_suffix t ~suffix
;;

let chop_suffix_exn = File_path.chop_suffix_exn

let%bench_fun "chop_suffix_exn, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix_exn t ~suffix
;;

let%bench_fun "chop_suffix_exn, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix_exn t ~suffix
;;

let chop_suffix_or_error = File_path.chop_suffix_or_error

let%bench_fun "chop_suffix_or_error, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix_or_error t ~suffix
;;

let%bench_fun "chop_suffix_or_error, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix_or_error t ~suffix
;;

let chop_suffix_if_exists = File_path.chop_suffix_if_exists

let%bench_fun "chop_suffix_if_exists, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix_if_exists t ~suffix
;;

let%bench_fun "chop_suffix_if_exists, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix_if_exists t ~suffix
;;

let append = File_path.append

let%bench_fun "append, root" =
  let prefix = Sys.opaque_identity root in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> append prefix suffix
;;

let%bench_fun "append, absolute" =
  let prefix = Sys.opaque_identity (of_string "/foo") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> append prefix suffix
;;

let%bench_fun "append, relative" =
  let prefix = Sys.opaque_identity (of_string "foo") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> append prefix suffix
;;

let number_of_parts = File_path.number_of_parts

let%bench_fun "number_of_parts, root" =
  let t = Sys.opaque_identity root in
  fun () -> number_of_parts t
;;

let%bench_fun "number_of_parts, dot" =
  let t = Sys.opaque_identity dot in
  fun () -> number_of_parts t
;;

let%bench_fun "number_of_parts, absolute compound" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> number_of_parts t
;;

let%bench_fun "number_of_parts, relative compound" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> number_of_parts t
;;

let to_parts = File_path.to_parts

let%bench_fun "to_parts, root" =
  let t = Sys.opaque_identity root in
  fun () -> to_parts t
;;

let%bench_fun "to_parts, dot" =
  let t = Sys.opaque_identity dot in
  fun () -> to_parts t
;;

let%bench_fun "to_parts, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> to_parts t
;;

let%bench_fun "to_parts, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> to_parts t
;;

let of_parts_absolute = File_path.of_parts_absolute

let%bench_fun "of_parts_absolute" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> of_parts_absolute parts
;;

let of_parts_relative = File_path.of_parts_relative

let%bench_fun "of_parts_relative, empty" =
  let parts = Sys.opaque_identity [] in
  fun () -> of_parts_relative parts
;;

let%bench_fun "of_parts_relative, non-empty" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> of_parts_relative parts
;;

let of_parts_relative_exn = File_path.of_parts_relative_exn

let%bench_fun "of_parts_relative_exn" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> of_parts_relative_exn parts
;;

let of_parts_relative_or_error = File_path.of_parts_relative_or_error

let%bench_fun "of_parts_relative_or_error" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> of_parts_relative_or_error parts
;;

let of_parts_relative_defaulting_to_dot = File_path.of_parts_relative_defaulting_to_dot

let%bench_fun "of_parts_relative_defaulting_to_dot, empty" =
  let parts = Sys.opaque_identity [] in
  fun () -> of_parts_relative_defaulting_to_dot parts
;;

let%bench_fun "of_parts_relative_defaulting_to_dot, non-empty" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> of_parts_relative_defaulting_to_dot parts
;;

let of_parts_relative_nonempty = File_path.of_parts_relative_nonempty

let%bench_fun "of_parts_relative_nonempty" =
  let parts =
    Sys.opaque_identity
      ([ File_path.Part.of_string "foo"
       ; File_path.Part.of_string "bar"
       ; File_path.Part.of_string "baz"
       ]
       : _ Nonempty_list.t)
  in
  fun () -> of_parts_relative_nonempty parts
;;

let make_absolute = File_path.make_absolute

let%bench_fun "make_absolute, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> make_absolute t ~under
;;

let%bench_fun "make_absolute, relative" =
  let t = Sys.opaque_identity (of_string "bar/baz") in
  let under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> make_absolute t ~under
;;

let make_relative = File_path.make_relative

let%bench_fun "make_relative, absolute under" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> make_relative t ~if_under
;;

let%bench_fun "make_relative, absolute not-under" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foobie") in
  fun () -> make_relative t ~if_under
;;

let%bench_fun "make_relative, relative" =
  let t = Sys.opaque_identity (of_string "bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> make_relative t ~if_under
;;

let make_relative_exn = File_path.make_relative_exn

let%bench_fun "make_relative_exn, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> make_relative_exn t ~if_under
;;

let%bench_fun "make_relative_exn, relative" =
  let t = Sys.opaque_identity (of_string "bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> make_relative_exn t ~if_under
;;

let make_relative_or_error = File_path.make_relative_or_error

let%bench_fun "make_relative_or_error, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> make_relative_or_error t ~if_under
;;

let%bench_fun "make_relative_or_error, relative" =
  let t = Sys.opaque_identity (of_string "bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> make_relative_or_error t ~if_under
;;

let make_relative_if_possible = File_path.make_relative_if_possible

let%bench_fun "make_relative_if_possible, absolute under" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> make_relative_if_possible t ~if_under
;;

let%bench_fun "make_relative_if_possible, absolute not-under" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foobie") in
  fun () -> make_relative_if_possible t ~if_under
;;

let%bench_fun "make_relative_if_possible, relative" =
  let t = Sys.opaque_identity (of_string "bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> make_relative_if_possible t ~if_under
;;

module Variant = File_path.Variant

let to_variant = File_path.to_variant

let%bench_fun "to_variant, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> to_variant t
;;

let%bench_fun "to_variant, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> to_variant t
;;

let of_variant = File_path.of_variant

let%bench_fun "of_variant, absolute" =
  let variant =
    Sys.opaque_identity (Variant.Absolute (File_path.Absolute.of_string "/foo/bar/baz"))
  in
  fun () -> of_variant variant
;;

let%bench_fun "of_variant, relative" =
  let variant =
    Sys.opaque_identity (Variant.Relative (File_path.Relative.of_string "foo/bar/baz"))
  in
  fun () -> of_variant variant
;;

let simplify_dot = File_path.simplify_dot

let%bench_fun "simplify_dot, absolute unchanged" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> simplify_dot t
;;

let%bench_fun "simplify_dot, relative unchanged" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> simplify_dot t
;;

let%bench_fun "simplify_dot, absolute changed" =
  let t = Sys.opaque_identity (of_string "/foo/./bar/baz/.") in
  fun () -> simplify_dot t
;;

let%bench_fun "simplify_dot, relative changed" =
  let t = Sys.opaque_identity (of_string "./foo/./bar/baz") in
  fun () -> simplify_dot t
;;

let simplify_dot_and_dot_dot_naively = File_path.simplify_dot_and_dot_dot_naively

let%bench_fun "simplify_dot_and_dot_dot_naively, absolute unchanged" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> simplify_dot_and_dot_dot_naively t
;;

let%bench_fun "simplify_dot_and_dot_dot_naively, relative unchanged" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> simplify_dot_and_dot_dot_naively t
;;

let%bench_fun "simplify_dot_and_dot_dot_naively, absolute changed" =
  let t = Sys.opaque_identity (of_string "/foo/quux/../bar/baz/.") in
  fun () -> simplify_dot_and_dot_dot_naively t
;;

let%bench_fun "simplify_dot_and_dot_dot_naively, relative changed" =
  let t = Sys.opaque_identity (of_string "./foo/quux/../bar/baz") in
  fun () -> simplify_dot_and_dot_dot_naively t
;;
