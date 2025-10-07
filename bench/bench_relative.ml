(* See comment in [bench_path.ml]. *)

open! Core

type t = File_path.Relative.t
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp, sexp_grammar]

let arg_type = File_path.Relative.arg_type
let dot = File_path.Relative.dot
let dot_dot = File_path.Relative.dot_dot

include (
  File_path.Relative :
  sig
  @@ portable
    include
      Identifiable.S
      with type t := t
       and type comparator_witness = File_path.Relative.comparator_witness
  end)

let%bench_fun "equal =" =
  let x = Sys.opaque_identity (of_string "foo/bar/baz") in
  let y = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> equal x y
;;

let%bench_fun "equal <>" =
  let x = Sys.opaque_identity dot in
  let y = Sys.opaque_identity (of_string "foo/bar/baz") in
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

let%bench_fun "of_string, canonical" =
  let string = Sys.opaque_identity "foo/bar/baz" in
  fun () -> of_string string
;;

let%bench_fun "of_string, non-canonical" =
  let string = Sys.opaque_identity "foo//bar/baz/" in
  fun () -> of_string string
;;

let%bench_fun "to_string" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> to_string t
;;

let%bench_fun "t_of_sexp, canonical" =
  let string = Sys.opaque_identity (Sexp.Atom "foo/bar/baz") in
  fun () -> t_of_sexp string
;;

let%bench_fun "t_of_sexp, non-canonical" =
  let string = Sys.opaque_identity (Sexp.Atom "foo//bar/baz/") in
  fun () -> t_of_sexp string
;;

let%bench_fun "sexp_of_t" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> sexp_of_t t
;;

module Expert = struct
  let unchecked_of_canonical_string =
    File_path.Relative.Expert.unchecked_of_canonical_string
  ;;

  let%bench_fun "unchecked_of_canonical_string" =
    let string = Sys.opaque_identity "foo/bar/baz" in
    fun () -> unchecked_of_canonical_string string
  ;;
end

let invariant = File_path.Relative.invariant

let%bench_fun "invariant" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> invariant t
;;

let of_part = File_path.Relative.of_part

let%bench_fun "of_part" =
  let part = Sys.opaque_identity (File_path.Part.of_string "foo") in
  fun () -> of_part part
;;

let basename = File_path.Relative.basename

let%bench_fun "basename" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> basename t
;;

let dirname = File_path.Relative.dirname

let%bench_fun "dirname, some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> dirname t
;;

let%bench_fun "dirname, none" =
  let t = Sys.opaque_identity (of_string "foo.bar.baz") in
  fun () -> dirname t
;;

let dirname_exn = File_path.Relative.dirname_exn

let%bench_fun "dirname_exn" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> dirname_exn t
;;

let dirname_or_error = File_path.Relative.dirname_or_error

let%bench_fun "dirname_or_error" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> dirname_or_error t
;;

let dirname_defaulting_to_dot = File_path.Relative.dirname_defaulting_to_dot

let%bench_fun "dirname_defaulting_to_dot, dir" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> dirname_defaulting_to_dot t
;;

let%bench_fun "dirname_defaulting_to_dot, dot" =
  let t = Sys.opaque_identity (of_string "foo.bar.baz") in
  fun () -> dirname_defaulting_to_dot t
;;

let top_dir = File_path.Relative.top_dir

let%bench_fun "top_dir, some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> top_dir t
;;

let%bench_fun "top_dir, none" =
  let t = Sys.opaque_identity dot in
  fun () -> top_dir t
;;

let top_dir_exn = File_path.Relative.top_dir_exn

let%bench_fun "top_dir_exn" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> top_dir_exn t
;;

let top_dir_or_error = File_path.Relative.top_dir_or_error

let%bench_fun "top_dir_or_error" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> top_dir_or_error t
;;

let top_dir_defaulting_to_dot = File_path.Relative.top_dir_defaulting_to_dot

let%bench_fun "top_dir_defaulting_to_dot, dir" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> top_dir_defaulting_to_dot t
;;

let%bench_fun "top_dir_defaulting_to_dot, dot" =
  let t = Sys.opaque_identity dot in
  fun () -> top_dir_defaulting_to_dot t
;;

let all_but_top_dir = File_path.Relative.all_but_top_dir

let%bench_fun "all_but_top_dir, some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> all_but_top_dir t
;;

let%bench_fun "all_but_top_dir, none" =
  let t = Sys.opaque_identity dot in
  fun () -> all_but_top_dir t
;;

let all_but_top_dir_exn = File_path.Relative.all_but_top_dir_exn

let%bench_fun "all_but_top_dir_exn" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> all_but_top_dir_exn t
;;

let all_but_top_dir_or_error = File_path.Relative.all_but_top_dir_or_error

let%bench_fun "all_but_top_dir_or_error" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> all_but_top_dir_or_error t
;;

let all_but_top_dir_defaulting_to_self =
  File_path.Relative.all_but_top_dir_defaulting_to_self
;;

let%bench_fun "all_but_top_dir_defaulting_to_self, path" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> all_but_top_dir_defaulting_to_self t
;;

let%bench_fun "all_but_top_dir_defaulting_to_self, self" =
  let t = Sys.opaque_identity dot in
  fun () -> all_but_top_dir_defaulting_to_self t
;;

let top_dir_and_all_but_top_dir = File_path.Relative.top_dir_and_all_but_top_dir

let%bench_fun "top_dir_and_all_but_top_dir, some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> top_dir_and_all_but_top_dir t
;;

let%bench_fun "top_dir_and_all_but_top_dir, none" =
  let t = Sys.opaque_identity dot in
  fun () -> top_dir_and_all_but_top_dir t
;;

let append_to_basename_exn = File_path.Relative.append_to_basename_exn

let%bench_fun "append_to_basename_exn, empty" =
  let t = Sys.opaque_identity (of_string "foo/bar") in
  let suffix = Sys.opaque_identity "" in
  fun () -> append_to_basename_exn t suffix
;;

let%bench_fun "append_to_basename_exn, nonempty" =
  let t = Sys.opaque_identity (of_string "foo/bar") in
  let suffix = Sys.opaque_identity ".baz" in
  fun () -> append_to_basename_exn t suffix
;;

let append_part = File_path.Relative.append_part

let%bench_fun "append_part" =
  let t = Sys.opaque_identity (of_string "foo/bar") in
  let suffix = Sys.opaque_identity (File_path.Part.of_string "baz") in
  fun () -> append_part t suffix
;;

let prepend_part = File_path.Relative.prepend_part

let%bench_fun "prepend_part" =
  let prefix = Sys.opaque_identity (File_path.Part.of_string "foo") in
  let t = Sys.opaque_identity (of_string "bar/baz") in
  fun () -> prepend_part prefix t
;;

let is_prefix = File_path.Relative.is_prefix

let%bench_fun "is_prefix, true" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> is_prefix t ~prefix
;;

let%bench_fun "is_prefix, false" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foobie") in
  fun () -> is_prefix t ~prefix
;;

let chop_prefix = File_path.Relative.chop_prefix

let%bench_fun "chop_prefix, some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> chop_prefix t ~prefix
;;

let%bench_fun "chop_prefix, none" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foobie") in
  fun () -> chop_prefix t ~prefix
;;

let chop_prefix_exn = File_path.Relative.chop_prefix_exn

let%bench_fun "chop_prefix_exn" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> chop_prefix_exn t ~prefix
;;

let chop_prefix_or_error = File_path.Relative.chop_prefix_or_error

let%bench_fun "chop_prefix_or_error" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> chop_prefix_or_error t ~prefix
;;

let chop_prefix_if_exists = File_path.Relative.chop_prefix_if_exists

let%bench_fun "chop_prefix_if_exists" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> chop_prefix_if_exists t ~prefix
;;

let is_suffix = File_path.Relative.is_suffix

let%bench_fun "is_suffix, true" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> is_suffix t ~suffix
;;

let%bench_fun "is_suffix, false" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> is_suffix t ~suffix
;;

let chop_suffix = File_path.Relative.chop_suffix

let%bench_fun "chop_suffix, some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix t ~suffix
;;

let%bench_fun "chop_suffix, none" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> chop_suffix t ~suffix
;;

let chop_suffix_exn = File_path.Relative.chop_suffix_exn

let%bench_fun "chop_suffix_exn" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix_exn t ~suffix
;;

let chop_suffix_or_error = File_path.Relative.chop_suffix_or_error

let%bench_fun "chop_suffix_or_error" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix_or_error t ~suffix
;;

let chop_suffix_if_exists = File_path.Relative.chop_suffix_if_exists

let%bench_fun "chop_suffix_if_exists" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> chop_suffix_if_exists t ~suffix
;;

let append = File_path.Relative.append

let%bench_fun "append" =
  let prefix = Sys.opaque_identity (of_string "foo") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> append prefix suffix
;;

let number_of_parts = File_path.Relative.number_of_parts

let%bench_fun "number_of_parts, dot" =
  let t = Sys.opaque_identity dot in
  fun () -> number_of_parts t
;;

let%bench_fun "number_of_parts, compound" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> number_of_parts t
;;

let to_parts = File_path.Relative.to_parts

let%bench_fun "to_parts, dot" =
  let t = Sys.opaque_identity dot in
  fun () -> to_parts t
;;

let%bench_fun "to_parts, compound" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> to_parts t
;;

let to_parts_nonempty = File_path.Relative.to_parts_nonempty

let%bench_fun "to_parts_nonempty, dot" =
  let t = Sys.opaque_identity dot in
  fun () -> to_parts_nonempty t
;;

let%bench_fun "to_parts_nonempty, compound" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> to_parts_nonempty t
;;

let of_parts = File_path.Relative.of_parts

let%bench_fun "of_parts, empty" =
  let parts = Sys.opaque_identity [] in
  fun () -> of_parts parts
;;

let%bench_fun "of_parts, non-empty" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> of_parts parts
;;

let of_parts_exn = File_path.Relative.of_parts_exn

let%bench_fun "of_parts_exn" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> of_parts_exn parts
;;

let of_parts_or_error = File_path.Relative.of_parts_or_error

let%bench_fun "of_parts_or_error" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> of_parts_or_error parts
;;

let of_parts_defaulting_to_dot = File_path.Relative.of_parts_defaulting_to_dot

let%bench_fun "of_parts_defaulting_to_dot, empty" =
  let parts = Sys.opaque_identity [] in
  fun () -> of_parts_defaulting_to_dot parts
;;

let%bench_fun "of_parts_defaulting_to_dot, non-empty" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> of_parts_defaulting_to_dot parts
;;

let of_parts_nonempty = File_path.Relative.of_parts_nonempty

let%bench_fun "of_parts_nonempty" =
  let parts =
    Sys.opaque_identity
      ([ File_path.Part.of_string "foo"
       ; File_path.Part.of_string "bar"
       ; File_path.Part.of_string "baz"
       ]
       : _ Nonempty_list.t)
  in
  fun () -> of_parts_nonempty parts
;;

let simplify_dot = File_path.Relative.simplify_dot

let%bench_fun "simplify_dot, unchanged" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> simplify_dot t
;;

let%bench_fun "simplify_dot, changed" =
  let t = Sys.opaque_identity (of_string "./foo/./bar/baz") in
  fun () -> simplify_dot t
;;

let simplify_dot_and_dot_dot_naively = File_path.Relative.simplify_dot_and_dot_dot_naively

let%bench_fun "simplify_dot_and_dot_dot_naively, unchanged" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> simplify_dot_and_dot_dot_naively t
;;

let%bench_fun "simplify_dot_and_dot_dot_naively, changed" =
  let t = Sys.opaque_identity (of_string "./foo/quux/../bar/baz") in
  fun () -> simplify_dot_and_dot_dot_naively t
;;
