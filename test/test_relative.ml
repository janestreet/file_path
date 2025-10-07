(* See comment in [test_path.ml]. *)

open! Core
open Expect_test_helpers_core

type t = File_path.Relative.t
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp, sexp_grammar]

let dot = File_path.Relative.dot
let dot_dot = File_path.Relative.dot_dot

let%expect_test _ =
  Helpers.test_constants (module File_path.Relative) [ dot; dot_dot ];
  [%expect
    {|
    .
    ..
    |}]
;;

include (
  File_path.Relative :
  sig
  @@ portable
    include
      Identifiable.S
      with type t := t
       and type comparator_witness = File_path.Relative.comparator_witness
  end)

let%expect_test _ =
  Helpers.test_compare (module File_path.Relative) Examples.Relative.for_compare;
  [%expect
    {|
    ("\001\255"
     -dot-is-not-always-first
     .
     ./.
     ././.
     ./..
     ..
     ../.
     ../..
     .hidden
     "This is a sentence; it has punctuation, capitalization, and spaces!"
     bin
     bin/exe
     bin/exe/file
     bin/exe.file
     bin.exe
     binary
     filename.txt
     "\255\001")
    |}]
;;

let%expect_test _ =
  Helpers.test_of_string
    (module File_path.Relative)
    Examples.Relative.strings_for_of_string;
  [%expect
    {|
    (= .)
    (= ..)
    (= filename.txt)
    (= bin)
    (= .hidden)
    (= "This is a sentence; it has punctuation, capitalization, and spaces!")
    (= "\001\255")
    (= ./.)
    (= ../..)
    (= ././.)
    (= bin/exe)
    (= bin/exe/file)
    (~ ./ .)
    (~ .//. ./.)
    (~ .//.// ./.)
    (~ bin/exe/ bin/exe)
    (~ bin//exe//file bin/exe/file)
    (~ bin//exe//file/ bin/exe/file)
    (! ("File_path.Relative.of_string: invalid string" ""))
    (! ("File_path.Relative.of_string: invalid string" "invalid/\000/null"))
    (! ("File_path.Relative.of_string: invalid string" /invalid/absolute))
    |}]
;;

let%expect_test _ =
  Helpers.test_containers (module File_path.Relative) Examples.Relative.for_conversion;
  [%expect
    {|
    (Set
     ("\001\255"
      .
      ./.
      ././.
      ..
      ../..
      .hidden
      "This is a sentence; it has punctuation, capitalization, and spaces!"
      bin
      bin/exe
      bin/exe/file
      filename.txt))
    (Map
     (("\001\255" 0)
      (. 1)
      (./. 2)
      (././. 3)
      (.. 4)
      (../.. 5)
      (.hidden 6)
      ("This is a sentence; it has punctuation, capitalization, and spaces!" 7)
      (bin 8)
      (bin/exe 9)
      (bin/exe/file 10)
      (filename.txt 11)))
    (Hash_set
     ("\001\255"
      .
      ./.
      ././.
      ..
      ../..
      .hidden
      "This is a sentence; it has punctuation, capitalization, and spaces!"
      bin
      bin/exe
      bin/exe/file
      filename.txt))
    (Table
     (("\001\255" 0)
      (. 1)
      (./. 2)
      (././. 3)
      (.. 4)
      (../.. 5)
      (.hidden 6)
      ("This is a sentence; it has punctuation, capitalization, and spaces!" 7)
      (bin 8)
      (bin/exe 9)
      (bin/exe/file 10)
      (filename.txt 11)))
    |}]
;;

module Expert = struct
  let unchecked_of_canonical_string =
    File_path.Relative.Expert.unchecked_of_canonical_string
  ;;
end

let invariant = File_path.Relative.invariant

let%expect_test _ =
  Helpers.test_invariant
    (module File_path.Relative)
    Examples.Relative.strings_for_of_string;
  [%expect
    {|
    (= .)
    (= ..)
    (= filename.txt)
    (= bin)
    (= .hidden)
    (= "This is a sentence; it has punctuation, capitalization, and spaces!")
    (= "\001\255")
    (= ./.)
    (= ../..)
    (= ././.)
    (= bin/exe)
    (= bin/exe/file)
    (! ("File_path.Relative.invariant: non-canonical representation" ./))
    (! ("File_path.Relative.invariant: non-canonical representation" .//.))
    (! ("File_path.Relative.invariant: non-canonical representation" .//.//))
    (! ("File_path.Relative.invariant: non-canonical representation" bin/exe/))
    (!
     ("File_path.Relative.invariant: non-canonical representation" bin//exe//file))
    (!
     ("File_path.Relative.invariant: non-canonical representation"
      bin//exe//file/))
    (! ("File_path.Relative.invariant: invalid string" ""))
    (! ("File_path.Relative.invariant: invalid string" "invalid/\000/null"))
    (! ("File_path.Relative.invariant: invalid string" /invalid/absolute))
    |}]
;;

let number_of_parts = File_path.Relative.number_of_parts

let%expect_test _ =
  Helpers.test
    number_of_parts
    ~input:(module File_path.Relative)
    ~output:(module Int)
    ~examples:Examples.Relative.for_conversion
    ~correctness:(fun _ number_of_parts ->
      require
        (Int.( >= ) number_of_parts 1)
        ~if_false_then_print_s:
          (lazy [%sexp "fewer than one part", { number_of_parts : int }]));
  [%expect
    {|
    (. -> 1)
    (.. -> 1)
    (filename.txt -> 1)
    (bin -> 1)
    (.hidden -> 1)
    ("This is a sentence; it has punctuation, capitalization, and spaces!" -> 1)
    ("\001\255" -> 1)
    (./. -> 2)
    (../.. -> 2)
    (././. -> 3)
    (bin/exe -> 2)
    (bin/exe/file -> 3)
    |}]
;;

let of_part = File_path.Relative.of_part

let%expect_test _ =
  Helpers.test
    of_part
    ~input:(module File_path.Part)
    ~output:(module File_path.Relative)
    ~examples:Examples.Part.for_conversion
    ~correctness:(fun _ of_part ->
      require_equal
        (module Int)
        (number_of_parts of_part)
        1
        ~message:"[of_part] and [number_of_parts] are inconsistent");
  [%expect
    {|
    (. -> .)
    (.. -> ..)
    (filename.txt -> filename.txt)
    (bin -> bin)
    (.hidden -> .hidden)
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!")
    ("\001\255" -> "\001\255")
    |}]
;;

let basename = File_path.Relative.basename

let%expect_test _ =
  Helpers.test
    basename
    ~input:(module File_path.Relative)
    ~output:(module File_path.Part)
    ~examples:Examples.Relative.for_basename_and_dirname
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    (. -> .)
    (.. -> ..)
    (singleton -> singleton)
    (./file -> file)
    (dir/. -> .)
    (../.. -> ..)
    (a/b -> b)
    (a/b/c -> c)
    (a/b/c/d -> d)
    (long/chain/of/names/ending/in/this -> this)
    |}]
;;

let dirname = File_path.Relative.dirname

let%expect_test _ =
  Helpers.test
    dirname
    ~input:(module File_path.Relative)
    ~output:(module Helpers.Option (File_path.Relative))
    ~examples:Examples.Relative.for_basename_and_dirname
    ~correctness:(fun relative dirname ->
      require_equal
        (module Bool)
        (Option.is_none dirname)
        (Int.equal (number_of_parts relative) 1)
        ~message:"[dirname] and [number_of_parts] are inconsistent");
  [%expect
    {|
    (. -> ())
    (.. -> ())
    (singleton -> ())
    (./file -> (.))
    (dir/. -> (dir))
    (../.. -> (..))
    (a/b -> (a))
    (a/b/c -> (a/b))
    (a/b/c/d -> (a/b/c))
    (long/chain/of/names/ending/in/this -> (long/chain/of/names/ending/in))
    |}]
;;

let dirname_exn = File_path.Relative.dirname_exn

let%expect_test _ =
  Helpers.test
    (fun t -> Or_error.try_with (fun () -> dirname_exn t))
    ~input:(module File_path.Relative)
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.Relative.for_basename_and_dirname
    ~correctness:(fun relative dirname_exn ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (Or_error.ok dirname_exn)
        (dirname relative)
        ~message:"[dirname_exn] and [dirname] are inconsistent");
  [%expect
    {|
    (. -> (Error ("File_path.Relative.dirname_exn: path contains no slash" .)))
    (.. -> (Error ("File_path.Relative.dirname_exn: path contains no slash" ..)))
    (singleton
     ->
     (Error ("File_path.Relative.dirname_exn: path contains no slash" singleton)))
    (./file -> (Ok .))
    (dir/. -> (Ok dir))
    (../.. -> (Ok ..))
    (a/b -> (Ok a))
    (a/b/c -> (Ok a/b))
    (a/b/c/d -> (Ok a/b/c))
    (long/chain/of/names/ending/in/this -> (Ok long/chain/of/names/ending/in))
    |}]
;;

let dirname_or_error = File_path.Relative.dirname_or_error

let%expect_test _ =
  Helpers.test
    (fun t -> dirname_or_error t)
    ~input:(module File_path.Relative)
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.Relative.for_basename_and_dirname
    ~correctness:(fun relative dirname_or_error ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (Or_error.ok dirname_or_error)
        (dirname relative)
        ~message:"[dirname_or_error] and [dirname] are inconsistent");
  [%expect
    {|
    (.
     ->
     (Error ("File_path.Relative.dirname_or_error: path contains no slash" .)))
    (..
     ->
     (Error ("File_path.Relative.dirname_or_error: path contains no slash" ..)))
    (singleton
     ->
     (Error
      ("File_path.Relative.dirname_or_error: path contains no slash" singleton)))
    (./file -> (Ok .))
    (dir/. -> (Ok dir))
    (../.. -> (Ok ..))
    (a/b -> (Ok a))
    (a/b/c -> (Ok a/b))
    (a/b/c/d -> (Ok a/b/c))
    (long/chain/of/names/ending/in/this -> (Ok long/chain/of/names/ending/in))
    |}]
;;

let dirname_defaulting_to_dot = File_path.Relative.dirname_defaulting_to_dot

let%expect_test _ =
  Helpers.test
    dirname_defaulting_to_dot
    ~input:(module File_path.Relative)
    ~output:(module File_path.Relative)
    ~examples:Examples.Relative.for_basename_and_dirname
    ~correctness:(fun relative dirname_defaulting_to_dot ->
      require_equal
        (module File_path.Relative)
        dirname_defaulting_to_dot
        (Option.value (dirname relative) ~default:dot)
        ~message:"[dirname_defaulting_to_dot] and [dirname] are inconsistent");
  [%expect
    {|
    (. -> .)
    (.. -> .)
    (singleton -> .)
    (./file -> .)
    (dir/. -> dir)
    (../.. -> ..)
    (a/b -> a)
    (a/b/c -> a/b)
    (a/b/c/d -> a/b/c)
    (long/chain/of/names/ending/in/this -> long/chain/of/names/ending/in)
    |}]
;;

let top_dir = File_path.Relative.top_dir

let%expect_test _ =
  Helpers.test
    top_dir
    ~input:(module File_path.Relative)
    ~output:(module Helpers.Option (File_path.Part))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative top_dir ->
      require_equal
        (module Bool)
        (Option.is_none top_dir)
        (Option.is_none (dirname relative))
        ~message:"[top_dir] and [dirname] are inconsistent");
  [%expect
    {|
    (. -> ())
    (.. -> ())
    (singleton -> ())
    (./file -> (.))
    (dir/. -> (dir))
    (../.. -> (..))
    (a/b -> (a))
    (a/b/c -> (a))
    (a/b/c/d -> (a))
    (long/chain/of/names/ending/in/this -> (long))
    |}]
;;

let top_dir_exn = File_path.Relative.top_dir_exn

let%expect_test _ =
  Helpers.test
    (fun t -> Or_error.try_with (fun () -> top_dir_exn t))
    ~input:(module File_path.Relative)
    ~output:(module Helpers.Or_error (File_path.Part))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative top_dir_exn ->
      require_equal
        (module Helpers.Option (File_path.Part))
        (Or_error.ok top_dir_exn)
        (top_dir relative)
        ~message:"[top_dir_exn] and [top_dir] are inconsistent");
  [%expect
    {|
    (. -> (Error ("File_path.Relative.top_dir_exn: path contains no slash" .)))
    (.. -> (Error ("File_path.Relative.top_dir_exn: path contains no slash" ..)))
    (singleton
     ->
     (Error ("File_path.Relative.top_dir_exn: path contains no slash" singleton)))
    (./file -> (Ok .))
    (dir/. -> (Ok dir))
    (../.. -> (Ok ..))
    (a/b -> (Ok a))
    (a/b/c -> (Ok a))
    (a/b/c/d -> (Ok a))
    (long/chain/of/names/ending/in/this -> (Ok long))
    |}]
;;

let top_dir_or_error = File_path.Relative.top_dir_or_error

let%expect_test _ =
  Helpers.test
    top_dir_or_error
    ~input:(module File_path.Relative)
    ~output:(module Helpers.Or_error (File_path.Part))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative top_dir_or_error ->
      require_equal
        (module Helpers.Option (File_path.Part))
        (Or_error.ok top_dir_or_error)
        (top_dir relative)
        ~message:"[top_dir_or_error] and [top_dir] are inconsistent");
  [%expect
    {|
    (.
     ->
     (Error ("File_path.Relative.top_dir_or_error: path contains no slash" .)))
    (..
     ->
     (Error ("File_path.Relative.top_dir_or_error: path contains no slash" ..)))
    (singleton
     ->
     (Error
      ("File_path.Relative.top_dir_or_error: path contains no slash" singleton)))
    (./file -> (Ok .))
    (dir/. -> (Ok dir))
    (../.. -> (Ok ..))
    (a/b -> (Ok a))
    (a/b/c -> (Ok a))
    (a/b/c/d -> (Ok a))
    (long/chain/of/names/ending/in/this -> (Ok long))
    |}]
;;

let top_dir_defaulting_to_dot = File_path.Relative.top_dir_defaulting_to_dot

let%expect_test _ =
  Helpers.test
    top_dir_defaulting_to_dot
    ~input:(module File_path.Relative)
    ~output:(module File_path.Part)
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative top_dir_defaulting_to_dot ->
      require_equal
        (module File_path.Part)
        top_dir_defaulting_to_dot
        (Option.value (top_dir relative) ~default:File_path.Part.dot)
        ~message:"[top_dir_defaulting_to_dot] and [top_dir] are inconsistent");
  [%expect
    {|
    (. -> .)
    (.. -> .)
    (singleton -> .)
    (./file -> .)
    (dir/. -> dir)
    (../.. -> ..)
    (a/b -> a)
    (a/b/c -> a)
    (a/b/c/d -> a)
    (long/chain/of/names/ending/in/this -> long)
    |}]
;;

let all_but_top_dir = File_path.Relative.all_but_top_dir

let%expect_test _ =
  Helpers.test
    all_but_top_dir
    ~input:(module File_path.Relative)
    ~output:(module Helpers.Option (File_path.Relative))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative all_but_top_dir ->
      require_equal
        (module Bool)
        (Option.is_none all_but_top_dir)
        (Option.is_none (dirname relative))
        ~message:"[all_but_top_dir] and [dirname] are inconsistent");
  [%expect
    {|
    (. -> ())
    (.. -> ())
    (singleton -> ())
    (./file -> (file))
    (dir/. -> (.))
    (../.. -> (..))
    (a/b -> (b))
    (a/b/c -> (b/c))
    (a/b/c/d -> (b/c/d))
    (long/chain/of/names/ending/in/this -> (chain/of/names/ending/in/this))
    |}]
;;

let all_but_top_dir_exn = File_path.Relative.all_but_top_dir_exn

let%expect_test _ =
  Helpers.test
    (fun t -> Or_error.try_with (fun () -> all_but_top_dir_exn t))
    ~input:(module File_path.Relative)
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative all_but_top_dir_exn ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (Or_error.ok all_but_top_dir_exn)
        (all_but_top_dir relative)
        ~message:"[all_but_top_dir_exn] and [all_but_top_dir] are inconsistent");
  [%expect
    {|
    (.
     ->
     (Error ("File_path.Relative.all_but_top_dir_exn: path contains no slash" .)))
    (..
     ->
     (Error ("File_path.Relative.all_but_top_dir_exn: path contains no slash" ..)))
    (singleton
     ->
     (Error
      ("File_path.Relative.all_but_top_dir_exn: path contains no slash" singleton)))
    (./file -> (Ok file))
    (dir/. -> (Ok .))
    (../.. -> (Ok ..))
    (a/b -> (Ok b))
    (a/b/c -> (Ok b/c))
    (a/b/c/d -> (Ok b/c/d))
    (long/chain/of/names/ending/in/this -> (Ok chain/of/names/ending/in/this))
    |}]
;;

let all_but_top_dir_or_error = File_path.Relative.all_but_top_dir_or_error

let%expect_test _ =
  Helpers.test
    all_but_top_dir_or_error
    ~input:(module File_path.Relative)
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative all_but_top_dir_or_error ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (Or_error.ok all_but_top_dir_or_error)
        (all_but_top_dir relative)
        ~message:"[all_but_top_dir_or_error] and [all_but_top_dir] are inconsistent");
  [%expect
    {|
    (.
     ->
     (Error
      ("File_path.Relative.all_but_top_dir_or_error: path contains no slash" .)))
    (..
     ->
     (Error
      ("File_path.Relative.all_but_top_dir_or_error: path contains no slash" ..)))
    (singleton
     ->
     (Error
      ("File_path.Relative.all_but_top_dir_or_error: path contains no slash"
       singleton)))
    (./file -> (Ok file))
    (dir/. -> (Ok .))
    (../.. -> (Ok ..))
    (a/b -> (Ok b))
    (a/b/c -> (Ok b/c))
    (a/b/c/d -> (Ok b/c/d))
    (long/chain/of/names/ending/in/this -> (Ok chain/of/names/ending/in/this))
    |}]
;;

let all_but_top_dir_defaulting_to_self =
  File_path.Relative.all_but_top_dir_defaulting_to_self
;;

let%expect_test _ =
  Helpers.test
    all_but_top_dir_defaulting_to_self
    ~input:(module File_path.Relative)
    ~output:(module File_path.Relative)
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative all_but_top_dir_defaulting_to_self ->
      require_equal
        (module File_path.Relative)
        all_but_top_dir_defaulting_to_self
        (Option.value (all_but_top_dir relative) ~default:relative)
        ~message:
          "[all_but_top_dir_defaulting_to_self] and [all_but_top_dir] are inconsistent");
  [%expect
    {|
    (. -> .)
    (.. -> ..)
    (singleton -> singleton)
    (./file -> file)
    (dir/. -> .)
    (../.. -> ..)
    (a/b -> b)
    (a/b/c -> b/c)
    (a/b/c/d -> b/c/d)
    (long/chain/of/names/ending/in/this -> chain/of/names/ending/in/this)
    |}]
;;

let top_dir_and_all_but_top_dir = File_path.Relative.top_dir_and_all_but_top_dir

let%expect_test _ =
  Helpers.test
    (fun path -> top_dir_and_all_but_top_dir path)
    ~input:(module File_path.Relative)
    ~output:(module Helpers.Option (Helpers.Tuple2 (File_path.Part) (File_path.Relative)))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun path top_dir_and_all_but_top_dir ->
      require_equal
        (module Helpers.Option (Helpers.Tuple2 (File_path.Part) (File_path.Relative)))
        top_dir_and_all_but_top_dir
        (Option.both (top_dir path) (all_but_top_dir path))
        ~message:
          "[top_dir_and_all_but_top_dir] and [top_dir]/[all_but_top_dir] are inconsistent");
  [%expect
    {|
    (. -> ())
    (.. -> ())
    (singleton -> ())
    (./file -> ((. file)))
    (dir/. -> ((dir .)))
    (../.. -> ((.. ..)))
    (a/b -> ((a b)))
    (a/b/c -> ((a b/c)))
    (a/b/c/d -> ((a b/c/d)))
    (long/chain/of/names/ending/in/this -> ((long chain/of/names/ending/in/this)))
    |}]
;;

let append_to_basename_exn = File_path.Relative.append_to_basename_exn

let%expect_test _ =
  Helpers.test
    (fun (path, string) ->
      Or_error.try_with (fun () -> append_to_basename_exn path string))
    ~input:(module Helpers.Tuple2 (File_path.Relative) (String))
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.Relative.for_append_to_basename
    ~correctness:(fun (path, string) append_to_basename_exn ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (Or_error.ok append_to_basename_exn)
        (if String.mem string '/' || String.mem string '\000'
         then None
         else Some (of_string (to_string path ^ string))));
  [%expect
    {|
    ((. x) -> (Ok .x))
    ((.. y) -> (Ok ..y))
    ((a .b) -> (Ok a.b))
    ((a/b .c) -> (Ok a/b.c))
    ((a/b/c .d) -> (Ok a/b/c.d))
    ((a/b/c "") -> (Ok a/b/c))
    ((a/b/c invalid/slash)
     ->
     (Error
      ("File_path.Relative.append_to_basename_exn: suffix contains invalid characters"
       ((path a/b/c) (suffix invalid/slash)))))
    ((a/b/c "invalid\000null")
     ->
     (Error
      ("File_path.Relative.append_to_basename_exn: suffix contains invalid characters"
       ((path a/b/c) (suffix "invalid\000null")))))
    ((long/chain/of/names/ending/in -this)
     ->
     (Ok long/chain/of/names/ending/in-this))
    |}]
;;

let append_part = File_path.Relative.append_part

let%expect_test _ =
  Helpers.test
    (fun (relative, suffix) -> append_part relative suffix)
    ~input:(module Helpers.Tuple2 (File_path.Relative) (File_path.Part))
    ~output:(module File_path.Relative)
    ~examples:Examples.Relative.for_append_part
    ~correctness:(fun (relative, part) append_part ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (dirname append_part)
        (Some relative)
        ~message:"[append_part] and [dirname] are inconsistent";
      require_equal
        (module File_path.Part)
        (basename append_part)
        part
        ~message:"[append_part] and [basename] are inconsistent");
  [%expect
    {|
    ((. file) -> ./file)
    ((dir .) -> dir/.)
    ((.. ..) -> ../..)
    ((a b) -> a/b)
    ((a/b c) -> a/b/c)
    ((a/b/c d) -> a/b/c/d)
    ((long/chain/of/names/ending/in this) -> long/chain/of/names/ending/in/this)
    |}]
;;

let prepend_part = File_path.Relative.prepend_part

let%expect_test _ =
  Helpers.test
    (fun (prefix, relative) -> prepend_part prefix relative)
    ~input:(module Helpers.Tuple2 (File_path.Part) (File_path.Relative))
    ~output:(module File_path.Relative)
    ~examples:Examples.Relative.for_prepend_part
    ~correctness:(fun (part, relative) prepend_part ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (all_but_top_dir prepend_part)
        (Some relative)
        ~message:"[prepend_part] and [all_but_top_dir] are inconsistent";
      require_equal
        (module Helpers.Option (File_path.Part))
        (top_dir prepend_part)
        (Some part)
        ~message:"[prepend_part] and [top_dir] are inconsistent");
  [%expect
    {|
    ((. file) -> ./file)
    ((dir .) -> dir/.)
    ((.. ..) -> ../..)
    ((a b) -> a/b)
    ((a b/c) -> a/b/c)
    ((a b/c/d) -> a/b/c/d)
    ((long chain/of/names/ending/in/this) -> long/chain/of/names/ending/in/this)
    |}]
;;

let is_prefix = File_path.Relative.is_prefix

let%expect_test _ =
  Helpers.test_predicate
    (fun { t; prefix } -> is_prefix t ~prefix)
    ~input:(module Helpers.With_prefix (File_path.Relative))
    ~examples:Examples.Relative.for_chop_prefix
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    ((success
      (((t .) (prefix .))
       ((t ..) (prefix ..))
       ((t a/b/c) (prefix a/b/c))
       ((t ./file) (prefix .))
       ((t dir/.) (prefix dir))
       ((t ../..) (prefix ..))
       ((t a/b/c/d) (prefix a))
       ((t a/b/c/d) (prefix a/b))
       ((t a/b/c/d) (prefix a/b/c))
       ((t long/chain/of/names/ending/in/this) (prefix long/chain/of/names))))
     (failure
      (((t ..) (prefix .))
       ((t ./b) (prefix ./a))
       ((t c/d) (prefix a/b))
       ((t a) (prefix a/b/c))
       ((t a/b) (prefix a/b/c)))))
    |}]
;;

let chop_prefix = File_path.Relative.chop_prefix

let%expect_test _ =
  Helpers.test
    (fun { t; prefix } -> chop_prefix t ~prefix)
    ~input:(module Helpers.With_prefix (File_path.Relative))
    ~output:(module Helpers.Option (File_path.Relative))
    ~examples:Examples.Relative.for_chop_prefix
    ~correctness:(fun { t; prefix } chop_prefix ->
      require_equal
        (module Bool)
        (is_prefix t ~prefix)
        (Option.is_some chop_prefix)
        ~message:"[chop_prefix] and [is_prefix] are inconsistent");
  [%expect
    {|
    (((t ..) (prefix .)) -> ())
    (((t ./b) (prefix ./a)) -> ())
    (((t c/d) (prefix a/b)) -> ())
    (((t a) (prefix a/b/c)) -> ())
    (((t a/b) (prefix a/b/c)) -> ())
    (((t .) (prefix .)) -> (.))
    (((t ..) (prefix ..)) -> (.))
    (((t a/b/c) (prefix a/b/c)) -> (.))
    (((t ./file) (prefix .)) -> (file))
    (((t dir/.) (prefix dir)) -> (.))
    (((t ../..) (prefix ..)) -> (..))
    (((t a/b/c/d) (prefix a)) -> (b/c/d))
    (((t a/b/c/d) (prefix a/b)) -> (c/d))
    (((t a/b/c/d) (prefix a/b/c)) -> (d))
    (((t long/chain/of/names/ending/in/this) (prefix long/chain/of/names))
     ->
     (ending/in/this))
    |}]
;;

let chop_prefix_exn = File_path.Relative.chop_prefix_exn

let%expect_test _ =
  Helpers.test
    (fun { t; prefix } -> Or_error.try_with (fun () -> chop_prefix_exn t ~prefix))
    ~input:(module Helpers.With_prefix (File_path.Relative))
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.Relative.for_chop_prefix
    ~correctness:(fun { t; prefix } chop_prefix_exn ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (chop_prefix t ~prefix)
        (Or_error.ok chop_prefix_exn)
        ~message:"[chop_prefix_exn] and [chop_prefix] are inconsistent");
  [%expect
    {|
    (((t ..) (prefix .))
     ->
     (Error
      ("File_path.Relative.chop_prefix_exn: not a prefix" ((path ..) (prefix .)))))
    (((t ./b) (prefix ./a))
     ->
     (Error
      ("File_path.Relative.chop_prefix_exn: not a prefix"
       ((path ./b) (prefix ./a)))))
    (((t c/d) (prefix a/b))
     ->
     (Error
      ("File_path.Relative.chop_prefix_exn: not a prefix"
       ((path c/d) (prefix a/b)))))
    (((t a) (prefix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_prefix_exn: not a prefix"
       ((path a) (prefix a/b/c)))))
    (((t a/b) (prefix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_prefix_exn: not a prefix"
       ((path a/b) (prefix a/b/c)))))
    (((t .) (prefix .)) -> (Ok .))
    (((t ..) (prefix ..)) -> (Ok .))
    (((t a/b/c) (prefix a/b/c)) -> (Ok .))
    (((t ./file) (prefix .)) -> (Ok file))
    (((t dir/.) (prefix dir)) -> (Ok .))
    (((t ../..) (prefix ..)) -> (Ok ..))
    (((t a/b/c/d) (prefix a)) -> (Ok b/c/d))
    (((t a/b/c/d) (prefix a/b)) -> (Ok c/d))
    (((t a/b/c/d) (prefix a/b/c)) -> (Ok d))
    (((t long/chain/of/names/ending/in/this) (prefix long/chain/of/names))
     ->
     (Ok ending/in/this))
    |}]
;;

let chop_prefix_or_error = File_path.Relative.chop_prefix_or_error

let%expect_test _ =
  Helpers.test
    (fun { t; prefix } -> chop_prefix_or_error t ~prefix)
    ~input:(module Helpers.With_prefix (File_path.Relative))
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.Relative.for_chop_prefix
    ~correctness:(fun { t; prefix } chop_prefix_or_error ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (chop_prefix t ~prefix)
        (Or_error.ok chop_prefix_or_error)
        ~message:"[chop_prefix_or_error] and [chop_prefix] are inconsistent");
  [%expect
    {|
    (((t ..) (prefix .))
     ->
     (Error
      ("File_path.Relative.chop_prefix_or_error: not a prefix"
       ((path ..) (prefix .)))))
    (((t ./b) (prefix ./a))
     ->
     (Error
      ("File_path.Relative.chop_prefix_or_error: not a prefix"
       ((path ./b) (prefix ./a)))))
    (((t c/d) (prefix a/b))
     ->
     (Error
      ("File_path.Relative.chop_prefix_or_error: not a prefix"
       ((path c/d) (prefix a/b)))))
    (((t a) (prefix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_prefix_or_error: not a prefix"
       ((path a) (prefix a/b/c)))))
    (((t a/b) (prefix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_prefix_or_error: not a prefix"
       ((path a/b) (prefix a/b/c)))))
    (((t .) (prefix .)) -> (Ok .))
    (((t ..) (prefix ..)) -> (Ok .))
    (((t a/b/c) (prefix a/b/c)) -> (Ok .))
    (((t ./file) (prefix .)) -> (Ok file))
    (((t dir/.) (prefix dir)) -> (Ok .))
    (((t ../..) (prefix ..)) -> (Ok ..))
    (((t a/b/c/d) (prefix a)) -> (Ok b/c/d))
    (((t a/b/c/d) (prefix a/b)) -> (Ok c/d))
    (((t a/b/c/d) (prefix a/b/c)) -> (Ok d))
    (((t long/chain/of/names/ending/in/this) (prefix long/chain/of/names))
     ->
     (Ok ending/in/this))
    |}]
;;

let chop_prefix_if_exists = File_path.Relative.chop_prefix_if_exists

let%expect_test _ =
  Helpers.test
    (fun { t; prefix } -> chop_prefix_if_exists t ~prefix)
    ~input:(module Helpers.With_prefix (File_path.Relative))
    ~output:(module File_path.Relative)
    ~examples:Examples.Relative.for_chop_prefix
    ~correctness:(fun { t; prefix } chop_prefix_if_exists ->
      require_equal
        (module File_path.Relative)
        (chop_prefix t ~prefix |> Option.value ~default:t)
        chop_prefix_if_exists
        ~message:"[chop_prefix_if_exists] and [chop_prefix] are inconsistent");
  [%expect
    {|
    (((t ..) (prefix .)) -> ..)
    (((t ./b) (prefix ./a)) -> ./b)
    (((t c/d) (prefix a/b)) -> c/d)
    (((t a) (prefix a/b/c)) -> a)
    (((t a/b) (prefix a/b/c)) -> a/b)
    (((t .) (prefix .)) -> .)
    (((t ..) (prefix ..)) -> .)
    (((t a/b/c) (prefix a/b/c)) -> .)
    (((t ./file) (prefix .)) -> file)
    (((t dir/.) (prefix dir)) -> .)
    (((t ../..) (prefix ..)) -> ..)
    (((t a/b/c/d) (prefix a)) -> b/c/d)
    (((t a/b/c/d) (prefix a/b)) -> c/d)
    (((t a/b/c/d) (prefix a/b/c)) -> d)
    (((t long/chain/of/names/ending/in/this) (prefix long/chain/of/names))
     ->
     ending/in/this)
    |}]
;;

let is_suffix = File_path.Relative.is_suffix

let%expect_test _ =
  Helpers.test_predicate
    (fun { t; suffix } -> is_suffix t ~suffix)
    ~input:(module Helpers.With_suffix (File_path.Relative))
    ~examples:Examples.Relative.for_chop_suffix
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    ((success
      (((t .) (suffix .))
       ((t ..) (suffix ..))
       ((t a/b/c) (suffix a/b/c))
       ((t ./file) (suffix file))
       ((t dir/.) (suffix .))
       ((t ../..) (suffix ..))
       ((t a/b/c/d) (suffix b/c/d))
       ((t a/b/c/d) (suffix c/d))
       ((t a/b/c/d) (suffix d))
       ((t long/chain/of/names/ending/in/this) (suffix ending/in/this))))
     (failure
      (((t ..) (suffix .))
       ((t b/.) (suffix a/.))
       ((t c/d) (suffix a/b))
       ((t c) (suffix a/b/c))
       ((t b/c) (suffix a/b/c)))))
    |}]
;;

let chop_suffix = File_path.Relative.chop_suffix

let%expect_test _ =
  Helpers.test
    (fun { t; suffix } -> chop_suffix t ~suffix)
    ~input:(module Helpers.With_suffix (File_path.Relative))
    ~output:(module Helpers.Option (File_path.Relative))
    ~examples:Examples.Relative.for_chop_suffix
    ~correctness:(fun { t; suffix } chop_suffix ->
      require_equal
        (module Bool)
        (is_suffix t ~suffix)
        (Option.is_some chop_suffix)
        ~message:"[chop_suffix] and [is_suffix] are inconsistent");
  [%expect
    {|
    (((t ..) (suffix .)) -> ())
    (((t b/.) (suffix a/.)) -> ())
    (((t c/d) (suffix a/b)) -> ())
    (((t c) (suffix a/b/c)) -> ())
    (((t b/c) (suffix a/b/c)) -> ())
    (((t .) (suffix .)) -> (.))
    (((t ..) (suffix ..)) -> (.))
    (((t a/b/c) (suffix a/b/c)) -> (.))
    (((t ./file) (suffix file)) -> (.))
    (((t dir/.) (suffix .)) -> (dir))
    (((t ../..) (suffix ..)) -> (..))
    (((t a/b/c/d) (suffix b/c/d)) -> (a))
    (((t a/b/c/d) (suffix c/d)) -> (a/b))
    (((t a/b/c/d) (suffix d)) -> (a/b/c))
    (((t long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     (long/chain/of/names))
    |}]
;;

let chop_suffix_exn = File_path.Relative.chop_suffix_exn

let%expect_test _ =
  Helpers.test
    (fun { t; suffix } -> Or_error.try_with (fun () -> chop_suffix_exn t ~suffix))
    ~input:(module Helpers.With_suffix (File_path.Relative))
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.Relative.for_chop_suffix
    ~correctness:(fun { t; suffix } chop_suffix_exn ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (chop_suffix t ~suffix)
        (Or_error.ok chop_suffix_exn)
        ~message:"[chop_suffix_exn] and [chop_suffix] are inconsistent");
  [%expect
    {|
    (((t ..) (suffix .))
     ->
     (Error
      ("File_path.Relative.chop_suffix_exn: not a suffix" ((path ..) (suffix .)))))
    (((t b/.) (suffix a/.))
     ->
     (Error
      ("File_path.Relative.chop_suffix_exn: not a suffix"
       ((path b/.) (suffix a/.)))))
    (((t c/d) (suffix a/b))
     ->
     (Error
      ("File_path.Relative.chop_suffix_exn: not a suffix"
       ((path c/d) (suffix a/b)))))
    (((t c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_suffix_exn: not a suffix"
       ((path c) (suffix a/b/c)))))
    (((t b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_suffix_exn: not a suffix"
       ((path b/c) (suffix a/b/c)))))
    (((t .) (suffix .)) -> (Ok .))
    (((t ..) (suffix ..)) -> (Ok .))
    (((t a/b/c) (suffix a/b/c)) -> (Ok .))
    (((t ./file) (suffix file)) -> (Ok .))
    (((t dir/.) (suffix .)) -> (Ok dir))
    (((t ../..) (suffix ..)) -> (Ok ..))
    (((t a/b/c/d) (suffix b/c/d)) -> (Ok a))
    (((t a/b/c/d) (suffix c/d)) -> (Ok a/b))
    (((t a/b/c/d) (suffix d)) -> (Ok a/b/c))
    (((t long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     (Ok long/chain/of/names))
    |}]
;;

let chop_suffix_or_error = File_path.Relative.chop_suffix_or_error

let%expect_test _ =
  Helpers.test
    (fun { t; suffix } -> chop_suffix_or_error t ~suffix)
    ~input:(module Helpers.With_suffix (File_path.Relative))
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.Relative.for_chop_suffix
    ~correctness:(fun { t; suffix } chop_suffix_or_error ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (chop_suffix t ~suffix)
        (Or_error.ok chop_suffix_or_error)
        ~message:"[chop_suffix_or_error] and [chop_suffix] are inconsistent");
  [%expect
    {|
    (((t ..) (suffix .))
     ->
     (Error
      ("File_path.Relative.chop_suffix_or_error: not a suffix"
       ((path ..) (suffix .)))))
    (((t b/.) (suffix a/.))
     ->
     (Error
      ("File_path.Relative.chop_suffix_or_error: not a suffix"
       ((path b/.) (suffix a/.)))))
    (((t c/d) (suffix a/b))
     ->
     (Error
      ("File_path.Relative.chop_suffix_or_error: not a suffix"
       ((path c/d) (suffix a/b)))))
    (((t c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_suffix_or_error: not a suffix"
       ((path c) (suffix a/b/c)))))
    (((t b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_suffix_or_error: not a suffix"
       ((path b/c) (suffix a/b/c)))))
    (((t .) (suffix .)) -> (Ok .))
    (((t ..) (suffix ..)) -> (Ok .))
    (((t a/b/c) (suffix a/b/c)) -> (Ok .))
    (((t ./file) (suffix file)) -> (Ok .))
    (((t dir/.) (suffix .)) -> (Ok dir))
    (((t ../..) (suffix ..)) -> (Ok ..))
    (((t a/b/c/d) (suffix b/c/d)) -> (Ok a))
    (((t a/b/c/d) (suffix c/d)) -> (Ok a/b))
    (((t a/b/c/d) (suffix d)) -> (Ok a/b/c))
    (((t long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     (Ok long/chain/of/names))
    |}]
;;

let chop_suffix_if_exists = File_path.Relative.chop_suffix_if_exists

let%expect_test _ =
  Helpers.test
    (fun { t; suffix } -> chop_suffix_if_exists t ~suffix)
    ~input:(module Helpers.With_suffix (File_path.Relative))
    ~output:(module File_path.Relative)
    ~examples:Examples.Relative.for_chop_suffix
    ~correctness:(fun { t; suffix } chop_suffix_if_exists ->
      require_equal
        (module File_path.Relative)
        (chop_suffix t ~suffix |> Option.value ~default:t)
        chop_suffix_if_exists
        ~message:"[chop_suffix_if_exists] and [chop_suffix] are inconsistent");
  [%expect
    {|
    (((t ..) (suffix .)) -> ..)
    (((t b/.) (suffix a/.)) -> b/.)
    (((t c/d) (suffix a/b)) -> c/d)
    (((t c) (suffix a/b/c)) -> c)
    (((t b/c) (suffix a/b/c)) -> b/c)
    (((t .) (suffix .)) -> .)
    (((t ..) (suffix ..)) -> .)
    (((t a/b/c) (suffix a/b/c)) -> .)
    (((t ./file) (suffix file)) -> .)
    (((t dir/.) (suffix .)) -> dir)
    (((t ../..) (suffix ..)) -> ..)
    (((t a/b/c/d) (suffix b/c/d)) -> a)
    (((t a/b/c/d) (suffix c/d)) -> a/b)
    (((t a/b/c/d) (suffix d)) -> a/b/c)
    (((t long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     long/chain/of/names)
    |}]
;;

let append = File_path.Relative.append

let%expect_test _ =
  Helpers.test
    (fun (prefix, suffix) -> append prefix suffix)
    ~input:(module Helpers.Tuple2 (File_path.Relative) (File_path.Relative))
    ~output:(module File_path.Relative)
    ~examples:Examples.Relative.for_append
    ~correctness:(fun (prefix, suffix) append ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (Some suffix)
        (chop_prefix append ~prefix)
        ~message:"[append] and [chop_prefix] are inconsistent";
      require_equal
        (module Helpers.Option (File_path.Relative))
        (Some prefix)
        (chop_suffix append ~suffix)
        ~message:"[append] and [chop_suffix] are inconsistent");
  [%expect
    {|
    ((. file) -> ./file)
    ((dir .) -> dir/.)
    ((.. ..) -> ../..)
    ((a b/c/d) -> a/b/c/d)
    ((a/b c/d) -> a/b/c/d)
    ((a/b/c d) -> a/b/c/d)
    ((long/chain/of/names ending/in/this) -> long/chain/of/names/ending/in/this)
    |}]
;;

let to_parts = File_path.Relative.to_parts

let%expect_test _ =
  Helpers.test
    to_parts
    ~input:(module File_path.Relative)
    ~output:(module Helpers.List (File_path.Part))
    ~examples:Examples.Relative.for_conversion
    ~correctness:(fun relative to_parts ->
      require_equal
        (module Int)
        (List.length to_parts)
        (number_of_parts relative)
        ~message:"[to_parts] and [number_of_parts] are inconsistent");
  [%expect
    {|
    (. -> (.))
    (.. -> (..))
    (filename.txt -> (filename.txt))
    (bin -> (bin))
    (.hidden -> (.hidden))
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     ("This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("\001\255" -> ("\001\255"))
    (./. -> (. .))
    (../.. -> (.. ..))
    (././. -> (. . .))
    (bin/exe -> (bin exe))
    (bin/exe/file -> (bin exe file))
    |}]
;;

let to_parts_nonempty = File_path.Relative.to_parts_nonempty

let%expect_test _ =
  Helpers.test
    to_parts_nonempty
    ~input:(module File_path.Relative)
    ~output:(module Helpers.Nonempty_list (File_path.Part))
    ~examples:Examples.Relative.for_conversion
    ~correctness:(fun relative to_parts_nonempty ->
      require_equal
        (module Helpers.List (File_path.Part))
        (Nonempty_list.to_list to_parts_nonempty)
        (to_parts relative)
        ~message:"[to_parts_nonempty] and [to_parts] are inconsistent");
  [%expect
    {|
    (. -> (.))
    (.. -> (..))
    (filename.txt -> (filename.txt))
    (bin -> (bin))
    (.hidden -> (.hidden))
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     ("This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("\001\255" -> ("\001\255"))
    (./. -> (. .))
    (../.. -> (.. ..))
    (././. -> (. . .))
    (bin/exe -> (bin exe))
    (bin/exe/file -> (bin exe file))
    |}]
;;

let of_parts = File_path.Relative.of_parts

let%expect_test _ =
  Helpers.test
    of_parts
    ~input:(module Helpers.List (File_path.Part))
    ~output:(module Helpers.Option (File_path.Relative))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts ->
      require_equal
        (module Helpers.List (File_path.Part))
        (Option.value_map of_parts ~f:to_parts ~default:[])
        parts
        ~message:"[of_parts] and [to_parts] are inconsistent");
  [%expect
    {|
    (() -> ())
    ((.) -> (.))
    ((..) -> (..))
    ((filename.txt) -> (filename.txt))
    ((bin) -> (bin))
    ((.hidden) -> (.hidden))
    (("This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     ("This is a sentence; it has punctuation, capitalization, and spaces!"))
    (("\001\255") -> ("\001\255"))
    ((. .) -> (./.))
    ((.. .) -> (../.))
    ((.. .) -> (../.))
    ((.. ..) -> (../..))
    ((filename.txt .) -> (filename.txt/.))
    ((.. filename.txt) -> (../filename.txt))
    ((bin .) -> (bin/.))
    ((.. bin) -> (../bin))
    ((.hidden .) -> (.hidden/.))
    ((.. .hidden) -> (../.hidden))
    (("This is a sentence; it has punctuation, capitalization, and spaces!" .)
     ->
     ("This is a sentence; it has punctuation, capitalization, and spaces!/."))
    ((.. "This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     ("../This is a sentence; it has punctuation, capitalization, and spaces!"))
    (("\001\255" .) -> ("\001\255/."))
    ((.. "\001\255") -> ("../\001\255"))
    ((.hidden bin.exe) -> (.hidden/bin.exe))
    ((.hidden bin exe.file) -> (.hidden/bin/exe.file))
    |}]
;;

let of_parts_exn = File_path.Relative.of_parts_exn

let%expect_test _ =
  Helpers.test
    (fun parts -> Or_error.try_with (fun () -> of_parts_exn parts))
    ~input:(module Helpers.List (File_path.Part))
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_exn ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (of_parts parts)
        (Or_error.ok of_parts_exn)
        ~message:"[of_parts_exn] and [of_parts] are inconsistent");
  [%expect
    {|
    (() -> (Error "File_path.Relative.of_parts_exn: empty list"))
    ((.) -> (Ok .))
    ((..) -> (Ok ..))
    ((filename.txt) -> (Ok filename.txt))
    ((bin) -> (Ok bin))
    ((.hidden) -> (Ok .hidden))
    (("This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     (Ok "This is a sentence; it has punctuation, capitalization, and spaces!"))
    (("\001\255") -> (Ok "\001\255"))
    ((. .) -> (Ok ./.))
    ((.. .) -> (Ok ../.))
    ((.. .) -> (Ok ../.))
    ((.. ..) -> (Ok ../..))
    ((filename.txt .) -> (Ok filename.txt/.))
    ((.. filename.txt) -> (Ok ../filename.txt))
    ((bin .) -> (Ok bin/.))
    ((.. bin) -> (Ok ../bin))
    ((.hidden .) -> (Ok .hidden/.))
    ((.. .hidden) -> (Ok ../.hidden))
    (("This is a sentence; it has punctuation, capitalization, and spaces!" .)
     ->
     (Ok "This is a sentence; it has punctuation, capitalization, and spaces!/."))
    ((.. "This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     (Ok "../This is a sentence; it has punctuation, capitalization, and spaces!"))
    (("\001\255" .) -> (Ok "\001\255/."))
    ((.. "\001\255") -> (Ok "../\001\255"))
    ((.hidden bin.exe) -> (Ok .hidden/bin.exe))
    ((.hidden bin exe.file) -> (Ok .hidden/bin/exe.file))
    |}]
;;

let of_parts_or_error = File_path.Relative.of_parts_or_error

let%expect_test _ =
  Helpers.test
    of_parts_or_error
    ~input:(module Helpers.List (File_path.Part))
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_or_error ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (of_parts parts)
        (Or_error.ok of_parts_or_error)
        ~message:"[of_parts_or_error] and [of_parts] are inconsistent");
  [%expect
    {|
    (() -> (Error "File_path.Relative.of_parts_or_error: empty list"))
    ((.) -> (Ok .))
    ((..) -> (Ok ..))
    ((filename.txt) -> (Ok filename.txt))
    ((bin) -> (Ok bin))
    ((.hidden) -> (Ok .hidden))
    (("This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     (Ok "This is a sentence; it has punctuation, capitalization, and spaces!"))
    (("\001\255") -> (Ok "\001\255"))
    ((. .) -> (Ok ./.))
    ((.. .) -> (Ok ../.))
    ((.. .) -> (Ok ../.))
    ((.. ..) -> (Ok ../..))
    ((filename.txt .) -> (Ok filename.txt/.))
    ((.. filename.txt) -> (Ok ../filename.txt))
    ((bin .) -> (Ok bin/.))
    ((.. bin) -> (Ok ../bin))
    ((.hidden .) -> (Ok .hidden/.))
    ((.. .hidden) -> (Ok ../.hidden))
    (("This is a sentence; it has punctuation, capitalization, and spaces!" .)
     ->
     (Ok "This is a sentence; it has punctuation, capitalization, and spaces!/."))
    ((.. "This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     (Ok "../This is a sentence; it has punctuation, capitalization, and spaces!"))
    (("\001\255" .) -> (Ok "\001\255/."))
    ((.. "\001\255") -> (Ok "../\001\255"))
    ((.hidden bin.exe) -> (Ok .hidden/bin.exe))
    ((.hidden bin exe.file) -> (Ok .hidden/bin/exe.file))
    |}]
;;

let of_parts_defaulting_to_dot = File_path.Relative.of_parts_defaulting_to_dot

let%expect_test _ =
  Helpers.test
    of_parts_defaulting_to_dot
    ~input:(module Helpers.List (File_path.Part))
    ~output:(module File_path.Relative)
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_defaulting_to_dot ->
      require_equal
        (module File_path.Relative)
        (Option.value (of_parts parts) ~default:dot)
        of_parts_defaulting_to_dot
        ~message:"[of_parts_defaulting_to_dot] and [of_parts] are inconsistent");
  [%expect
    {|
    (() -> .)
    ((.) -> .)
    ((..) -> ..)
    ((filename.txt) -> filename.txt)
    ((bin) -> bin)
    ((.hidden) -> .hidden)
    (("This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!")
    (("\001\255") -> "\001\255")
    ((. .) -> ./.)
    ((.. .) -> ../.)
    ((.. .) -> ../.)
    ((.. ..) -> ../..)
    ((filename.txt .) -> filename.txt/.)
    ((.. filename.txt) -> ../filename.txt)
    ((bin .) -> bin/.)
    ((.. bin) -> ../bin)
    ((.hidden .) -> .hidden/.)
    ((.. .hidden) -> ../.hidden)
    (("This is a sentence; it has punctuation, capitalization, and spaces!" .)
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!/.")
    ((.. "This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "../This is a sentence; it has punctuation, capitalization, and spaces!")
    (("\001\255" .) -> "\001\255/.")
    ((.. "\001\255") -> "../\001\255")
    ((.hidden bin.exe) -> .hidden/bin.exe)
    ((.hidden bin exe.file) -> .hidden/bin/exe.file)
    |}]
;;

let of_parts_nonempty = File_path.Relative.of_parts_nonempty

let%expect_test _ =
  Helpers.test
    of_parts_nonempty
    ~input:(module Helpers.Nonempty_list (File_path.Part))
    ~output:(module File_path.Relative)
    ~examples:
      (List.filter_map Examples.Part.lists_for_conversion ~f:Nonempty_list.of_list)
    ~correctness:(fun parts of_parts_nonempty ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (of_parts (Nonempty_list.to_list parts))
        (Some of_parts_nonempty)
        ~message:"[of_parts_nonempty] and [of_parts] are inconsistent");
  [%expect
    {|
    ((.) -> .)
    ((..) -> ..)
    ((filename.txt) -> filename.txt)
    ((bin) -> bin)
    ((.hidden) -> .hidden)
    (("This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!")
    (("\001\255") -> "\001\255")
    ((. .) -> ./.)
    ((.. .) -> ../.)
    ((.. .) -> ../.)
    ((.. ..) -> ../..)
    ((filename.txt .) -> filename.txt/.)
    ((.. filename.txt) -> ../filename.txt)
    ((bin .) -> bin/.)
    ((.. bin) -> ../bin)
    ((.hidden .) -> .hidden/.)
    ((.. .hidden) -> ../.hidden)
    (("This is a sentence; it has punctuation, capitalization, and spaces!" .)
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!/.")
    ((.. "This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "../This is a sentence; it has punctuation, capitalization, and spaces!")
    (("\001\255" .) -> "\001\255/.")
    ((.. "\001\255") -> "../\001\255")
    ((.hidden bin.exe) -> .hidden/bin.exe)
    ((.hidden bin exe.file) -> .hidden/bin/exe.file)
    |}]
;;

let simplify_dot = File_path.Relative.simplify_dot

let%expect_test _ =
  Helpers.test
    simplify_dot
    ~input:(module File_path.Relative)
    ~output:(module File_path.Relative)
    ~examples:Examples.Relative.for_simplify
    ~correctness:(fun original simplified ->
      require_equal
        (module File_path.Relative)
        simplified
        (to_parts original
         |> List.filter ~f:(File_path.Part.( <> ) File_path.Part.dot)
         |> of_parts_defaulting_to_dot)
        ~message:"[simplify_dot] and is not equivalent to filtering out [.]";
      if equal original simplified
      then
        require_no_allocation (fun () ->
          ignore (Sys.opaque_identity (simplify_dot original) : t)));
  [%expect
    {|
    (. -> .)
    (.. -> ..)
    (filename.txt -> filename.txt)
    (bin -> bin)
    (.hidden -> .hidden)
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!")
    ("\001\255" -> "\001\255")
    (a/b -> a/b)
    (a/b/. -> a/b)
    (a/./b -> a/b)
    (./a/b -> a/b)
    (./a/./b/. -> a/b)
    (a/b/./. -> a/b)
    (a/././b -> a/b)
    (././a/b -> a/b)
    (././a/././b/./. -> a/b)
    (a/b/.. -> a/b/..)
    (a/../b -> a/../b)
    (../a/b -> ../a/b)
    (../a/../b/.. -> ../a/../b/..)
    (a/b/../.. -> a/b/../..)
    (a/../../b -> a/../../b)
    (../../a/b -> ../../a/b)
    (../../a/../../b/../.. -> ../../a/../../b/../..)
    (a/b/./.. -> a/b/..)
    (a/./../b -> a/../b)
    (./../a/b -> ../a/b)
    (./../a/./../b/./.. -> ../a/../b/..)
    (a/b/../. -> a/b/..)
    (a/.././b -> a/../b)
    (.././a/b -> ../a/b)
    (.././a/.././b/../. -> ../a/../b/..)
    |}]
;;

let simplify_dot_and_dot_dot_naively = File_path.Relative.simplify_dot_and_dot_dot_naively

let%expect_test _ =
  Helpers.test
    simplify_dot_and_dot_dot_naively
    ~input:(module File_path.Relative)
    ~output:(module File_path.Relative)
    ~examples:Examples.Relative.for_simplify
    ~correctness:(fun original simplified ->
      require_equal
        (module Helpers.List (File_path.Part))
        (to_parts simplified
         |> List.drop_while ~f:(File_path.Part.( = ) File_path.Part.dot_dot))
        (to_parts simplified
         |> List.filter ~f:(File_path.Part.( <> ) File_path.Part.dot_dot))
        ~message:
          "not all [..] parts in [simplify_dot_and_dot_dot_naively] are at the beginning";
      require_equal
        (module File_path.Relative)
        simplified
        (simplify_dot_and_dot_dot_naively (simplify_dot original))
        ~message:"[simplify_dot_and_dot_dot_naively] does not ignore [.] parts";
      require_equal
        (module File_path.Relative)
        simplified
        (simplify_dot simplified)
        ~message:"[simplify_dot_and_dot_dot_naively] does not simplify all [.] parts";
      require_equal
        (module File_path.Relative)
        simplified
        (simplify_dot_and_dot_dot_naively simplified)
        ~message:"[simplify_dot_and_dot_dot_naively] is not idempotent";
      if equal original simplified
      then
        require_no_allocation (fun () ->
          ignore (Sys.opaque_identity (simplify_dot_and_dot_dot_naively original) : t)));
  [%expect
    {|
    (. -> .)
    (.. -> ..)
    (filename.txt -> filename.txt)
    (bin -> bin)
    (.hidden -> .hidden)
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!")
    ("\001\255" -> "\001\255")
    (a/b -> a/b)
    (a/b/. -> a/b)
    (a/./b -> a/b)
    (./a/b -> a/b)
    (./a/./b/. -> a/b)
    (a/b/./. -> a/b)
    (a/././b -> a/b)
    (././a/b -> a/b)
    (././a/././b/./. -> a/b)
    (a/b/.. -> a)
    (a/../b -> b)
    (../a/b -> ../a/b)
    (../a/../b/.. -> ..)
    (a/b/../.. -> .)
    (a/../../b -> ../b)
    (../../a/b -> ../../a/b)
    (../../a/../../b/../.. -> ../../../..)
    (a/b/./.. -> a)
    (a/./../b -> b)
    (./../a/b -> ../a/b)
    (./../a/./../b/./.. -> ..)
    (a/b/../. -> a)
    (a/.././b -> b)
    (.././a/b -> ../a/b)
    (.././a/.././b/../. -> ..)
    |}]
;;

(* Test command-line autocompletion separately. *)
include Test_relative_completion
