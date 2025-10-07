(* See comment in [test_path.ml]. *)

open! Core
open Expect_test_helpers_core

type t = File_path.Part.t
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp, sexp_grammar]

let dot = File_path.Part.dot
let dot_dot = File_path.Part.dot_dot

let%expect_test _ =
  Helpers.test_constants (module File_path.Part) [ dot; dot_dot ];
  [%expect
    {|
    .
    ..
    |}]
;;

include (
  File_path.Part :
  sig
  @@ portable
    include
      Identifiable.S
      with type t := t
       and type comparator_witness = File_path.Part.comparator_witness
  end)

let%expect_test _ =
  Helpers.test_compare (module File_path.Part) Examples.Part.for_compare;
  [%expect
    {|
    ("\001\255"
     -dot-is-not-always-first
     .
     ..
     .hidden
     "This is a sentence; it has punctuation, capitalization, and spaces!"
     bin
     bin.exe
     binary
     filename.txt
     "\255\001")
    |}]
;;

let%expect_test _ =
  Helpers.test_of_string (module File_path.Part) Examples.Part.strings_for_of_string;
  [%expect
    {|
    (= .)
    (= ..)
    (= filename.txt)
    (= bin)
    (= .hidden)
    (= "This is a sentence; it has punctuation, capitalization, and spaces!")
    (= "\001\255")
    (! ("File_path.Part.of_string: invalid string" ""))
    (! ("File_path.Part.of_string: invalid string" invalid/slash))
    (! ("File_path.Part.of_string: invalid string" "invalid\000null"))
    |}]
;;

let%expect_test _ =
  Helpers.test_containers (module File_path.Part) Examples.Part.for_conversion;
  [%expect
    {|
    (Set
     ("\001\255"
      .
      ..
      .hidden
      "This is a sentence; it has punctuation, capitalization, and spaces!"
      bin
      filename.txt))
    (Map
     (("\001\255" 0)
      (. 1)
      (.. 2)
      (.hidden 3)
      ("This is a sentence; it has punctuation, capitalization, and spaces!" 4)
      (bin 5)
      (filename.txt 6)))
    (Hash_set
     ("\001\255"
      .
      ..
      .hidden
      "This is a sentence; it has punctuation, capitalization, and spaces!"
      bin
      filename.txt))
    (Table
     (("\001\255" 0)
      (. 1)
      (.. 2)
      (.hidden 3)
      ("This is a sentence; it has punctuation, capitalization, and spaces!" 4)
      (bin 5)
      (filename.txt 6)))
    |}]
;;

let invariant = File_path.Part.invariant

module Expert = struct
  let unchecked_of_canonical_string = File_path.Part.Expert.unchecked_of_canonical_string
end

let%expect_test _ =
  Helpers.test_invariant (module File_path.Part) Examples.Part.strings_for_of_string;
  [%expect
    {|
    (= .)
    (= ..)
    (= filename.txt)
    (= bin)
    (= .hidden)
    (= "This is a sentence; it has punctuation, capitalization, and spaces!")
    (= "\001\255")
    (! ("File_path.Part.invariant: invalid string" ""))
    (! ("File_path.Part.invariant: invalid string" invalid/slash))
    (! ("File_path.Part.invariant: invalid string" "invalid\000null"))
    |}]
;;

let append_to_basename_exn = File_path.Part.append_to_basename_exn

let%expect_test _ =
  Helpers.test
    (fun (path, string) ->
      Or_error.try_with (fun () -> append_to_basename_exn path string))
    ~input:(module Helpers.Tuple2 (File_path.Part) (String))
    ~output:(module Helpers.Or_error (File_path.Part))
    ~examples:Examples.Part.for_append_to_basename
    ~correctness:(fun (path, string) append_to_basename_exn ->
      require_equal
        (module Helpers.Option (File_path.Part))
        (Or_error.ok append_to_basename_exn)
        (Option.try_with (fun () -> of_string (to_string path ^ string))));
  [%expect
    {|
    ((. x) -> (Ok .x))
    ((.. y) -> (Ok ..y))
    ((a .b) -> (Ok a.b))
    ((b invalid/slash)
     ->
     (Error
      ("File_path.Part.append_to_basename_exn: suffix contains invalid characters"
       ((path b) (suffix invalid/slash)))))
    ((c "invalid\000null")
     ->
     (Error
      ("File_path.Part.append_to_basename_exn: suffix contains invalid characters"
       ((path c) (suffix "invalid\000null")))))
    ((long-hyphenated-name-ending-in -this)
     ->
     (Ok long-hyphenated-name-ending-in-this))
    |}]
;;

(* Test command-line autocompletion separately. *)
include Test_part_completion
