(* See comment in [test_path.ml]. *)

open! Core

type t = File_path.Part.t [@@deriving quickcheck, sexp]

let dot = File_path.Part.dot
let dot_dot = File_path.Part.dot_dot

let%expect_test _ =
  Helpers.test_constants (module File_path.Part) [ dot; dot_dot ];
  [%expect {|
    .
    .. |}]
;;

include (
  File_path.Part :
    Identifiable.S
  with type t := t
   and type comparator_witness = File_path.Part.comparator_witness)

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
     "\255\001") |}]
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
    (! ("File_path.Part.of_string: invalid string" "invalid\000null")) |}]
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
      (filename.txt 6))) |}]
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
    (! ("File_path.Part.invariant: invalid string" "invalid\000null")) |}]
;;

(* Test command-line autocompletion separately. *)
include Test_part_completion
