(* We constrain these modules to provide the full interface of [File_path] and its
   submodules to make sure we test (nearly) every function. We do not test all ppx-derived
   or functor-generated functions, but otherwise we test everything. Any time a new
   binding is added to the library, it is required here; every binding added here should
   come with a test unless there is a compelling reason not to.

   We use [Helpers] for all tests. In tests requiring a [correctness] callback, we provide
   one testing the function against previously tested functions in the library. We only
   leave [correctness] tests empty if all functions it is tested against come below.

   We provide [~message] or [~if_false_then_print_s] arguments to all [require*]
   functions, mostly to make the purpose of tests more readable for reviewers. *)

open! Core
open Expect_test_helpers_core

type t = File_path.t
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp, sexp_grammar]

let root = File_path.root
let dot = File_path.dot
let dot_dot = File_path.dot_dot

let%expect_test _ =
  Helpers.test_constants (module File_path) [ root; dot; dot_dot ];
  [%expect
    {|
    /
    .
    ..
    |}]
;;

include (
  File_path :
  sig
  @@ portable
    include
      Identifiable.S
      with type t := t
       and type comparator_witness = File_path.comparator_witness
  end)

let%expect_test _ =
  Helpers.test_compare (module File_path) Examples.for_compare;
  [%expect
    {|
    (/
     "/\001\255"
     /-dot-is-not-always-first
     /.
     /./.
     /././.
     /./..
     /..
     /../.
     /../..
     /.hidden
     "/This is a sentence; it has punctuation, capitalization, and spaces!"
     /bin
     /bin/exe
     /bin/exe/file
     /bin/exe.file
     /bin.exe
     /binary
     /filename.txt
     "/\255\001"
     "\001\255"
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
  Helpers.test_of_string (module File_path) Examples.strings_for_of_string;
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
    (= /)
    (= /.)
    (= /..)
    (= /filename.txt)
    (= /bin)
    (= /.hidden)
    (= "/This is a sentence; it has punctuation, capitalization, and spaces!")
    (= "/\001\255")
    (= /./.)
    (= /../..)
    (= /././.)
    (= /bin/exe)
    (= /bin/exe/file)
    (~ ./ .)
    (~ .//. ./.)
    (~ .//.// ./.)
    (~ bin/exe/ bin/exe)
    (~ bin//exe//file bin/exe/file)
    (~ bin//exe//file/ bin/exe/file)
    (~ // /)
    (~ //. /.)
    (~ /./ /.)
    (~ /.//. /./.)
    (~ /.//.// /./.)
    (~ /bin/exe/ /bin/exe)
    (~ /bin//exe//file /bin/exe/file)
    (~ /bin//exe//file/ /bin/exe/file)
    (! ("File_path.of_string: invalid string" ""))
    (! ("File_path.of_string: invalid string" "invalid/\000/null"))
    |}]
;;

let%expect_test _ =
  Helpers.test_containers (module File_path) Examples.for_conversion;
  [%expect
    {|
    (Set
     (/
      "/\001\255"
      /.
      /./.
      /././.
      /..
      /../..
      /.hidden
      "/This is a sentence; it has punctuation, capitalization, and spaces!"
      /bin
      /bin/exe
      /bin/exe/file
      /filename.txt
      "\001\255"
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
     ((/ 0)
      ("/\001\255" 1)
      (/. 2)
      (/./. 3)
      (/././. 4)
      (/.. 5)
      (/../.. 6)
      (/.hidden 7)
      ("/This is a sentence; it has punctuation, capitalization, and spaces!" 8)
      (/bin 9)
      (/bin/exe 10)
      (/bin/exe/file 11)
      (/filename.txt 12)
      ("\001\255" 13)
      (. 14)
      (./. 15)
      (././. 16)
      (.. 17)
      (../.. 18)
      (.hidden 19)
      ("This is a sentence; it has punctuation, capitalization, and spaces!" 20)
      (bin 21)
      (bin/exe 22)
      (bin/exe/file 23)
      (filename.txt 24)))
    (Hash_set
     (/
      "/\001\255"
      /.
      /./.
      /././.
      /..
      /../..
      /.hidden
      "/This is a sentence; it has punctuation, capitalization, and spaces!"
      /bin
      /bin/exe
      /bin/exe/file
      /filename.txt
      "\001\255"
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
     ((/ 0)
      ("/\001\255" 1)
      (/. 2)
      (/./. 3)
      (/././. 4)
      (/.. 5)
      (/../.. 6)
      (/.hidden 7)
      ("/This is a sentence; it has punctuation, capitalization, and spaces!" 8)
      (/bin 9)
      (/bin/exe 10)
      (/bin/exe/file 11)
      (/filename.txt 12)
      ("\001\255" 13)
      (. 14)
      (./. 15)
      (././. 16)
      (.. 17)
      (../.. 18)
      (.hidden 19)
      ("This is a sentence; it has punctuation, capitalization, and spaces!" 20)
      (bin 21)
      (bin/exe 22)
      (bin/exe/file 23)
      (filename.txt 24)))
    |}]
;;

module Expert = struct
  let unchecked_of_canonical_string = File_path.Expert.unchecked_of_canonical_string
end

let invariant = File_path.invariant

let%expect_test _ =
  Helpers.test_invariant (module File_path) Examples.strings_for_of_string;
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
    (= /)
    (= /.)
    (= /..)
    (= /filename.txt)
    (= /bin)
    (= /.hidden)
    (= "/This is a sentence; it has punctuation, capitalization, and spaces!")
    (= "/\001\255")
    (= /./.)
    (= /../..)
    (= /././.)
    (= /bin/exe)
    (= /bin/exe/file)
    (! ("File_path.invariant: non-canonical representation" ./))
    (! ("File_path.invariant: non-canonical representation" .//.))
    (! ("File_path.invariant: non-canonical representation" .//.//))
    (! ("File_path.invariant: non-canonical representation" bin/exe/))
    (! ("File_path.invariant: non-canonical representation" bin//exe//file))
    (! ("File_path.invariant: non-canonical representation" bin//exe//file/))
    (! ("File_path.invariant: non-canonical representation" //))
    (! ("File_path.invariant: non-canonical representation" //.))
    (! ("File_path.invariant: non-canonical representation" /./))
    (! ("File_path.invariant: non-canonical representation" /.//.))
    (! ("File_path.invariant: non-canonical representation" /.//.//))
    (! ("File_path.invariant: non-canonical representation" /bin/exe/))
    (! ("File_path.invariant: non-canonical representation" /bin//exe//file))
    (! ("File_path.invariant: non-canonical representation" /bin//exe//file/))
    (! ("File_path.invariant: invalid string" ""))
    (! ("File_path.invariant: invalid string" "invalid/\000/null"))
    |}]
;;

let is_relative = File_path.is_relative

let%expect_test _ =
  Helpers.test_predicate
    is_relative
    ~input:(module File_path)
    ~examples:Examples.for_conversion
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    ((success
      (.
       ..
       filename.txt
       bin
       .hidden
       "This is a sentence; it has punctuation, capitalization, and spaces!"
       "\001\255"
       ./.
       ../..
       ././.
       bin/exe
       bin/exe/file))
     (failure
      (/
       /.
       /..
       /filename.txt
       /bin
       /.hidden
       "/This is a sentence; it has punctuation, capitalization, and spaces!"
       "/\001\255"
       /./.
       /../..
       /././.
       /bin/exe
       /bin/exe/file)))
    |}]
;;

let is_absolute = File_path.is_absolute

let%expect_test _ =
  Helpers.test_predicate
    is_absolute
    ~input:(module File_path)
    ~examples:Examples.for_conversion
    ~correctness:(fun path is_absolute ->
      require_equal
        (module Bool)
        is_absolute
        (not (is_relative path))
        ~message:"[is_absolute] and [is_relative] are inconsistent");
  [%expect
    {|
    ((success
      (/
       /.
       /..
       /filename.txt
       /bin
       /.hidden
       "/This is a sentence; it has punctuation, capitalization, and spaces!"
       "/\001\255"
       /./.
       /../..
       /././.
       /bin/exe
       /bin/exe/file))
     (failure
      (.
       ..
       filename.txt
       bin
       .hidden
       "This is a sentence; it has punctuation, capitalization, and spaces!"
       "\001\255"
       ./.
       ../..
       ././.
       bin/exe
       bin/exe/file)))
    |}]
;;

let to_absolute = File_path.to_absolute

let%expect_test _ =
  Helpers.test
    to_absolute
    ~input:(module File_path)
    ~output:(module Helpers.Option (File_path.Absolute))
    ~examples:Examples.for_conversion
    ~correctness:(fun path to_absolute ->
      require_equal
        (module Bool)
        (Option.is_some to_absolute)
        (is_absolute path)
        ~message:"[to_absolute] and [is_absolute] are inconsistent");
  [%expect
    {|
    (. -> ())
    (.. -> ())
    (filename.txt -> ())
    (bin -> ())
    (.hidden -> ())
    ("This is a sentence; it has punctuation, capitalization, and spaces!" -> ())
    ("\001\255" -> ())
    (./. -> ())
    (../.. -> ())
    (././. -> ())
    (bin/exe -> ())
    (bin/exe/file -> ())
    (/ -> (/))
    (/. -> (/.))
    (/.. -> (/..))
    (/filename.txt -> (/filename.txt))
    (/bin -> (/bin))
    (/.hidden -> (/.hidden))
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     ("/This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("/\001\255" -> ("/\001\255"))
    (/./. -> (/./.))
    (/../.. -> (/../..))
    (/././. -> (/././.))
    (/bin/exe -> (/bin/exe))
    (/bin/exe/file -> (/bin/exe/file))
    |}]
;;

let to_relative = File_path.to_relative

let%expect_test _ =
  Helpers.test
    to_relative
    ~input:(module File_path)
    ~output:(module Helpers.Option (File_path.Relative))
    ~examples:Examples.for_conversion
    ~correctness:(fun path to_relative ->
      require_equal
        (module Bool)
        (Option.is_some to_relative)
        (is_relative path)
        ~message:"[to_relative] and [is_relative] are inconsistent");
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
    (./. -> (./.))
    (../.. -> (../..))
    (././. -> (././.))
    (bin/exe -> (bin/exe))
    (bin/exe/file -> (bin/exe/file))
    (/ -> ())
    (/. -> ())
    (/.. -> ())
    (/filename.txt -> ())
    (/bin -> ())
    (/.hidden -> ())
    ("/This is a sentence; it has punctuation, capitalization, and spaces!" -> ())
    ("/\001\255" -> ())
    (/./. -> ())
    (/../.. -> ())
    (/././. -> ())
    (/bin/exe -> ())
    (/bin/exe/file -> ())
    |}]
;;

let to_absolute_exn = File_path.to_absolute_exn

let%expect_test _ =
  Helpers.test
    (fun t -> Or_error.try_with (fun () -> to_absolute_exn t))
    ~input:(module File_path)
    ~output:(module Helpers.Or_error (File_path.Absolute))
    ~examples:Examples.for_conversion
    ~correctness:(fun path to_absolute_exn ->
      require_equal
        (module Helpers.Option (File_path.Absolute))
        (Or_error.ok to_absolute_exn)
        (to_absolute path)
        ~message:"[to_absolute_exn] and [to_absolute] are inconsistent");
  [%expect
    {|
    (. -> (Error ("File_path.to_absolute_exn: path is relative" .)))
    (.. -> (Error ("File_path.to_absolute_exn: path is relative" ..)))
    (filename.txt
     ->
     (Error ("File_path.to_absolute_exn: path is relative" filename.txt)))
    (bin -> (Error ("File_path.to_absolute_exn: path is relative" bin)))
    (.hidden -> (Error ("File_path.to_absolute_exn: path is relative" .hidden)))
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Error
      ("File_path.to_absolute_exn: path is relative"
       "This is a sentence; it has punctuation, capitalization, and spaces!")))
    ("\001\255"
     ->
     (Error ("File_path.to_absolute_exn: path is relative" "\001\255")))
    (./. -> (Error ("File_path.to_absolute_exn: path is relative" ./.)))
    (../.. -> (Error ("File_path.to_absolute_exn: path is relative" ../..)))
    (././. -> (Error ("File_path.to_absolute_exn: path is relative" ././.)))
    (bin/exe -> (Error ("File_path.to_absolute_exn: path is relative" bin/exe)))
    (bin/exe/file
     ->
     (Error ("File_path.to_absolute_exn: path is relative" bin/exe/file)))
    (/ -> (Ok /))
    (/. -> (Ok /.))
    (/.. -> (Ok /..))
    (/filename.txt -> (Ok /filename.txt))
    (/bin -> (Ok /bin))
    (/.hidden -> (Ok /.hidden))
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Ok "/This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("/\001\255" -> (Ok "/\001\255"))
    (/./. -> (Ok /./.))
    (/../.. -> (Ok /../..))
    (/././. -> (Ok /././.))
    (/bin/exe -> (Ok /bin/exe))
    (/bin/exe/file -> (Ok /bin/exe/file))
    |}]
;;

let to_relative_exn = File_path.to_relative_exn

let%expect_test _ =
  Helpers.test
    (fun t -> Or_error.try_with (fun () -> to_relative_exn t))
    ~input:(module File_path)
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.for_conversion
    ~correctness:(fun path to_relative_exn ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (Or_error.ok to_relative_exn)
        (to_relative path)
        ~message:"[to_relative_exn] and [to_relative] are inconsistent");
  [%expect
    {|
    (. -> (Ok .))
    (.. -> (Ok ..))
    (filename.txt -> (Ok filename.txt))
    (bin -> (Ok bin))
    (.hidden -> (Ok .hidden))
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Ok "This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("\001\255" -> (Ok "\001\255"))
    (./. -> (Ok ./.))
    (../.. -> (Ok ../..))
    (././. -> (Ok ././.))
    (bin/exe -> (Ok bin/exe))
    (bin/exe/file -> (Ok bin/exe/file))
    (/ -> (Error ("File_path.to_relative_exn: path is absolute" /)))
    (/. -> (Error ("File_path.to_relative_exn: path is absolute" /.)))
    (/.. -> (Error ("File_path.to_relative_exn: path is absolute" /..)))
    (/filename.txt
     ->
     (Error ("File_path.to_relative_exn: path is absolute" /filename.txt)))
    (/bin -> (Error ("File_path.to_relative_exn: path is absolute" /bin)))
    (/.hidden -> (Error ("File_path.to_relative_exn: path is absolute" /.hidden)))
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Error
      ("File_path.to_relative_exn: path is absolute"
       "/This is a sentence; it has punctuation, capitalization, and spaces!")))
    ("/\001\255"
     ->
     (Error ("File_path.to_relative_exn: path is absolute" "/\001\255")))
    (/./. -> (Error ("File_path.to_relative_exn: path is absolute" /./.)))
    (/../.. -> (Error ("File_path.to_relative_exn: path is absolute" /../..)))
    (/././. -> (Error ("File_path.to_relative_exn: path is absolute" /././.)))
    (/bin/exe -> (Error ("File_path.to_relative_exn: path is absolute" /bin/exe)))
    (/bin/exe/file
     ->
     (Error ("File_path.to_relative_exn: path is absolute" /bin/exe/file)))
    |}]
;;

let to_absolute_or_error = File_path.to_absolute_or_error

let%expect_test _ =
  Helpers.test
    to_absolute_or_error
    ~input:(module File_path)
    ~output:(module Helpers.Or_error (File_path.Absolute))
    ~examples:Examples.for_conversion
    ~correctness:(fun path to_absolute_or_error ->
      require_equal
        (module Helpers.Option (File_path.Absolute))
        (Or_error.ok to_absolute_or_error)
        (to_absolute path)
        ~message:"[to_absolute_or_error] and [to_absolute] are inconsistent");
  [%expect
    {|
    (. -> (Error ("File_path.to_absolute_or_error: path is relative" .)))
    (.. -> (Error ("File_path.to_absolute_or_error: path is relative" ..)))
    (filename.txt
     ->
     (Error ("File_path.to_absolute_or_error: path is relative" filename.txt)))
    (bin -> (Error ("File_path.to_absolute_or_error: path is relative" bin)))
    (.hidden
     ->
     (Error ("File_path.to_absolute_or_error: path is relative" .hidden)))
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Error
      ("File_path.to_absolute_or_error: path is relative"
       "This is a sentence; it has punctuation, capitalization, and spaces!")))
    ("\001\255"
     ->
     (Error ("File_path.to_absolute_or_error: path is relative" "\001\255")))
    (./. -> (Error ("File_path.to_absolute_or_error: path is relative" ./.)))
    (../.. -> (Error ("File_path.to_absolute_or_error: path is relative" ../..)))
    (././. -> (Error ("File_path.to_absolute_or_error: path is relative" ././.)))
    (bin/exe
     ->
     (Error ("File_path.to_absolute_or_error: path is relative" bin/exe)))
    (bin/exe/file
     ->
     (Error ("File_path.to_absolute_or_error: path is relative" bin/exe/file)))
    (/ -> (Ok /))
    (/. -> (Ok /.))
    (/.. -> (Ok /..))
    (/filename.txt -> (Ok /filename.txt))
    (/bin -> (Ok /bin))
    (/.hidden -> (Ok /.hidden))
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Ok "/This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("/\001\255" -> (Ok "/\001\255"))
    (/./. -> (Ok /./.))
    (/../.. -> (Ok /../..))
    (/././. -> (Ok /././.))
    (/bin/exe -> (Ok /bin/exe))
    (/bin/exe/file -> (Ok /bin/exe/file))
    |}]
;;

let to_relative_or_error = File_path.to_relative_or_error

let%expect_test _ =
  Helpers.test
    to_relative_or_error
    ~input:(module File_path)
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.for_conversion
    ~correctness:(fun path to_relative_or_error ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (Or_error.ok to_relative_or_error)
        (to_relative path)
        ~message:"[to_relative_or_error] and [to_relative] are inconsistent");
  [%expect
    {|
    (. -> (Ok .))
    (.. -> (Ok ..))
    (filename.txt -> (Ok filename.txt))
    (bin -> (Ok bin))
    (.hidden -> (Ok .hidden))
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Ok "This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("\001\255" -> (Ok "\001\255"))
    (./. -> (Ok ./.))
    (../.. -> (Ok ../..))
    (././. -> (Ok ././.))
    (bin/exe -> (Ok bin/exe))
    (bin/exe/file -> (Ok bin/exe/file))
    (/ -> (Error ("File_path.to_relative_or_error: path is absolute" /)))
    (/. -> (Error ("File_path.to_relative_or_error: path is absolute" /.)))
    (/.. -> (Error ("File_path.to_relative_or_error: path is absolute" /..)))
    (/filename.txt
     ->
     (Error ("File_path.to_relative_or_error: path is absolute" /filename.txt)))
    (/bin -> (Error ("File_path.to_relative_or_error: path is absolute" /bin)))
    (/.hidden
     ->
     (Error ("File_path.to_relative_or_error: path is absolute" /.hidden)))
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Error
      ("File_path.to_relative_or_error: path is absolute"
       "/This is a sentence; it has punctuation, capitalization, and spaces!")))
    ("/\001\255"
     ->
     (Error ("File_path.to_relative_or_error: path is absolute" "/\001\255")))
    (/./. -> (Error ("File_path.to_relative_or_error: path is absolute" /./.)))
    (/../..
     ->
     (Error ("File_path.to_relative_or_error: path is absolute" /../..)))
    (/././.
     ->
     (Error ("File_path.to_relative_or_error: path is absolute" /././.)))
    (/bin/exe
     ->
     (Error ("File_path.to_relative_or_error: path is absolute" /bin/exe)))
    (/bin/exe/file
     ->
     (Error ("File_path.to_relative_or_error: path is absolute" /bin/exe/file)))
    |}]
;;

let of_absolute = File_path.of_absolute

let%expect_test _ =
  Helpers.test
    of_absolute
    ~input:(module File_path.Absolute)
    ~output:(module File_path)
    ~examples:Examples.Absolute.for_conversion
    ~correctness:(fun absolute path ->
      require_equal
        (module Helpers.Option (File_path.Absolute))
        (to_absolute path)
        (Some absolute)
        ~message:"[of_absolute] and [to_absolute] are inconsistent");
  [%expect
    {|
    (/ -> /)
    (/. -> /.)
    (/.. -> /..)
    (/filename.txt -> /filename.txt)
    (/bin -> /bin)
    (/.hidden -> /.hidden)
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     "/This is a sentence; it has punctuation, capitalization, and spaces!")
    ("/\001\255" -> "/\001\255")
    (/./. -> /./.)
    (/../.. -> /../..)
    (/././. -> /././.)
    (/bin/exe -> /bin/exe)
    (/bin/exe/file -> /bin/exe/file)
    |}]
;;

let of_relative = File_path.of_relative

let%expect_test _ =
  Helpers.test
    of_relative
    ~input:(module File_path.Relative)
    ~output:(module File_path)
    ~examples:Examples.Relative.for_conversion
    ~correctness:(fun relative path ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (to_relative path)
        (Some relative)
        ~message:"[of_relative] and [to_relative] are inconsistent");
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
    (./. -> ./.)
    (../.. -> ../..)
    (././. -> ././.)
    (bin/exe -> bin/exe)
    (bin/exe/file -> bin/exe/file)
    |}]
;;

let of_part_relative = File_path.of_part_relative

let%expect_test _ =
  Helpers.test
    of_part_relative
    ~input:(module File_path.Part)
    ~output:(module File_path)
    ~examples:Examples.Part.for_conversion
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
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

let number_of_parts = File_path.number_of_parts

let%expect_test _ =
  Helpers.test
    number_of_parts
    ~input:(module File_path)
    ~output:(module Int)
    ~examples:Examples.for_conversion
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
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
    (/ -> 0)
    (/. -> 1)
    (/.. -> 1)
    (/filename.txt -> 1)
    (/bin -> 1)
    (/.hidden -> 1)
    ("/This is a sentence; it has punctuation, capitalization, and spaces!" -> 1)
    ("/\001\255" -> 1)
    (/./. -> 2)
    (/../.. -> 2)
    (/././. -> 3)
    (/bin/exe -> 2)
    (/bin/exe/file -> 3)
    |}]
;;

let basename = File_path.basename

let%expect_test _ =
  Helpers.test
    basename
    ~input:(module File_path)
    ~output:(module Helpers.Option (File_path.Part))
    ~examples:Examples.for_basename_and_dirname
    ~correctness:(fun path basename ->
      require_equal
        (module Bool)
        (Option.is_none basename)
        (equal path root)
        ~message:"[basename] is inconsistent with [equal root]");
  [%expect
    {|
    (. -> (.))
    (.. -> (..))
    (singleton -> (singleton))
    (./file -> (file))
    (dir/. -> (.))
    (../.. -> (..))
    (a/b -> (b))
    (a/b/c -> (c))
    (a/b/c/d -> (d))
    (long/chain/of/names/ending/in/this -> (this))
    (/ -> ())
    (/. -> (.))
    (/.. -> (..))
    (/singleton -> (singleton))
    (/./file -> (file))
    (/dir/. -> (.))
    (/../.. -> (..))
    (/a/b -> (b))
    (/a/b/c -> (c))
    (/a/b/c/d -> (d))
    (/long/chain/of/names/ending/in/this -> (this))
    |}]
;;

let basename_exn = File_path.basename_exn

let%expect_test _ =
  Helpers.test
    (fun path -> Or_error.try_with (fun () -> basename_exn path))
    ~input:(module File_path)
    ~output:(module Helpers.Or_error (File_path.Part))
    ~examples:Examples.for_basename_and_dirname
    ~correctness:(fun path basename_exn ->
      require_equal
        (module Helpers.Option (File_path.Part))
        (Or_error.ok basename_exn)
        (basename path)
        ~message:"[basename_exn] and [basename] are inconsistent");
  [%expect
    {|
    (. -> (Ok .))
    (.. -> (Ok ..))
    (singleton -> (Ok singleton))
    (./file -> (Ok file))
    (dir/. -> (Ok .))
    (../.. -> (Ok ..))
    (a/b -> (Ok b))
    (a/b/c -> (Ok c))
    (a/b/c/d -> (Ok d))
    (long/chain/of/names/ending/in/this -> (Ok this))
    (/ -> (Error "File_path.basename_exn: root path"))
    (/. -> (Ok .))
    (/.. -> (Ok ..))
    (/singleton -> (Ok singleton))
    (/./file -> (Ok file))
    (/dir/. -> (Ok .))
    (/../.. -> (Ok ..))
    (/a/b -> (Ok b))
    (/a/b/c -> (Ok c))
    (/a/b/c/d -> (Ok d))
    (/long/chain/of/names/ending/in/this -> (Ok this))
    |}]
;;

let basename_or_error = File_path.basename_or_error

let%expect_test _ =
  Helpers.test
    basename_or_error
    ~input:(module File_path)
    ~output:(module Helpers.Or_error (File_path.Part))
    ~examples:Examples.for_basename_and_dirname
    ~correctness:(fun path basename_or_error ->
      require_equal
        (module Helpers.Option (File_path.Part))
        (Or_error.ok basename_or_error)
        (basename path)
        ~message:"[basename_or_error] and [basename] are inconsistent");
  [%expect
    {|
    (. -> (Ok .))
    (.. -> (Ok ..))
    (singleton -> (Ok singleton))
    (./file -> (Ok file))
    (dir/. -> (Ok .))
    (../.. -> (Ok ..))
    (a/b -> (Ok b))
    (a/b/c -> (Ok c))
    (a/b/c/d -> (Ok d))
    (long/chain/of/names/ending/in/this -> (Ok this))
    (/ -> (Error "File_path.basename_or_error: root path"))
    (/. -> (Ok .))
    (/.. -> (Ok ..))
    (/singleton -> (Ok singleton))
    (/./file -> (Ok file))
    (/dir/. -> (Ok .))
    (/../.. -> (Ok ..))
    (/a/b -> (Ok b))
    (/a/b/c -> (Ok c))
    (/a/b/c/d -> (Ok d))
    (/long/chain/of/names/ending/in/this -> (Ok this))
    |}]
;;

let basename_defaulting_to_dot = File_path.basename_defaulting_to_dot

let%expect_test _ =
  Helpers.test
    basename_defaulting_to_dot
    ~input:(module File_path)
    ~output:(module File_path.Part)
    ~examples:Examples.for_basename_and_dirname
    ~correctness:(fun path basename_defaulting_to_dot ->
      require_equal
        (module File_path.Part)
        basename_defaulting_to_dot
        (Option.value (basename path) ~default:File_path.Part.dot)
        ~message:"[basename_defaulting_to_dot] and [basename] are inconsistent");
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
    (/ -> .)
    (/. -> .)
    (/.. -> ..)
    (/singleton -> singleton)
    (/./file -> file)
    (/dir/. -> .)
    (/../.. -> ..)
    (/a/b -> b)
    (/a/b/c -> c)
    (/a/b/c/d -> d)
    (/long/chain/of/names/ending/in/this -> this)
    |}]
;;

let dirname = File_path.dirname

let%expect_test _ =
  Helpers.test
    dirname
    ~input:(module File_path)
    ~output:(module Helpers.Option (File_path))
    ~examples:Examples.for_basename_and_dirname
    ~correctness:(fun path dirname ->
      require_equal
        (module Bool)
        (Option.is_none dirname)
        (Int.equal (number_of_parts path) (if is_absolute path then 0 else 1))
        ~message:"[dirname] is not [Some] in the right cases");
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
    (/ -> ())
    (/. -> (/))
    (/.. -> (/))
    (/singleton -> (/))
    (/./file -> (/.))
    (/dir/. -> (/dir))
    (/../.. -> (/..))
    (/a/b -> (/a))
    (/a/b/c -> (/a/b))
    (/a/b/c/d -> (/a/b/c))
    (/long/chain/of/names/ending/in/this -> (/long/chain/of/names/ending/in))
    |}]
;;

let dirname_exn = File_path.dirname_exn

let%expect_test _ =
  Helpers.test
    (fun path -> Or_error.try_with (fun () -> dirname_exn path))
    ~input:(module File_path)
    ~output:(module Helpers.Or_error (File_path))
    ~examples:Examples.for_basename_and_dirname
    ~correctness:(fun path dirname_exn ->
      require_equal
        (module Helpers.Option (File_path))
        (Or_error.ok dirname_exn)
        (dirname path)
        ~message:"[dirname_exn] and [dirname] are inconsistent");
  [%expect
    {|
    (. -> (Error ("File_path.dirname_exn: path contains no slash" .)))
    (.. -> (Error ("File_path.dirname_exn: path contains no slash" ..)))
    (singleton
     ->
     (Error ("File_path.dirname_exn: path contains no slash" singleton)))
    (./file -> (Ok .))
    (dir/. -> (Ok dir))
    (../.. -> (Ok ..))
    (a/b -> (Ok a))
    (a/b/c -> (Ok a/b))
    (a/b/c/d -> (Ok a/b/c))
    (long/chain/of/names/ending/in/this -> (Ok long/chain/of/names/ending/in))
    (/ -> (Error "File_path.dirname_exn: root path"))
    (/. -> (Ok /))
    (/.. -> (Ok /))
    (/singleton -> (Ok /))
    (/./file -> (Ok /.))
    (/dir/. -> (Ok /dir))
    (/../.. -> (Ok /..))
    (/a/b -> (Ok /a))
    (/a/b/c -> (Ok /a/b))
    (/a/b/c/d -> (Ok /a/b/c))
    (/long/chain/of/names/ending/in/this -> (Ok /long/chain/of/names/ending/in))
    |}]
;;

let dirname_or_error = File_path.dirname_or_error

let%expect_test _ =
  Helpers.test
    dirname_or_error
    ~input:(module File_path)
    ~output:(module Helpers.Or_error (File_path))
    ~examples:Examples.for_basename_and_dirname
    ~correctness:(fun path dirname_or_error ->
      require_equal
        (module Helpers.Option (File_path))
        (Or_error.ok dirname_or_error)
        (dirname path)
        ~message:"[dirname_or_error] and [dirname] are inconsistent");
  [%expect
    {|
    (. -> (Error ("File_path.dirname_or_error: path contains no slash" .)))
    (.. -> (Error ("File_path.dirname_or_error: path contains no slash" ..)))
    (singleton
     ->
     (Error ("File_path.dirname_or_error: path contains no slash" singleton)))
    (./file -> (Ok .))
    (dir/. -> (Ok dir))
    (../.. -> (Ok ..))
    (a/b -> (Ok a))
    (a/b/c -> (Ok a/b))
    (a/b/c/d -> (Ok a/b/c))
    (long/chain/of/names/ending/in/this -> (Ok long/chain/of/names/ending/in))
    (/ -> (Error "File_path.dirname_or_error: root path"))
    (/. -> (Ok /))
    (/.. -> (Ok /))
    (/singleton -> (Ok /))
    (/./file -> (Ok /.))
    (/dir/. -> (Ok /dir))
    (/../.. -> (Ok /..))
    (/a/b -> (Ok /a))
    (/a/b/c -> (Ok /a/b))
    (/a/b/c/d -> (Ok /a/b/c))
    (/long/chain/of/names/ending/in/this -> (Ok /long/chain/of/names/ending/in))
    |}]
;;

let dirname_defaulting_to_dot_or_root = File_path.dirname_defaulting_to_dot_or_root

let%expect_test _ =
  Helpers.test
    dirname_defaulting_to_dot_or_root
    ~input:(module File_path)
    ~output:(module File_path)
    ~examples:Examples.for_basename_and_dirname
    ~correctness:(fun path dirname_defaulting_to_dot_or_root ->
      require_equal
        (module File_path)
        dirname_defaulting_to_dot_or_root
        (Option.value (dirname path) ~default:(if is_relative path then dot else root))
        ~message:"[dirname_defaulting_to_dot_or_root] and [dirname] are inconsistent");
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
    (/ -> /)
    (/. -> /)
    (/.. -> /)
    (/singleton -> /)
    (/./file -> /.)
    (/dir/. -> /dir)
    (/../.. -> /..)
    (/a/b -> /a)
    (/a/b/c -> /a/b)
    (/a/b/c/d -> /a/b/c)
    (/long/chain/of/names/ending/in/this -> /long/chain/of/names/ending/in)
    |}]
;;

let dirname_and_basename = File_path.dirname_and_basename

let%expect_test _ =
  Helpers.test
    (fun path -> dirname_and_basename path)
    ~input:(module File_path)
    ~output:(module Helpers.Option (Helpers.Tuple2 (File_path) (File_path.Part)))
    ~examples:Examples.for_basename_and_dirname
    ~correctness:(fun path dirname_and_basename ->
      require_equal
        (module Helpers.Option (Helpers.Tuple2 (File_path) (File_path.Part)))
        dirname_and_basename
        (Option.both (dirname path) (basename path))
        ~message:"[dirname_and_basename] and [dirname]/[basename] are inconsistent");
  [%expect
    {|
    (. -> ())
    (.. -> ())
    (singleton -> ())
    (./file -> ((. file)))
    (dir/. -> ((dir .)))
    (../.. -> ((.. ..)))
    (a/b -> ((a b)))
    (a/b/c -> ((a/b c)))
    (a/b/c/d -> ((a/b/c d)))
    (long/chain/of/names/ending/in/this -> ((long/chain/of/names/ending/in this)))
    (/ -> ())
    (/. -> ((/ .)))
    (/.. -> ((/ ..)))
    (/singleton -> ((/ singleton)))
    (/./file -> ((/. file)))
    (/dir/. -> ((/dir .)))
    (/../.. -> ((/.. ..)))
    (/a/b -> ((/a b)))
    (/a/b/c -> ((/a/b c)))
    (/a/b/c/d -> ((/a/b/c d)))
    (/long/chain/of/names/ending/in/this
     ->
     ((/long/chain/of/names/ending/in this)))
    |}]
;;

let append_to_basename_exn = File_path.append_to_basename_exn

let%expect_test _ =
  Helpers.test
    (fun (path, string) ->
      Or_error.try_with (fun () -> append_to_basename_exn path string))
    ~input:(module Helpers.Tuple2 (File_path) (String))
    ~output:(module Helpers.Or_error (File_path))
    ~examples:Examples.for_append_to_basename
    ~correctness:(fun (path, string) append_to_basename_exn ->
      require_equal
        (module Helpers.Option (File_path))
        (Or_error.ok append_to_basename_exn)
        (if equal path root || String.mem string '/' || String.mem string '\000'
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
      ("File_path.append_to_basename_exn: suffix contains invalid characters"
       ((path a/b/c) (suffix invalid/slash)))))
    ((a/b/c "invalid\000null")
     ->
     (Error
      ("File_path.append_to_basename_exn: suffix contains invalid characters"
       ((path a/b/c) (suffix "invalid\000null")))))
    ((long/chain/of/names/ending/in -this)
     ->
     (Ok long/chain/of/names/ending/in-this))
    ((/ "")
     ->
     (Error
      ("File_path.append_to_basename_exn: root path has no basename"
       ((path /) (suffix "")))))
    ((/ x)
     ->
     (Error
      ("File_path.append_to_basename_exn: root path has no basename"
       ((path /) (suffix x)))))
    ((/ invalid/slash)
     ->
     (Error
      ("File_path.append_to_basename_exn: root path has no basename"
       ((path /) (suffix invalid/slash)))))
    ((/ "invalid\000null")
     ->
     (Error
      ("File_path.append_to_basename_exn: root path has no basename"
       ((path /) (suffix "invalid\000null")))))
    ((/. x) -> (Ok /.x))
    ((/.. y) -> (Ok /..y))
    ((/a .b) -> (Ok /a.b))
    ((/a/b .c) -> (Ok /a/b.c))
    ((/a/b/c .d) -> (Ok /a/b/c.d))
    ((/a/b/c "") -> (Ok /a/b/c))
    ((/a/b/c invalid/slash)
     ->
     (Error
      ("File_path.append_to_basename_exn: suffix contains invalid characters"
       ((path /a/b/c) (suffix invalid/slash)))))
    ((/a/b/c "invalid\000null")
     ->
     (Error
      ("File_path.append_to_basename_exn: suffix contains invalid characters"
       ((path /a/b/c) (suffix "invalid\000null")))))
    ((/long/chain/of/names/ending/in -this)
     ->
     (Ok /long/chain/of/names/ending/in-this))
    |}]
;;

let append_part = File_path.append_part

let%expect_test _ =
  Helpers.test
    (fun (path, suffix) -> append_part path suffix)
    ~input:(module Helpers.Tuple2 (File_path) (File_path.Part))
    ~examples:Examples.for_append_part
    ~output:(module File_path)
    ~correctness:(fun (path, part) append_part ->
      require_equal
        (module Helpers.Option (File_path))
        (dirname append_part)
        (Some path)
        ~message:"[append_part] and [dirname] are inconsistent";
      require_equal
        (module Helpers.Option (File_path.Part))
        (basename append_part)
        (Some part)
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
    ((/ .) -> /.)
    ((/ ..) -> /..)
    ((/ singleton) -> /singleton)
    ((/. file) -> /./file)
    ((/dir .) -> /dir/.)
    ((/.. ..) -> /../..)
    ((/a b) -> /a/b)
    ((/a/b c) -> /a/b/c)
    ((/a/b/c d) -> /a/b/c/d)
    ((/long/chain/of/names/ending/in this) -> /long/chain/of/names/ending/in/this)
    |}]
;;

let is_prefix = File_path.is_prefix

let%expect_test _ =
  Helpers.test_predicate
    (fun { t; prefix } -> is_prefix t ~prefix)
    ~input:(module Helpers.With_prefix (File_path))
    ~examples:Examples.for_chop_prefix
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
       ((t long/chain/of/names/ending/in/this) (prefix long/chain/of/names))
       ((t /) (prefix /))
       ((t /.) (prefix /.))
       ((t /..) (prefix /..))
       ((t /a/b/c) (prefix /a/b/c))
       ((t /./file) (prefix /.))
       ((t /dir/.) (prefix /dir))
       ((t /../..) (prefix /..))
       ((t /a/b/c/d) (prefix /a))
       ((t /a/b/c/d) (prefix /a/b))
       ((t /a/b/c/d) (prefix /a/b/c))
       ((t /long/chain/of/names/ending/in/this) (prefix /long/chain/of/names))))
     (failure
      (((t ..) (prefix .))
       ((t ./b) (prefix ./a))
       ((t c/d) (prefix a/b))
       ((t a) (prefix a/b/c))
       ((t a/b) (prefix a/b/c))
       ((t /..) (prefix /.))
       ((t /./b) (prefix /./a))
       ((t /c/d) (prefix /a/b))
       ((t /a) (prefix /a/b/c))
       ((t /a/b) (prefix /a/b/c))
       ((t /) (prefix .))
       ((t .) (prefix /))
       ((t /a/b/c) (prefix a/b))
       ((t a/b/c) (prefix /a/b)))))
    |}]
;;

let chop_prefix = File_path.chop_prefix

let%expect_test _ =
  Helpers.test
    (fun { t; prefix } -> chop_prefix t ~prefix)
    ~input:(module Helpers.With_prefix (File_path))
    ~output:(module Helpers.Option (File_path.Relative))
    ~examples:Examples.for_chop_prefix
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
    (((t /) (prefix /)) -> (.))
    (((t /..) (prefix /.)) -> ())
    (((t /./b) (prefix /./a)) -> ())
    (((t /c/d) (prefix /a/b)) -> ())
    (((t /a) (prefix /a/b/c)) -> ())
    (((t /a/b) (prefix /a/b/c)) -> ())
    (((t /.) (prefix /.)) -> (.))
    (((t /..) (prefix /..)) -> (.))
    (((t /a/b/c) (prefix /a/b/c)) -> (.))
    (((t /./file) (prefix /.)) -> (file))
    (((t /dir/.) (prefix /dir)) -> (.))
    (((t /../..) (prefix /..)) -> (..))
    (((t /a/b/c/d) (prefix /a)) -> (b/c/d))
    (((t /a/b/c/d) (prefix /a/b)) -> (c/d))
    (((t /a/b/c/d) (prefix /a/b/c)) -> (d))
    (((t /long/chain/of/names/ending/in/this) (prefix /long/chain/of/names))
     ->
     (ending/in/this))
    (((t /) (prefix .)) -> ())
    (((t .) (prefix /)) -> ())
    (((t /a/b/c) (prefix a/b)) -> ())
    (((t a/b/c) (prefix /a/b)) -> ())
    |}]
;;

let chop_prefix_exn = File_path.chop_prefix_exn

let%expect_test _ =
  Helpers.test
    (fun { t; prefix } -> Or_error.try_with (fun () -> chop_prefix_exn t ~prefix))
    ~input:(module Helpers.With_prefix (File_path))
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.for_chop_prefix
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
     (Error ("File_path.chop_prefix_exn: not a prefix" ((path ..) (prefix .)))))
    (((t ./b) (prefix ./a))
     ->
     (Error ("File_path.chop_prefix_exn: not a prefix" ((path ./b) (prefix ./a)))))
    (((t c/d) (prefix a/b))
     ->
     (Error ("File_path.chop_prefix_exn: not a prefix" ((path c/d) (prefix a/b)))))
    (((t a) (prefix a/b/c))
     ->
     (Error ("File_path.chop_prefix_exn: not a prefix" ((path a) (prefix a/b/c)))))
    (((t a/b) (prefix a/b/c))
     ->
     (Error
      ("File_path.chop_prefix_exn: not a prefix" ((path a/b) (prefix a/b/c)))))
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
    (((t /) (prefix /)) -> (Ok .))
    (((t /..) (prefix /.))
     ->
     (Error ("File_path.chop_prefix_exn: not a prefix" ((path /..) (prefix /.)))))
    (((t /./b) (prefix /./a))
     ->
     (Error
      ("File_path.chop_prefix_exn: not a prefix" ((path /./b) (prefix /./a)))))
    (((t /c/d) (prefix /a/b))
     ->
     (Error
      ("File_path.chop_prefix_exn: not a prefix" ((path /c/d) (prefix /a/b)))))
    (((t /a) (prefix /a/b/c))
     ->
     (Error
      ("File_path.chop_prefix_exn: not a prefix" ((path /a) (prefix /a/b/c)))))
    (((t /a/b) (prefix /a/b/c))
     ->
     (Error
      ("File_path.chop_prefix_exn: not a prefix" ((path /a/b) (prefix /a/b/c)))))
    (((t /.) (prefix /.)) -> (Ok .))
    (((t /..) (prefix /..)) -> (Ok .))
    (((t /a/b/c) (prefix /a/b/c)) -> (Ok .))
    (((t /./file) (prefix /.)) -> (Ok file))
    (((t /dir/.) (prefix /dir)) -> (Ok .))
    (((t /../..) (prefix /..)) -> (Ok ..))
    (((t /a/b/c/d) (prefix /a)) -> (Ok b/c/d))
    (((t /a/b/c/d) (prefix /a/b)) -> (Ok c/d))
    (((t /a/b/c/d) (prefix /a/b/c)) -> (Ok d))
    (((t /long/chain/of/names/ending/in/this) (prefix /long/chain/of/names))
     ->
     (Ok ending/in/this))
    (((t /) (prefix .))
     ->
     (Error ("File_path.chop_prefix_exn: not a prefix" ((path /) (prefix .)))))
    (((t .) (prefix /))
     ->
     (Error ("File_path.chop_prefix_exn: not a prefix" ((path .) (prefix /)))))
    (((t /a/b/c) (prefix a/b))
     ->
     (Error
      ("File_path.chop_prefix_exn: not a prefix" ((path /a/b/c) (prefix a/b)))))
    (((t a/b/c) (prefix /a/b))
     ->
     (Error
      ("File_path.chop_prefix_exn: not a prefix" ((path a/b/c) (prefix /a/b)))))
    |}]
;;

let chop_prefix_or_error = File_path.chop_prefix_or_error

let%expect_test _ =
  Helpers.test
    (fun { t; prefix } -> chop_prefix_or_error t ~prefix)
    ~input:(module Helpers.With_prefix (File_path))
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.for_chop_prefix
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
      ("File_path.chop_prefix_or_error: not a prefix" ((path ..) (prefix .)))))
    (((t ./b) (prefix ./a))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path ./b) (prefix ./a)))))
    (((t c/d) (prefix a/b))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path c/d) (prefix a/b)))))
    (((t a) (prefix a/b/c))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path a) (prefix a/b/c)))))
    (((t a/b) (prefix a/b/c))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path a/b) (prefix a/b/c)))))
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
    (((t /) (prefix /)) -> (Ok .))
    (((t /..) (prefix /.))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path /..) (prefix /.)))))
    (((t /./b) (prefix /./a))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path /./b) (prefix /./a)))))
    (((t /c/d) (prefix /a/b))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path /c/d) (prefix /a/b)))))
    (((t /a) (prefix /a/b/c))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path /a) (prefix /a/b/c)))))
    (((t /a/b) (prefix /a/b/c))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix"
       ((path /a/b) (prefix /a/b/c)))))
    (((t /.) (prefix /.)) -> (Ok .))
    (((t /..) (prefix /..)) -> (Ok .))
    (((t /a/b/c) (prefix /a/b/c)) -> (Ok .))
    (((t /./file) (prefix /.)) -> (Ok file))
    (((t /dir/.) (prefix /dir)) -> (Ok .))
    (((t /../..) (prefix /..)) -> (Ok ..))
    (((t /a/b/c/d) (prefix /a)) -> (Ok b/c/d))
    (((t /a/b/c/d) (prefix /a/b)) -> (Ok c/d))
    (((t /a/b/c/d) (prefix /a/b/c)) -> (Ok d))
    (((t /long/chain/of/names/ending/in/this) (prefix /long/chain/of/names))
     ->
     (Ok ending/in/this))
    (((t /) (prefix .))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path /) (prefix .)))))
    (((t .) (prefix /))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path .) (prefix /)))))
    (((t /a/b/c) (prefix a/b))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix"
       ((path /a/b/c) (prefix a/b)))))
    (((t a/b/c) (prefix /a/b))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix"
       ((path a/b/c) (prefix /a/b)))))
    |}]
;;

let chop_prefix_if_exists = File_path.chop_prefix_if_exists

let%expect_test _ =
  Helpers.test
    (fun { t; prefix } -> chop_prefix_if_exists t ~prefix)
    ~input:(module Helpers.With_prefix (File_path))
    ~output:(module File_path)
    ~examples:Examples.for_chop_prefix
    ~correctness:(fun { t; prefix } chop_prefix_if_exists ->
      require_equal
        (module File_path)
        (chop_prefix t ~prefix |> Option.value_map ~f:File_path.of_relative ~default:t)
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
    (((t /) (prefix /)) -> .)
    (((t /..) (prefix /.)) -> /..)
    (((t /./b) (prefix /./a)) -> /./b)
    (((t /c/d) (prefix /a/b)) -> /c/d)
    (((t /a) (prefix /a/b/c)) -> /a)
    (((t /a/b) (prefix /a/b/c)) -> /a/b)
    (((t /.) (prefix /.)) -> .)
    (((t /..) (prefix /..)) -> .)
    (((t /a/b/c) (prefix /a/b/c)) -> .)
    (((t /./file) (prefix /.)) -> file)
    (((t /dir/.) (prefix /dir)) -> .)
    (((t /../..) (prefix /..)) -> ..)
    (((t /a/b/c/d) (prefix /a)) -> b/c/d)
    (((t /a/b/c/d) (prefix /a/b)) -> c/d)
    (((t /a/b/c/d) (prefix /a/b/c)) -> d)
    (((t /long/chain/of/names/ending/in/this) (prefix /long/chain/of/names))
     ->
     ending/in/this)
    (((t /) (prefix .)) -> /)
    (((t .) (prefix /)) -> .)
    (((t /a/b/c) (prefix a/b)) -> /a/b/c)
    (((t a/b/c) (prefix /a/b)) -> a/b/c)
    |}]
;;

let is_suffix = File_path.is_suffix

let%expect_test _ =
  Helpers.test_predicate
    (fun { t; suffix } -> is_suffix t ~suffix)
    ~input:(module Helpers.With_suffix (File_path))
    ~examples:Examples.for_chop_suffix
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
       ((t long/chain/of/names/ending/in/this) (suffix ending/in/this))
       ((t /.) (suffix .))
       ((t /..) (suffix ..))
       ((t /a/b/c) (suffix a/b/c))
       ((t /./file) (suffix file))
       ((t /dir/.) (suffix .))
       ((t /../..) (suffix ..))
       ((t /a/b/c/d) (suffix b/c/d))
       ((t /a/b/c/d) (suffix c/d))
       ((t /a/b/c/d) (suffix d))
       ((t /long/chain/of/names/ending/in/this) (suffix ending/in/this))))
     (failure
      (((t ..) (suffix .))
       ((t b/.) (suffix a/.))
       ((t c/d) (suffix a/b))
       ((t c) (suffix a/b/c))
       ((t b/c) (suffix a/b/c))
       ((t /) (suffix .))
       ((t /..) (suffix .))
       ((t /b/.) (suffix a/.))
       ((t /c/d) (suffix a/b))
       ((t /c) (suffix a/b/c))
       ((t /b/c) (suffix a/b/c)))))
    |}]
;;

let chop_suffix = File_path.chop_suffix

let%expect_test _ =
  Helpers.test
    (fun { t; suffix } -> chop_suffix t ~suffix)
    ~input:(module Helpers.With_suffix (File_path))
    ~output:(module Helpers.Option (File_path))
    ~examples:Examples.for_chop_suffix
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
    (((t /) (suffix .)) -> ())
    (((t /..) (suffix .)) -> ())
    (((t /b/.) (suffix a/.)) -> ())
    (((t /c/d) (suffix a/b)) -> ())
    (((t /c) (suffix a/b/c)) -> ())
    (((t /b/c) (suffix a/b/c)) -> ())
    (((t /.) (suffix .)) -> (/))
    (((t /..) (suffix ..)) -> (/))
    (((t /a/b/c) (suffix a/b/c)) -> (/))
    (((t /./file) (suffix file)) -> (/.))
    (((t /dir/.) (suffix .)) -> (/dir))
    (((t /../..) (suffix ..)) -> (/..))
    (((t /a/b/c/d) (suffix b/c/d)) -> (/a))
    (((t /a/b/c/d) (suffix c/d)) -> (/a/b))
    (((t /a/b/c/d) (suffix d)) -> (/a/b/c))
    (((t /long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     (/long/chain/of/names))
    |}]
;;

let chop_suffix_exn = File_path.chop_suffix_exn

let%expect_test _ =
  Helpers.test
    (fun { t; suffix } -> Or_error.try_with (fun () -> chop_suffix_exn t ~suffix))
    ~input:(module Helpers.With_suffix (File_path))
    ~output:(module Helpers.Or_error (File_path))
    ~examples:Examples.for_chop_suffix
    ~correctness:(fun { t; suffix } chop_suffix_exn ->
      require_equal
        (module Helpers.Option (File_path))
        (chop_suffix t ~suffix)
        (Or_error.ok chop_suffix_exn)
        ~message:"[chop_suffix_exn] and [chop_suffix] are inconsistent");
  [%expect
    {|
    (((t ..) (suffix .))
     ->
     (Error ("File_path.chop_suffix_exn: not a suffix" ((path ..) (suffix .)))))
    (((t b/.) (suffix a/.))
     ->
     (Error ("File_path.chop_suffix_exn: not a suffix" ((path b/.) (suffix a/.)))))
    (((t c/d) (suffix a/b))
     ->
     (Error ("File_path.chop_suffix_exn: not a suffix" ((path c/d) (suffix a/b)))))
    (((t c) (suffix a/b/c))
     ->
     (Error ("File_path.chop_suffix_exn: not a suffix" ((path c) (suffix a/b/c)))))
    (((t b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.chop_suffix_exn: not a suffix" ((path b/c) (suffix a/b/c)))))
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
    (((t /) (suffix .))
     ->
     (Error ("File_path.chop_suffix_exn: not a suffix" ((path /) (suffix .)))))
    (((t /..) (suffix .))
     ->
     (Error ("File_path.chop_suffix_exn: not a suffix" ((path /..) (suffix .)))))
    (((t /b/.) (suffix a/.))
     ->
     (Error
      ("File_path.chop_suffix_exn: not a suffix" ((path /b/.) (suffix a/.)))))
    (((t /c/d) (suffix a/b))
     ->
     (Error
      ("File_path.chop_suffix_exn: not a suffix" ((path /c/d) (suffix a/b)))))
    (((t /c) (suffix a/b/c))
     ->
     (Error
      ("File_path.chop_suffix_exn: not a suffix" ((path /c) (suffix a/b/c)))))
    (((t /b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.chop_suffix_exn: not a suffix" ((path /b/c) (suffix a/b/c)))))
    (((t /.) (suffix .)) -> (Ok /))
    (((t /..) (suffix ..)) -> (Ok /))
    (((t /a/b/c) (suffix a/b/c)) -> (Ok /))
    (((t /./file) (suffix file)) -> (Ok /.))
    (((t /dir/.) (suffix .)) -> (Ok /dir))
    (((t /../..) (suffix ..)) -> (Ok /..))
    (((t /a/b/c/d) (suffix b/c/d)) -> (Ok /a))
    (((t /a/b/c/d) (suffix c/d)) -> (Ok /a/b))
    (((t /a/b/c/d) (suffix d)) -> (Ok /a/b/c))
    (((t /long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     (Ok /long/chain/of/names))
    |}]
;;

let chop_suffix_or_error = File_path.chop_suffix_or_error

let%expect_test _ =
  Helpers.test
    (fun { t; suffix } -> chop_suffix_or_error t ~suffix)
    ~input:(module Helpers.With_suffix (File_path))
    ~output:(module Helpers.Or_error (File_path))
    ~examples:Examples.for_chop_suffix
    ~correctness:(fun { t; suffix } chop_suffix_or_error ->
      require_equal
        (module Helpers.Option (File_path))
        (chop_suffix t ~suffix)
        (Or_error.ok chop_suffix_or_error)
        ~message:"[chop_suffix_or_error] and [chop_suffix] are inconsistent");
  [%expect
    {|
    (((t ..) (suffix .))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path ..) (suffix .)))))
    (((t b/.) (suffix a/.))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path b/.) (suffix a/.)))))
    (((t c/d) (suffix a/b))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path c/d) (suffix a/b)))))
    (((t c) (suffix a/b/c))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path c) (suffix a/b/c)))))
    (((t b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path b/c) (suffix a/b/c)))))
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
    (((t /) (suffix .))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path /) (suffix .)))))
    (((t /..) (suffix .))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path /..) (suffix .)))))
    (((t /b/.) (suffix a/.))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path /b/.) (suffix a/.)))))
    (((t /c/d) (suffix a/b))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path /c/d) (suffix a/b)))))
    (((t /c) (suffix a/b/c))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path /c) (suffix a/b/c)))))
    (((t /b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix"
       ((path /b/c) (suffix a/b/c)))))
    (((t /.) (suffix .)) -> (Ok /))
    (((t /..) (suffix ..)) -> (Ok /))
    (((t /a/b/c) (suffix a/b/c)) -> (Ok /))
    (((t /./file) (suffix file)) -> (Ok /.))
    (((t /dir/.) (suffix .)) -> (Ok /dir))
    (((t /../..) (suffix ..)) -> (Ok /..))
    (((t /a/b/c/d) (suffix b/c/d)) -> (Ok /a))
    (((t /a/b/c/d) (suffix c/d)) -> (Ok /a/b))
    (((t /a/b/c/d) (suffix d)) -> (Ok /a/b/c))
    (((t /long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     (Ok /long/chain/of/names))
    |}]
;;

let chop_suffix_if_exists = File_path.chop_suffix_if_exists

let%expect_test _ =
  Helpers.test
    (fun { t; suffix } -> chop_suffix_if_exists t ~suffix)
    ~input:(module Helpers.With_suffix (File_path))
    ~output:(module File_path)
    ~examples:Examples.for_chop_suffix
    ~correctness:(fun { t; suffix } chop_suffix_if_exists ->
      require_equal
        (module File_path)
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
    (((t /) (suffix .)) -> /)
    (((t /..) (suffix .)) -> /..)
    (((t /b/.) (suffix a/.)) -> /b/.)
    (((t /c/d) (suffix a/b)) -> /c/d)
    (((t /c) (suffix a/b/c)) -> /c)
    (((t /b/c) (suffix a/b/c)) -> /b/c)
    (((t /.) (suffix .)) -> /)
    (((t /..) (suffix ..)) -> /)
    (((t /a/b/c) (suffix a/b/c)) -> /)
    (((t /./file) (suffix file)) -> /.)
    (((t /dir/.) (suffix .)) -> /dir)
    (((t /../..) (suffix ..)) -> /..)
    (((t /a/b/c/d) (suffix b/c/d)) -> /a)
    (((t /a/b/c/d) (suffix c/d)) -> /a/b)
    (((t /a/b/c/d) (suffix d)) -> /a/b/c)
    (((t /long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     /long/chain/of/names)
    |}]
;;

let append = File_path.append

let%expect_test _ =
  Helpers.test
    (fun (prefix, suffix) -> append prefix suffix)
    ~input:(module Helpers.Tuple2 (File_path) (File_path.Relative))
    ~output:(module File_path)
    ~examples:Examples.for_append
    ~correctness:(fun (prefix, suffix) append ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (Some suffix)
        (chop_prefix append ~prefix)
        ~message:"[append] and [chop_prefix] are inconsistent";
      require_equal
        (module Helpers.Option (File_path))
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
    ((/ .) -> /.)
    ((/ ..) -> /..)
    ((/ a/b/c) -> /a/b/c)
    ((/. file) -> /./file)
    ((/dir .) -> /dir/.)
    ((/.. ..) -> /../..)
    ((/a b/c/d) -> /a/b/c/d)
    ((/a/b c/d) -> /a/b/c/d)
    ((/a/b/c d) -> /a/b/c/d)
    ((/long/chain/of/names ending/in/this) -> /long/chain/of/names/ending/in/this)
    |}]
;;

let to_parts = File_path.to_parts

let%expect_test _ =
  Helpers.test
    to_parts
    ~input:(module File_path)
    ~output:(module Helpers.List (File_path.Part))
    ~examples:Examples.for_conversion
    ~correctness:(fun path to_parts ->
      require_equal
        (module Int)
        (List.length to_parts)
        (number_of_parts path)
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
    (/ -> ())
    (/. -> (.))
    (/.. -> (..))
    (/filename.txt -> (filename.txt))
    (/bin -> (bin))
    (/.hidden -> (.hidden))
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     ("This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("/\001\255" -> ("\001\255"))
    (/./. -> (. .))
    (/../.. -> (.. ..))
    (/././. -> (. . .))
    (/bin/exe -> (bin exe))
    (/bin/exe/file -> (bin exe file))
    |}]
;;

let of_parts_absolute = File_path.of_parts_absolute

let%expect_test _ =
  Helpers.test
    of_parts_absolute
    ~input:(module Helpers.List (File_path.Part))
    ~output:(module File_path)
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_absolute ->
      require_equal
        (module Helpers.List (File_path.Part))
        (to_parts of_parts_absolute)
        parts
        ~message:"[of_parts_absolute] and [to_parts] are inconsistent");
  [%expect
    {|
    (() -> /)
    ((.) -> /.)
    ((..) -> /..)
    ((filename.txt) -> /filename.txt)
    ((bin) -> /bin)
    ((.hidden) -> /.hidden)
    (("This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "/This is a sentence; it has punctuation, capitalization, and spaces!")
    (("\001\255") -> "/\001\255")
    ((. .) -> /./.)
    ((.. .) -> /../.)
    ((.. .) -> /../.)
    ((.. ..) -> /../..)
    ((filename.txt .) -> /filename.txt/.)
    ((.. filename.txt) -> /../filename.txt)
    ((bin .) -> /bin/.)
    ((.. bin) -> /../bin)
    ((.hidden .) -> /.hidden/.)
    ((.. .hidden) -> /../.hidden)
    (("This is a sentence; it has punctuation, capitalization, and spaces!" .)
     ->
     "/This is a sentence; it has punctuation, capitalization, and spaces!/.")
    ((.. "This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "/../This is a sentence; it has punctuation, capitalization, and spaces!")
    (("\001\255" .) -> "/\001\255/.")
    ((.. "\001\255") -> "/../\001\255")
    ((.hidden bin.exe) -> /.hidden/bin.exe)
    ((.hidden bin exe.file) -> /.hidden/bin/exe.file)
    |}]
;;

let of_parts_relative = File_path.of_parts_relative

let%expect_test _ =
  Helpers.test
    of_parts_relative
    ~input:(module Helpers.List (File_path.Part))
    ~output:(module Helpers.Option (File_path))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_relative ->
      require_equal
        (module Helpers.List (File_path.Part))
        (Option.value_map of_parts_relative ~f:to_parts ~default:[])
        parts
        ~message:"[of_parts_relative] and [to_parts] are inconsistent");
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

let of_parts_relative_exn = File_path.of_parts_relative_exn

let%expect_test _ =
  Helpers.test
    (fun parts -> Or_error.try_with (fun () -> of_parts_relative_exn parts))
    ~input:(module Helpers.List (File_path.Part))
    ~output:(module Helpers.Or_error (File_path))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_relative_exn ->
      require_equal
        (module Helpers.Option (File_path))
        (of_parts_relative parts)
        (Or_error.ok of_parts_relative_exn)
        ~message:"[of_parts_relative_exn] and [of_parts_relative] are inconsistent");
  [%expect
    {|
    (() -> (Error "File_path.of_parts_relative_exn: empty list"))
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

let of_parts_relative_or_error = File_path.of_parts_relative_or_error

let%expect_test _ =
  Helpers.test
    of_parts_relative_or_error
    ~input:(module Helpers.List (File_path.Part))
    ~output:(module Helpers.Or_error (File_path))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_relative_or_error ->
      require_equal
        (module Helpers.Option (File_path))
        (of_parts_relative parts)
        (Or_error.ok of_parts_relative_or_error)
        ~message:"[of_parts_relative_or_error] and [of_parts_relative] are inconsistent");
  [%expect
    {|
    (() -> (Error "File_path.of_parts_relative_or_error: empty list"))
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

let of_parts_relative_defaulting_to_dot = File_path.of_parts_relative_defaulting_to_dot

let%expect_test _ =
  Helpers.test
    of_parts_relative_defaulting_to_dot
    ~input:(module Helpers.List (File_path.Part))
    ~output:(module File_path)
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_relative_defaulting_to_dot ->
      require_equal
        (module File_path)
        (Option.value (of_parts_relative parts) ~default:dot)
        of_parts_relative_defaulting_to_dot
        ~message:
          "[of_parts_relative_defaulting_to_dot] and [of_parts_relative] are inconsistent");
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

let of_parts_relative_nonempty = File_path.of_parts_relative_nonempty

let%expect_test _ =
  Helpers.test
    of_parts_relative_nonempty
    ~input:(module Helpers.Nonempty_list (File_path.Part))
    ~output:(module File_path)
    ~examples:
      (List.filter_map Examples.Part.lists_for_conversion ~f:Nonempty_list.of_list)
    ~correctness:(fun parts of_parts_relative_nonempty ->
      require_equal
        (module Helpers.Option (File_path))
        (of_parts_relative (Nonempty_list.to_list parts))
        (Some of_parts_relative_nonempty)
        ~message:"[of_parts_relative_nonempty] and [of_parts_relative] are inconsistent");
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

let make_absolute = File_path.make_absolute

let%expect_test _ =
  Helpers.test
    (fun (path, under) -> make_absolute path ~under)
    ~input:(module Helpers.Tuple2 (File_path) (File_path.Absolute))
    ~output:(module File_path.Absolute)
    ~examples:Examples.for_make_absolute
    ~correctness:(fun (path, under) make_absolute ->
      if is_absolute path
      then
        require_equal
          (module File_path.Absolute)
          (to_absolute_exn path)
          make_absolute
          ~message:"[make_absolute] and [to_absolute_exn] are inconsistent"
      else
        require_equal
          (module Helpers.Option (File_path.Relative))
          (Some (to_relative_exn path))
          (File_path.Absolute.chop_prefix make_absolute ~prefix:under)
          ~message:"[make_absolute] and [chop_prefix] are inconsistent");
  [%expect
    {|
    ((. /) -> /.)
    ((.. /) -> /..)
    ((a/b/c /) -> /a/b/c)
    ((file /.) -> /./file)
    ((. /dir) -> /dir/.)
    ((.. /..) -> /../..)
    ((b/c/d /a) -> /a/b/c/d)
    ((c/d /a/b) -> /a/b/c/d)
    ((d /a/b/c) -> /a/b/c/d)
    ((ending/in/this /long/chain/of/names) -> /long/chain/of/names/ending/in/this)
    ((/ /.) -> /)
    ((/. /) -> /.)
    ((/.. /) -> /..)
    ((/a/b/c /) -> /a/b/c)
    ((/file /.) -> /file)
    ((/. /dir) -> /.)
    ((/.. /..) -> /..)
    ((/b/c/d /a) -> /b/c/d)
    ((/c/d /a/b) -> /c/d)
    ((/d /a/b/c) -> /d)
    ((/ending/in/this /long/chain/of/names) -> /ending/in/this)
    |}]
;;

let make_relative = File_path.make_relative

let%expect_test _ =
  Helpers.test
    (fun (path, if_under) -> make_relative path ~if_under)
    ~input:(module Helpers.Tuple2 (File_path) (File_path.Absolute))
    ~output:(module Helpers.Option (File_path.Relative))
    ~examples:Examples.for_make_relative
    ~correctness:(fun (path, if_under) make_relative ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        make_relative
        (if is_relative path
         then Some (to_relative_exn path)
         else chop_prefix path ~prefix:(of_absolute if_under))
        ~message:"[make_relative] and [chop_prefix] are inconsistent");
  [%expect
    {|
    ((. /) -> (.))
    ((.. /) -> (..))
    ((a/b/c /) -> (a/b/c))
    ((file /.) -> (file))
    ((. /dir) -> (.))
    ((.. /..) -> (..))
    ((b/c/d /a) -> (b/c/d))
    ((c/d /a/b) -> (c/d))
    ((d /a/b/c) -> (d))
    ((ending/in/this /long/chain/of/names) -> (ending/in/this))
    ((/ /) -> (.))
    ((/.. /.) -> ())
    ((/./b /./a) -> ())
    ((/c/d /a/b) -> ())
    ((/a /a/b/c) -> ())
    ((/a/b /a/b/c) -> ())
    ((/. /.) -> (.))
    ((/.. /..) -> (.))
    ((/a/b/c /a/b/c) -> (.))
    ((/./file /.) -> (file))
    ((/dir/. /dir) -> (.))
    ((/../.. /..) -> (..))
    ((/a/b/c/d /a) -> (b/c/d))
    ((/a/b/c/d /a/b) -> (c/d))
    ((/a/b/c/d /a/b/c) -> (d))
    ((/long/chain/of/names/ending/in/this /long/chain/of/names)
     ->
     (ending/in/this))
    |}]
;;

let make_relative_exn = File_path.make_relative_exn

let%expect_test _ =
  Helpers.test
    (fun (path, if_under) ->
      Or_error.try_with (fun () -> make_relative_exn path ~if_under))
    ~input:(module Helpers.Tuple2 (File_path) (File_path.Absolute))
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.for_make_relative
    ~correctness:(fun (path, if_under) make_relative_exn ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (Or_error.ok make_relative_exn)
        (make_relative path ~if_under)
        ~message:"[make_relative_exn] and [make_relative] are inconsistent");
  [%expect
    {|
    ((. /) -> (Ok .))
    ((.. /) -> (Ok ..))
    ((a/b/c /) -> (Ok a/b/c))
    ((file /.) -> (Ok file))
    ((. /dir) -> (Ok .))
    ((.. /..) -> (Ok ..))
    ((b/c/d /a) -> (Ok b/c/d))
    ((c/d /a/b) -> (Ok c/d))
    ((d /a/b/c) -> (Ok d))
    ((ending/in/this /long/chain/of/names) -> (Ok ending/in/this))
    ((/ /) -> (Ok .))
    ((/.. /.)
     ->
     (Error
      ("File_path.make_relative_exn: cannot make path relative"
       ((path /..) (if_under /.)))))
    ((/./b /./a)
     ->
     (Error
      ("File_path.make_relative_exn: cannot make path relative"
       ((path /./b) (if_under /./a)))))
    ((/c/d /a/b)
     ->
     (Error
      ("File_path.make_relative_exn: cannot make path relative"
       ((path /c/d) (if_under /a/b)))))
    ((/a /a/b/c)
     ->
     (Error
      ("File_path.make_relative_exn: cannot make path relative"
       ((path /a) (if_under /a/b/c)))))
    ((/a/b /a/b/c)
     ->
     (Error
      ("File_path.make_relative_exn: cannot make path relative"
       ((path /a/b) (if_under /a/b/c)))))
    ((/. /.) -> (Ok .))
    ((/.. /..) -> (Ok .))
    ((/a/b/c /a/b/c) -> (Ok .))
    ((/./file /.) -> (Ok file))
    ((/dir/. /dir) -> (Ok .))
    ((/../.. /..) -> (Ok ..))
    ((/a/b/c/d /a) -> (Ok b/c/d))
    ((/a/b/c/d /a/b) -> (Ok c/d))
    ((/a/b/c/d /a/b/c) -> (Ok d))
    ((/long/chain/of/names/ending/in/this /long/chain/of/names)
     ->
     (Ok ending/in/this))
    |}]
;;

let make_relative_or_error = File_path.make_relative_or_error

let%expect_test _ =
  Helpers.test
    (fun (path, if_under) -> make_relative_or_error path ~if_under)
    ~input:(module Helpers.Tuple2 (File_path) (File_path.Absolute))
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.for_make_relative
    ~correctness:(fun (path, if_under) make_relative_or_error ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (Or_error.ok make_relative_or_error)
        (make_relative path ~if_under)
        ~message:"[make_relative_or_error] and [make_relative] are inconsistent");
  [%expect
    {|
    ((. /) -> (Ok .))
    ((.. /) -> (Ok ..))
    ((a/b/c /) -> (Ok a/b/c))
    ((file /.) -> (Ok file))
    ((. /dir) -> (Ok .))
    ((.. /..) -> (Ok ..))
    ((b/c/d /a) -> (Ok b/c/d))
    ((c/d /a/b) -> (Ok c/d))
    ((d /a/b/c) -> (Ok d))
    ((ending/in/this /long/chain/of/names) -> (Ok ending/in/this))
    ((/ /) -> (Ok .))
    ((/.. /.)
     ->
     (Error
      ("File_path.make_relative_or_error: cannot make path relative"
       ((path /..) (if_under /.)))))
    ((/./b /./a)
     ->
     (Error
      ("File_path.make_relative_or_error: cannot make path relative"
       ((path /./b) (if_under /./a)))))
    ((/c/d /a/b)
     ->
     (Error
      ("File_path.make_relative_or_error: cannot make path relative"
       ((path /c/d) (if_under /a/b)))))
    ((/a /a/b/c)
     ->
     (Error
      ("File_path.make_relative_or_error: cannot make path relative"
       ((path /a) (if_under /a/b/c)))))
    ((/a/b /a/b/c)
     ->
     (Error
      ("File_path.make_relative_or_error: cannot make path relative"
       ((path /a/b) (if_under /a/b/c)))))
    ((/. /.) -> (Ok .))
    ((/.. /..) -> (Ok .))
    ((/a/b/c /a/b/c) -> (Ok .))
    ((/./file /.) -> (Ok file))
    ((/dir/. /dir) -> (Ok .))
    ((/../.. /..) -> (Ok ..))
    ((/a/b/c/d /a) -> (Ok b/c/d))
    ((/a/b/c/d /a/b) -> (Ok c/d))
    ((/a/b/c/d /a/b/c) -> (Ok d))
    ((/long/chain/of/names/ending/in/this /long/chain/of/names)
     ->
     (Ok ending/in/this))
    |}]
;;

let make_relative_if_possible = File_path.make_relative_if_possible

let%expect_test _ =
  Helpers.test
    (fun (path, if_under) -> make_relative_if_possible path ~if_under)
    ~input:(module Helpers.Tuple2 (File_path) (File_path.Absolute))
    ~output:(module File_path)
    ~examples:Examples.for_make_relative
    ~correctness:(fun (path, if_under) make_relative_if_possible ->
      require_equal
        (module File_path)
        make_relative_if_possible
        (Option.value_map (make_relative path ~if_under) ~f:of_relative ~default:path)
        ~message:"[make_relative_if_possible] and [make_relative] are inconsistent");
  [%expect
    {|
    ((. /) -> .)
    ((.. /) -> ..)
    ((a/b/c /) -> a/b/c)
    ((file /.) -> file)
    ((. /dir) -> .)
    ((.. /..) -> ..)
    ((b/c/d /a) -> b/c/d)
    ((c/d /a/b) -> c/d)
    ((d /a/b/c) -> d)
    ((ending/in/this /long/chain/of/names) -> ending/in/this)
    ((/ /) -> .)
    ((/.. /.) -> /..)
    ((/./b /./a) -> /./b)
    ((/c/d /a/b) -> /c/d)
    ((/a /a/b/c) -> /a)
    ((/a/b /a/b/c) -> /a/b)
    ((/. /.) -> .)
    ((/.. /..) -> .)
    ((/a/b/c /a/b/c) -> .)
    ((/./file /.) -> file)
    ((/dir/. /dir) -> .)
    ((/../.. /..) -> ..)
    ((/a/b/c/d /a) -> b/c/d)
    ((/a/b/c/d /a/b) -> c/d)
    ((/a/b/c/d /a/b/c) -> d)
    ((/long/chain/of/names/ending/in/this /long/chain/of/names) -> ending/in/this)
    |}]
;;

module Variant = File_path.Variant

let to_variant = File_path.to_variant

let%expect_test _ =
  Helpers.test
    to_variant
    ~input:(module File_path)
    ~output:(module File_path.Variant)
    ~examples:Examples.for_conversion
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    (. -> (Relative .))
    (.. -> (Relative ..))
    (filename.txt -> (Relative filename.txt))
    (bin -> (Relative bin))
    (.hidden -> (Relative .hidden))
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Relative
      "This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("\001\255" -> (Relative "\001\255"))
    (./. -> (Relative ./.))
    (../.. -> (Relative ../..))
    (././. -> (Relative ././.))
    (bin/exe -> (Relative bin/exe))
    (bin/exe/file -> (Relative bin/exe/file))
    (/ -> (Absolute /))
    (/. -> (Absolute /.))
    (/.. -> (Absolute /..))
    (/filename.txt -> (Absolute /filename.txt))
    (/bin -> (Absolute /bin))
    (/.hidden -> (Absolute /.hidden))
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Absolute
      "/This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("/\001\255" -> (Absolute "/\001\255"))
    (/./. -> (Absolute /./.))
    (/../.. -> (Absolute /../..))
    (/././. -> (Absolute /././.))
    (/bin/exe -> (Absolute /bin/exe))
    (/bin/exe/file -> (Absolute /bin/exe/file))
    |}]
;;

let of_variant = File_path.of_variant

let%expect_test _ =
  Helpers.test
    of_variant
    ~input:(module Variant)
    ~output:(module File_path)
    ~examples:Examples.variant_for_conversion
    ~correctness:(fun variant of_variant ->
      require_equal
        (module Variant)
        variant
        (to_variant of_variant)
        ~message:"[of_variant] and [to_variant] are inconsistent");
  [%expect
    {|
    ((Relative .) -> .)
    ((Relative ..) -> ..)
    ((Relative filename.txt) -> filename.txt)
    ((Relative bin) -> bin)
    ((Relative .hidden) -> .hidden)
    ((Relative
      "This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!")
    ((Relative "\001\255") -> "\001\255")
    ((Relative ./.) -> ./.)
    ((Relative ../..) -> ../..)
    ((Relative ././.) -> ././.)
    ((Relative bin/exe) -> bin/exe)
    ((Relative bin/exe/file) -> bin/exe/file)
    ((Absolute /) -> /)
    ((Absolute /.) -> /.)
    ((Absolute /..) -> /..)
    ((Absolute /filename.txt) -> /filename.txt)
    ((Absolute /bin) -> /bin)
    ((Absolute /.hidden) -> /.hidden)
    ((Absolute
      "/This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "/This is a sentence; it has punctuation, capitalization, and spaces!")
    ((Absolute "/\001\255") -> "/\001\255")
    ((Absolute /./.) -> /./.)
    ((Absolute /../..) -> /../..)
    ((Absolute /././.) -> /././.)
    ((Absolute /bin/exe) -> /bin/exe)
    ((Absolute /bin/exe/file) -> /bin/exe/file)
    |}]
;;

let simplify_dot = File_path.simplify_dot

let%expect_test _ =
  Helpers.test
    simplify_dot
    ~input:(module File_path)
    ~output:(module File_path)
    ~examples:Examples.for_simplify
    ~correctness:(fun original simplified ->
      require_equal
        (module File_path)
        simplified
        (let parts =
           to_parts original |> List.filter ~f:(File_path.Part.( <> ) File_path.Part.dot)
         in
         if is_absolute original
         then of_parts_absolute parts
         else of_parts_relative_defaulting_to_dot parts)
        ~message:"[simplify_dot] is not equivalent to filtering out [.] parts";
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
    (/ -> /)
    (/. -> /)
    (/.. -> /..)
    (/filename.txt -> /filename.txt)
    (/bin -> /bin)
    (/.hidden -> /.hidden)
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     "/This is a sentence; it has punctuation, capitalization, and spaces!")
    ("/\001\255" -> "/\001\255")
    (/a/b -> /a/b)
    (/a/b/. -> /a/b)
    (/a/./b -> /a/b)
    (/./a/b -> /a/b)
    (/./a/./b/. -> /a/b)
    (/a/b/./. -> /a/b)
    (/a/././b -> /a/b)
    (/././a/b -> /a/b)
    (/././a/././b/./. -> /a/b)
    (/a/b/.. -> /a/b/..)
    (/a/../b -> /a/../b)
    (/../a/b -> /../a/b)
    (/../a/../b/.. -> /../a/../b/..)
    (/a/b/../.. -> /a/b/../..)
    (/a/../../b -> /a/../../b)
    (/../../a/b -> /../../a/b)
    (/../../a/../../b/../.. -> /../../a/../../b/../..)
    (/a/b/./.. -> /a/b/..)
    (/a/./../b -> /a/../b)
    (/./../a/b -> /../a/b)
    (/./../a/./../b/./.. -> /../a/../b/..)
    (/a/b/../. -> /a/b/..)
    (/a/.././b -> /a/../b)
    (/.././a/b -> /../a/b)
    (/.././a/.././b/../. -> /../a/../b/..)
    |}]
;;

let simplify_dot_and_dot_dot_naively = File_path.simplify_dot_and_dot_dot_naively

let%expect_test _ =
  Helpers.test
    simplify_dot_and_dot_dot_naively
    ~input:(module File_path)
    ~output:(module File_path)
    ~examples:Examples.for_simplify
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
        (module File_path)
        simplified
        (simplify_dot_and_dot_dot_naively (simplify_dot original))
        ~message:"[simplify_dot_and_dot_dot_naively] does not ignore [.] parts";
      require_equal
        (module File_path)
        simplified
        (simplify_dot simplified)
        ~message:"[simplify_dot_and_dot_dot_naively] does not simplify all [.] parts";
      require_equal
        (module File_path)
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
    (/ -> /)
    (/. -> /)
    (/.. -> /)
    (/filename.txt -> /filename.txt)
    (/bin -> /bin)
    (/.hidden -> /.hidden)
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     "/This is a sentence; it has punctuation, capitalization, and spaces!")
    ("/\001\255" -> "/\001\255")
    (/a/b -> /a/b)
    (/a/b/. -> /a/b)
    (/a/./b -> /a/b)
    (/./a/b -> /a/b)
    (/./a/./b/. -> /a/b)
    (/a/b/./. -> /a/b)
    (/a/././b -> /a/b)
    (/././a/b -> /a/b)
    (/././a/././b/./. -> /a/b)
    (/a/b/.. -> /a)
    (/a/../b -> /b)
    (/../a/b -> /a/b)
    (/../a/../b/.. -> /)
    (/a/b/../.. -> /)
    (/a/../../b -> /b)
    (/../../a/b -> /a/b)
    (/../../a/../../b/../.. -> /)
    (/a/b/./.. -> /a)
    (/a/./../b -> /b)
    (/./../a/b -> /a/b)
    (/./../a/./../b/./.. -> /)
    (/a/b/../. -> /a)
    (/a/.././b -> /b)
    (/.././a/b -> /a/b)
    (/.././a/.././b/../. -> /)
    |}]
;;

(* Test command-line autocompletion separately. *)
include Test_path_completion
