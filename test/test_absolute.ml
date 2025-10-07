(* See comment in [test_path.ml]. *)

open! Core
open Expect_test_helpers_core

type t = File_path.Absolute.t
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp, sexp_grammar]

let root = File_path.Absolute.root

let%expect_test _ =
  Helpers.test_constants (module File_path.Absolute) [ root ];
  [%expect {| / |}]
;;

include (
  File_path.Absolute :
  sig
  @@ portable
    include
      Identifiable.S
      with type t := t
       and type comparator_witness = File_path.Absolute.comparator_witness
  end)

let%expect_test _ =
  Helpers.test_compare (module File_path.Absolute) Examples.Absolute.for_compare;
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
     "/\255\001")
    |}]
;;

let%expect_test _ =
  Helpers.test_of_string
    (module File_path.Absolute)
    Examples.Absolute.strings_for_of_string;
  [%expect
    {|
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
    (~ // /)
    (~ //. /.)
    (~ /./ /.)
    (~ /.//. /./.)
    (~ /.//.// /./.)
    (~ /bin/exe/ /bin/exe)
    (~ /bin//exe//file /bin/exe/file)
    (~ /bin//exe//file/ /bin/exe/file)
    (! ("File_path.Absolute.of_string: invalid string" ""))
    (! ("File_path.Absolute.of_string: invalid string" "/invalid/\000/null"))
    (! ("File_path.Absolute.of_string: invalid string" invalid/relative))
    |}]
;;

let%expect_test _ =
  Helpers.test_containers (module File_path.Absolute) Examples.Absolute.for_conversion;
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
      /filename.txt))
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
      (/filename.txt 12)))
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
      /filename.txt))
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
      (/filename.txt 12)))
    |}]
;;

module Expert = struct
  let unchecked_of_canonical_string =
    File_path.Absolute.Expert.unchecked_of_canonical_string
  ;;
end

let invariant = File_path.Absolute.invariant

let%expect_test _ =
  Helpers.test_invariant
    (module File_path.Absolute)
    Examples.Absolute.strings_for_of_string;
  [%expect
    {|
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
    (! ("File_path.Absolute.invariant: non-canonical representation" //))
    (! ("File_path.Absolute.invariant: non-canonical representation" //.))
    (! ("File_path.Absolute.invariant: non-canonical representation" /./))
    (! ("File_path.Absolute.invariant: non-canonical representation" /.//.))
    (! ("File_path.Absolute.invariant: non-canonical representation" /.//.//))
    (! ("File_path.Absolute.invariant: non-canonical representation" /bin/exe/))
    (!
     ("File_path.Absolute.invariant: non-canonical representation"
      /bin//exe//file))
    (!
     ("File_path.Absolute.invariant: non-canonical representation"
      /bin//exe//file/))
    (! ("File_path.Absolute.invariant: invalid string" ""))
    (! ("File_path.Absolute.invariant: invalid string" "/invalid/\000/null"))
    (! ("File_path.Absolute.invariant: invalid string" invalid/relative))
    |}]
;;

let basename = File_path.Absolute.basename

let%expect_test _ =
  Helpers.test
    basename
    ~input:(module File_path.Absolute)
    ~output:(module Helpers.Option (File_path.Part))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute basename ->
      require_equal
        (module Bool)
        (Option.is_none basename)
        (equal absolute root)
        ~message:"[basename] is inconsistent with [equal root]");
  [%expect
    {|
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

let basename_exn = File_path.Absolute.basename_exn

let%expect_test _ =
  Helpers.test
    (fun absolute -> Or_error.try_with (fun () -> basename_exn absolute))
    ~input:(module File_path.Absolute)
    ~output:(module Helpers.Or_error (File_path.Part))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute basename_exn ->
      require_equal
        (module Helpers.Option (File_path.Part))
        (Or_error.ok basename_exn)
        (basename absolute)
        ~message:"[basename_exn] and [basename] are inconsistent");
  [%expect
    {|
    (/ -> (Error "File_path.Absolute.basename_exn: root path"))
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

let basename_or_error = File_path.Absolute.basename_or_error

let%expect_test _ =
  Helpers.test
    basename_or_error
    ~input:(module File_path.Absolute)
    ~output:(module Helpers.Or_error (File_path.Part))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute basename_or_error ->
      require_equal
        (module Helpers.Option (File_path.Part))
        (Or_error.ok basename_or_error)
        (basename absolute)
        ~message:"[basename_or_error] and [basename] are inconsistent");
  [%expect
    {|
    (/ -> (Error "File_path.Absolute.basename_or_error: root path"))
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

let basename_defaulting_to_dot = File_path.Absolute.basename_defaulting_to_dot

let%expect_test _ =
  Helpers.test
    basename_defaulting_to_dot
    ~input:(module File_path.Absolute)
    ~output:(module File_path.Part)
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute basename_defaulting_to_dot ->
      require_equal
        (module File_path.Part)
        basename_defaulting_to_dot
        (Option.value (basename absolute) ~default:File_path.Part.dot)
        ~message:"[basename_defaulting_to_dot] and [basename] are inconsistent");
  [%expect
    {|
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

let dirname = File_path.Absolute.dirname

let%expect_test _ =
  Helpers.test
    dirname
    ~input:(module File_path.Absolute)
    ~output:(module Helpers.Option (File_path.Absolute))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute dirname ->
      require_equal
        (module Bool)
        (Option.is_none dirname)
        (Option.is_none (basename absolute))
        ~message:"[dirname] and [basename] are inconsistent");
  [%expect
    {|
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

let dirname_exn = File_path.Absolute.dirname_exn

let%expect_test _ =
  Helpers.test
    (fun absolute -> Or_error.try_with (fun () -> dirname_exn absolute))
    ~input:(module File_path.Absolute)
    ~output:(module Helpers.Or_error (File_path.Absolute))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute dirname_exn ->
      require_equal
        (module Helpers.Option (File_path.Absolute))
        (Or_error.ok dirname_exn)
        (dirname absolute)
        ~message:"[dirname_exn] and [dirname] are inconsistent");
  [%expect
    {|
    (/ -> (Error "File_path.Absolute.dirname_exn: root path"))
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

let dirname_or_error = File_path.Absolute.dirname_or_error

let%expect_test _ =
  Helpers.test
    dirname_or_error
    ~input:(module File_path.Absolute)
    ~output:(module Helpers.Or_error (File_path.Absolute))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute dirname_or_error ->
      require_equal
        (module Helpers.Option (File_path.Absolute))
        (Or_error.ok dirname_or_error)
        (dirname absolute)
        ~message:"[dirname_or_error] and [dirname] are inconsistent");
  [%expect
    {|
    (/ -> (Error "File_path.Absolute.dirname_or_error: root path"))
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

let dirname_defaulting_to_root = File_path.Absolute.dirname_defaulting_to_root

let%expect_test _ =
  Helpers.test
    dirname_defaulting_to_root
    ~input:(module File_path.Absolute)
    ~output:(module File_path.Absolute)
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute dirname_defaulting_to_root ->
      require_equal
        (module File_path.Absolute)
        dirname_defaulting_to_root
        (Option.value (dirname absolute) ~default:root)
        ~message:"[dirname_defaulting_to_root] and [dirname] are inconsistent");
  [%expect
    {|
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

let dirname_and_basename = File_path.Absolute.dirname_and_basename

let%expect_test _ =
  Helpers.test
    (fun path -> dirname_and_basename path)
    ~input:(module File_path.Absolute)
    ~output:(module Helpers.Option (Helpers.Tuple2 (File_path.Absolute) (File_path.Part)))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun path dirname_and_basename ->
      require_equal
        (module Helpers.Option (Helpers.Tuple2 (File_path.Absolute) (File_path.Part)))
        dirname_and_basename
        (Option.both (dirname path) (basename path))
        ~message:"[dirname_and_basename] and [dirname]/[basename] are inconsistent");
  [%expect
    {|
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

let append_to_basename_exn = File_path.Absolute.append_to_basename_exn

let%expect_test _ =
  Helpers.test
    (fun (path, string) ->
      Or_error.try_with (fun () -> append_to_basename_exn path string))
    ~input:(module Helpers.Tuple2 (File_path.Absolute) (String))
    ~output:(module Helpers.Or_error (File_path.Absolute))
    ~examples:Examples.Absolute.for_append_to_basename
    ~correctness:(fun (path, string) append_to_basename_exn ->
      require_equal
        (module Helpers.Option (File_path.Absolute))
        (Or_error.ok append_to_basename_exn)
        (if equal path root || String.mem string '/' || String.mem string '\000'
         then None
         else Some (of_string (to_string path ^ string))));
  [%expect
    {|
    ((/ "")
     ->
     (Error
      ("File_path.Absolute.append_to_basename_exn: root path has no basename"
       ((path /) (suffix "")))))
    ((/ x)
     ->
     (Error
      ("File_path.Absolute.append_to_basename_exn: root path has no basename"
       ((path /) (suffix x)))))
    ((/ invalid/slash)
     ->
     (Error
      ("File_path.Absolute.append_to_basename_exn: root path has no basename"
       ((path /) (suffix invalid/slash)))))
    ((/ "invalid\000null")
     ->
     (Error
      ("File_path.Absolute.append_to_basename_exn: root path has no basename"
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
      ("File_path.Absolute.append_to_basename_exn: suffix contains invalid characters"
       ((path /a/b/c) (suffix invalid/slash)))))
    ((/a/b/c "invalid\000null")
     ->
     (Error
      ("File_path.Absolute.append_to_basename_exn: suffix contains invalid characters"
       ((path /a/b/c) (suffix "invalid\000null")))))
    ((/long/chain/of/names/ending/in -this)
     ->
     (Ok /long/chain/of/names/ending/in-this))
    |}]
;;

let append_part = File_path.Absolute.append_part

let%expect_test _ =
  Helpers.test
    (fun (absolute, suffix) -> append_part absolute suffix)
    ~input:(module Helpers.Tuple2 (File_path.Absolute) (File_path.Part))
    ~output:(module File_path.Absolute)
    ~examples:Examples.Absolute.for_append_part
    ~correctness:(fun (absolute, part) append_part ->
      require_equal
        (module Helpers.Option (File_path.Absolute))
        (dirname append_part)
        (Some absolute)
        ~message:"[append_part] and [dirname] are inconsistent";
      require_equal
        (module Helpers.Option (File_path.Part))
        (basename append_part)
        (Some part)
        ~message:"[append_part] and [dirname] are inconsistent");
  [%expect
    {|
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

let is_prefix = File_path.Absolute.is_prefix

let%expect_test _ =
  Helpers.test_predicate
    (fun { t; prefix } -> is_prefix t ~prefix)
    ~input:(module Helpers.With_prefix (File_path.Absolute))
    ~examples:Examples.Absolute.for_chop_prefix
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    ((success
      (((t /) (prefix /))
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
      (((t /..) (prefix /.))
       ((t /./b) (prefix /./a))
       ((t /c/d) (prefix /a/b))
       ((t /a) (prefix /a/b/c))
       ((t /a/b) (prefix /a/b/c)))))
    |}]
;;

let chop_prefix = File_path.Absolute.chop_prefix

let%expect_test _ =
  Helpers.test
    (fun { t; prefix } -> chop_prefix t ~prefix)
    ~input:(module Helpers.With_prefix (File_path.Absolute))
    ~output:(module Helpers.Option (File_path.Relative))
    ~examples:Examples.Absolute.for_chop_prefix
    ~correctness:(fun { t; prefix } chop_prefix ->
      require_equal
        (module Bool)
        (is_prefix t ~prefix)
        (Option.is_some chop_prefix)
        ~message:"[chop_prefix] and [is_prefix] are inconsistent");
  [%expect
    {|
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
    |}]
;;

let chop_prefix_exn = File_path.Absolute.chop_prefix_exn

let%expect_test _ =
  Helpers.test
    (fun { t; prefix } -> Or_error.try_with (fun () -> chop_prefix_exn t ~prefix))
    ~input:(module Helpers.With_prefix (File_path.Absolute))
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.Absolute.for_chop_prefix
    ~correctness:(fun { t; prefix } chop_prefix_exn ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (chop_prefix t ~prefix)
        (Or_error.ok chop_prefix_exn)
        ~message:"[chop_prefix_exn] and [chop_prefix] are inconsistent");
  [%expect
    {|
    (((t /) (prefix /)) -> (Ok .))
    (((t /..) (prefix /.))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_exn: not a prefix"
       ((path /..) (prefix /.)))))
    (((t /./b) (prefix /./a))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_exn: not a prefix"
       ((path /./b) (prefix /./a)))))
    (((t /c/d) (prefix /a/b))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_exn: not a prefix"
       ((path /c/d) (prefix /a/b)))))
    (((t /a) (prefix /a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_exn: not a prefix"
       ((path /a) (prefix /a/b/c)))))
    (((t /a/b) (prefix /a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_exn: not a prefix"
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
    |}]
;;

let chop_prefix_or_error = File_path.Absolute.chop_prefix_or_error

let%expect_test _ =
  Helpers.test
    (fun { t; prefix } -> chop_prefix_or_error t ~prefix)
    ~input:(module Helpers.With_prefix (File_path.Absolute))
    ~output:(module Helpers.Or_error (File_path.Relative))
    ~examples:Examples.Absolute.for_chop_prefix
    ~correctness:(fun { t; prefix } chop_prefix_or_error ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (chop_prefix t ~prefix)
        (Or_error.ok chop_prefix_or_error)
        ~message:"[chop_prefix_or_error] and [chop_prefix] are inconsistent");
  [%expect
    {|
    (((t /) (prefix /)) -> (Ok .))
    (((t /..) (prefix /.))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_or_error: not a prefix"
       ((path /..) (prefix /.)))))
    (((t /./b) (prefix /./a))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_or_error: not a prefix"
       ((path /./b) (prefix /./a)))))
    (((t /c/d) (prefix /a/b))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_or_error: not a prefix"
       ((path /c/d) (prefix /a/b)))))
    (((t /a) (prefix /a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_or_error: not a prefix"
       ((path /a) (prefix /a/b/c)))))
    (((t /a/b) (prefix /a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_or_error: not a prefix"
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
    |}]
;;

let is_suffix = File_path.Absolute.is_suffix

let%expect_test _ =
  Helpers.test_predicate
    (fun { t; suffix } -> is_suffix t ~suffix)
    ~input:(module Helpers.With_suffix (File_path.Absolute))
    ~examples:Examples.Absolute.for_chop_suffix
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    ((success
      (((t /.) (suffix .))
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
      (((t /) (suffix .))
       ((t /..) (suffix .))
       ((t /b/.) (suffix a/.))
       ((t /c/d) (suffix a/b))
       ((t /c) (suffix a/b/c))
       ((t /b/c) (suffix a/b/c)))))
    |}]
;;

let chop_suffix = File_path.Absolute.chop_suffix

let%expect_test _ =
  Helpers.test
    (fun { t; suffix } -> chop_suffix t ~suffix)
    ~input:(module Helpers.With_suffix (File_path.Absolute))
    ~output:(module Helpers.Option (File_path.Absolute))
    ~examples:Examples.Absolute.for_chop_suffix
    ~correctness:(fun { t; suffix } chop_suffix ->
      require_equal
        (module Bool)
        (is_suffix t ~suffix)
        (Option.is_some chop_suffix)
        ~message:"[chop_suffix] and [is_suffix] are inconsistent");
  [%expect
    {|
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

let chop_suffix_exn = File_path.Absolute.chop_suffix_exn

let%expect_test _ =
  Helpers.test
    (fun { t; suffix } -> Or_error.try_with (fun () -> chop_suffix_exn t ~suffix))
    ~input:(module Helpers.With_suffix (File_path.Absolute))
    ~output:(module Helpers.Or_error (File_path.Absolute))
    ~examples:Examples.Absolute.for_chop_suffix
    ~correctness:(fun { t; suffix } chop_suffix_exn ->
      require_equal
        (module Helpers.Option (File_path.Absolute))
        (chop_suffix t ~suffix)
        (Or_error.ok chop_suffix_exn)
        ~message:"[chop_suffix_exn] and [chop_suffix] are inconsistent");
  [%expect
    {|
    (((t /) (suffix .))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_exn: not a suffix" ((path /) (suffix .)))))
    (((t /..) (suffix .))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_exn: not a suffix" ((path /..) (suffix .)))))
    (((t /b/.) (suffix a/.))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_exn: not a suffix"
       ((path /b/.) (suffix a/.)))))
    (((t /c/d) (suffix a/b))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_exn: not a suffix"
       ((path /c/d) (suffix a/b)))))
    (((t /c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_exn: not a suffix"
       ((path /c) (suffix a/b/c)))))
    (((t /b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_exn: not a suffix"
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

let chop_suffix_or_error = File_path.Absolute.chop_suffix_or_error

let%expect_test _ =
  Helpers.test
    (fun { t; suffix } -> chop_suffix_or_error t ~suffix)
    ~input:(module Helpers.With_suffix (File_path.Absolute))
    ~output:(module Helpers.Or_error (File_path.Absolute))
    ~examples:Examples.Absolute.for_chop_suffix
    ~correctness:(fun { t; suffix } chop_suffix_or_error ->
      require_equal
        (module Helpers.Option (File_path.Absolute))
        (chop_suffix t ~suffix)
        (Or_error.ok chop_suffix_or_error)
        ~message:"[chop_suffix_or_error] and [chop_suffix] are inconsistent");
  [%expect
    {|
    (((t /) (suffix .))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_or_error: not a suffix"
       ((path /) (suffix .)))))
    (((t /..) (suffix .))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_or_error: not a suffix"
       ((path /..) (suffix .)))))
    (((t /b/.) (suffix a/.))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_or_error: not a suffix"
       ((path /b/.) (suffix a/.)))))
    (((t /c/d) (suffix a/b))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_or_error: not a suffix"
       ((path /c/d) (suffix a/b)))))
    (((t /c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_or_error: not a suffix"
       ((path /c) (suffix a/b/c)))))
    (((t /b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_or_error: not a suffix"
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

let chop_suffix_if_exists = File_path.Absolute.chop_suffix_if_exists

let%expect_test _ =
  Helpers.test
    (fun { t; suffix } -> chop_suffix_if_exists t ~suffix)
    ~input:(module Helpers.With_suffix (File_path.Absolute))
    ~output:(module File_path.Absolute)
    ~examples:Examples.Absolute.for_chop_suffix
    ~correctness:(fun { t; suffix } chop_suffix_if_exists ->
      require_equal
        (module File_path.Absolute)
        (chop_suffix t ~suffix |> Option.value ~default:t)
        chop_suffix_if_exists
        ~message:"[chop_suffix_if_exists] and [chop_suffix] are inconsistent");
  [%expect
    {|
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

let append = File_path.Absolute.append

let%expect_test _ =
  Helpers.test
    (fun (prefix, suffix) -> append prefix suffix)
    ~input:(module Helpers.Tuple2 (File_path.Absolute) (File_path.Relative))
    ~output:(module File_path.Absolute)
    ~examples:Examples.Absolute.for_append
    ~correctness:(fun (prefix, suffix) append ->
      require_equal
        (module Helpers.Option (File_path.Relative))
        (Some suffix)
        (chop_prefix append ~prefix)
        ~message:"[append] and [chop_prefix] are inconsistent";
      require_equal
        (module Helpers.Option (File_path.Absolute))
        (Some prefix)
        (chop_suffix append ~suffix)
        ~message:"[append] and [chop_suffix] are inconsistent");
  [%expect
    {|
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

let number_of_parts = File_path.Absolute.number_of_parts

let%expect_test _ =
  Helpers.test
    number_of_parts
    ~input:(module File_path.Absolute)
    ~output:(module Int)
    ~examples:Examples.Absolute.for_conversion
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
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

let to_parts = File_path.Absolute.to_parts

let%expect_test _ =
  Helpers.test
    to_parts
    ~input:(module File_path.Absolute)
    ~output:(module Helpers.List (File_path.Part))
    ~examples:Examples.Absolute.for_conversion
    ~correctness:(fun absolute to_parts ->
      require_equal
        (module Int)
        (List.length to_parts)
        (number_of_parts absolute)
        ~message:"[to_parts] and [number_of_parts] are inconsistent");
  [%expect
    {|
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

let of_parts = File_path.Absolute.of_parts

let%expect_test _ =
  Helpers.test
    of_parts
    ~input:(module Helpers.List (File_path.Part))
    ~output:(module File_path.Absolute)
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts ->
      require_equal
        (module Helpers.List (File_path.Part))
        (to_parts of_parts)
        parts
        ~message:"[of_parts] and [to_parts] are inconsistent");
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

let simplify_dot = File_path.Absolute.simplify_dot

let%expect_test _ =
  Helpers.test
    simplify_dot
    ~input:(module File_path.Absolute)
    ~output:(module File_path.Absolute)
    ~examples:Examples.Absolute.for_simplify
    ~correctness:(fun original simplified ->
      require_equal
        (module Helpers.List (File_path.Part))
        (to_parts simplified)
        (to_parts original |> List.filter ~f:(File_path.Part.( <> ) File_path.Part.dot))
        ~message:"[simplify_dot] is not equivalent to filtering out [.]";
      if equal original simplified
      then
        require_no_allocation (fun () ->
          ignore (Sys.opaque_identity (simplify_dot original) : t)));
  [%expect
    {|
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

let simplify_dot_and_dot_dot_naively = File_path.Absolute.simplify_dot_and_dot_dot_naively

let%expect_test _ =
  Helpers.test
    simplify_dot_and_dot_dot_naively
    ~input:(module File_path.Absolute)
    ~output:(module File_path.Absolute)
    ~examples:Examples.Absolute.for_simplify
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
        (module File_path.Absolute)
        simplified
        (simplify_dot_and_dot_dot_naively (simplify_dot original))
        ~message:"[simplify_dot_and_dot_dot_naively] does not ignore [.] parts";
      require_equal
        (module File_path.Absolute)
        simplified
        (simplify_dot simplified)
        ~message:"[simplify_dot_and_dot_dot_naively] does not simplify all [.] parts";
      require_equal
        (module File_path.Absolute)
        simplified
        (simplify_dot_and_dot_dot_naively simplified)
        ~message:"[simplify_dot_and_dot_dot_naively] is not idempotent";
      if equal original simplified
      then
        require_no_allocation (fun () ->
          ignore (Sys.opaque_identity (simplify_dot_and_dot_dot_naively original) : t)));
  [%expect
    {|
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
include Test_absolute_completion
