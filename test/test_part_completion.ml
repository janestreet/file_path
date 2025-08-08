open! Core
open! Async

let arg_type = File_path.Part.arg_type

let%expect_test _ =
  let%bind () =
    Helpers_async.test_arg_type (module File_path.Part) ~expect_output:(fun () ->
      [%expect.output])
  in
  [%expect
    {|
    ""
    Choose: ".fe", "app", "home", "lib", "libmap.sexp"

    "."
    Choose: "", ".", "..", ".fe"

    ".f"
    ".fe"
    Finish: ".fe"

    "a"
    "ap"
    "app"
    Finish: "app"

    "h"
    "ho"
    "hom"
    "home"
    Finish: "home"

    "l"
    "li"
    Extend: "lib"

    "lib"
    Choose: "", "lib", "libmap.sexp"

    "libm"
    "libma"
    ...
    "libmap.sex"
    "libmap.sexp"
    Finish: "libmap.sexp"
    |}];
  return ()
;;
