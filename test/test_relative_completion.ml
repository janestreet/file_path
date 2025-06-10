open! Core
open! Async

let arg_type = File_path.Relative.arg_type

let%expect_test _ =
  let%bind () =
    Helpers_async.test_arg_type (module File_path.Relative) ~expect_output:(fun () ->
      [%expect.output])
  in
  [%expect
    {|
    ""
    Choose: ".fe/", "app/", "home/", "lib/", "libmap.sexp"

    "."
    Choose: "", "../", "./", ".fe/"

    ".f"
    ".fe"
    Extend: ".fe/"

    ".fe/"
    Finish: ".fe/"

    "a"
    "ap"
    "app"
    Extend: "app/"

    "app/"
    "app//"
    ...
    "app//too"
    "app//tool"
    Extend: "app/tool/"

    "app//tool/"
    "app//tool//"
    Choose: "jbuild", "tool.ml", "tool.mli", "tool_intf.ml"

    "app//tool//j"
    "app//tool//jb"
    ...
    "app//tool//jbuil"
    "app//tool//jbuild"
    Finish: "app/tool/jbuild"

    "app/t"
    "app/to"
    "app/too"
    "app/tool"
    Extend: "app/tool/"

    "app/tool/"
    Choose: "jbuild", "tool.ml", "tool.mli", "tool_intf.ml"

    "app/tool/j"
    "app/tool/jb"
    ...
    "app/tool/jbuil"
    "app/tool/jbuild"
    Finish: "app/tool/jbuild"

    "app/tool/t"
    "app/tool/to"
    "app/tool/too"
    Extend: "app/tool/tool"

    "app/tool/tool"
    Choose: "", "tool.ml", "tool.mli", "tool_intf.ml"

    "app/tool/tool."
    "app/tool/tool.m"
    Extend: "app/tool/tool.ml"

    "app/tool/tool.ml"
    Choose: "", "tool.ml", "tool.mli"

    "app/tool/tool.mli"
    Finish: "app/tool/tool.mli"

    "app/tool/tool_"
    "app/tool/tool_i"
    ...
    "app/tool/tool_intf.m"
    "app/tool/tool_intf.ml"
    Finish: "app/tool/tool_intf.ml"

    "h"
    "ho"
    "hom"
    "home"
    Extend: "home/"

    "home/"
    Choose: "dir/", "file", "qu\\\"te/", "s\\\\ash/", "sp\\ ce/", "t\\'ck/"

    "home/d"
    "home/di"
    "home/dir"
    Extend: "home/dir/"

    "home/dir/"
    Finish: "home/dir/"

    "home/f"
    "home/fi"
    "home/fil"
    "home/file"
    Finish: "home/file"

    "home/q"
    "home/qu"
    ...
    "home/qu\\\"t"
    "home/qu\\\"te"
    Extend: "home/qu\\\"te/"

    "home/qu\\\"te/"
    Finish: "home/qu\\\"te/"

    "home/s"
    "home/s\\"
    Choose: "", "s\\\\ash/", "sp\\ ce/"

    "home/s\\\\"
    "home/s\\\\a"
    "home/s\\\\as"
    "home/s\\\\ash"
    Extend: "home/s\\\\ash/"

    "home/s\\\\ash/"
    Finish: "home/s\\\\ash/"

    "home/s\\a"
    "home/s\\as"
    "home/s\\ash"
    "home/s\\ash/"
    Empty

    "home/sp"
    "home/sp\\"
    "home/sp\\ "
    "home/sp\\ c"
    "home/sp\\ ce"
    Extend: "home/sp\\ ce/"

    "home/sp\\ ce/"
    Finish: "home/sp\\ ce/"

    "home/t"
    "home/t\\"
    "home/t\\'"
    "home/t\\'c"
    "home/t\\'ck"
    Extend: "home/t\\'ck/"

    "home/t\\'ck/"
    Finish: "home/t\\'ck/"

    "l"
    "li"
    Extend: "lib"

    "lib"
    Choose: "", "lib/", "libmap.sexp"

    "lib/"
    "lib/c"
    "lib/co"
    "lib/cod"
    "lib/code"
    Extend: "lib/code/"

    "lib/code/"
    Choose: "code.ml", "code.mli", "code_intf.ml", "jbuild"

    "lib/code/c"
    "lib/code/co"
    "lib/code/cod"
    Extend: "lib/code/code"

    "lib/code/code"
    Choose: "", "code.ml", "code.mli", "code_intf.ml"

    "lib/code/code."
    "lib/code/code.m"
    Extend: "lib/code/code.ml"

    "lib/code/code.ml"
    Choose: "", "code.ml", "code.mli"

    "lib/code/code.mli"
    Finish: "lib/code/code.mli"

    "lib/code/code_"
    "lib/code/code_i"
    ...
    "lib/code/code_intf.m"
    "lib/code/code_intf.ml"
    Finish: "lib/code/code_intf.ml"

    "lib/code/j"
    "lib/code/jb"
    ...
    "lib/code/jbuil"
    "lib/code/jbuild"
    Finish: "lib/code/jbuild"

    "libm"
    "libma"
    ...
    "libmap.sex"
    "libmap.sexp"
    Finish: "libmap.sexp"
    |}];
  return ()
;;
