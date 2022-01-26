open! Core

(* The operators are synonyms for already-tested functions. *)

let ( ~/ ) = Test_relative.of_string
let ( !/ ) = Test_absolute.of_string
let ( ?/ ) = Test_path.of_string
let ( ~. ) = Test_part.of_string
let ( /~/ ) = Test_relative.append
let ( /!/ ) = Test_absolute.append
let ( /?/ ) = Test_path.append
let ( /~. ) = Test_relative.append_part
let ( /!. ) = Test_absolute.append_part
let ( /?. ) = Test_path.append_part
let ( /~^ ) = Test_relative.append_to_basename_exn
let ( /!^ ) = Test_absolute.append_to_basename_exn
let ( /?^ ) = Test_path.append_to_basename_exn
let ( /.^ ) = Test_part.append_to_basename_exn
