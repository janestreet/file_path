open! Core
include Operators_intf

(* The operators are synonyms for existing functions. No actual code should go in this
   file, all operators should also be exported by full names. *)

let ( ~/ ) = Relative.of_string
let ( !/ ) = Absolute.of_string
let ( ?/ ) = Path.of_string
let ( ~. ) = Part.of_string
let ( /~/ ) = Relative.append
let ( /!/ ) = Absolute.append
let ( /?/ ) = Path.append
let ( /~. ) = Relative.append_part
let ( /!. ) = Absolute.append_part
let ( /?. ) = Path.append_part
let ( /~^ ) = Relative.append_to_basename_exn
let ( /!^ ) = Absolute.append_to_basename_exn
let ( /?^ ) = Path.append_to_basename_exn
let ( /.^ ) = Part.append_to_basename_exn
