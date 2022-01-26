open! Core

(* The operators are synonyms for already-benchmarked functions. *)

let ( ~/ ) = Bench_relative.of_string
let ( !/ ) = Bench_absolute.of_string
let ( ?/ ) = Bench_path.of_string
let ( ~. ) = Bench_part.of_string
let ( /~/ ) = Bench_relative.append
let ( /!/ ) = Bench_absolute.append
let ( /?/ ) = Bench_path.append
let ( /~. ) = Bench_relative.append_part
let ( /!. ) = Bench_absolute.append_part
let ( /?. ) = Bench_path.append_part
let ( /~^ ) = Bench_relative.append_to_basename_exn
let ( /!^ ) = Bench_absolute.append_to_basename_exn
let ( /?^ ) = Bench_path.append_to_basename_exn
let ( /.^ ) = Bench_part.append_to_basename_exn
