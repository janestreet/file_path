open! Core

(** We do not need to test the [Types] module. *)
module Types = File_path.Types

module Absolute = Test_absolute
module Relative = Test_relative
module Part = Test_part
include Test_path
module Operators = Test_operators
module Stable = Test_stable
