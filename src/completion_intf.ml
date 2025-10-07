open! Core

module type S = sig @@ portable
  (** Command-line completion for various path types in [bash]. *)

  val complete_path : string -> string list
  val complete_absolute : string -> string list
  val complete_relative : string -> string list
  val complete_part : string -> string list
end

module type Completion = sig @@ portable
  module type S = S

  include S
end
