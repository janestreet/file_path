open! Core

module Definitions = struct
  module type S = sig @@ portable
    (** Command-line completion for various path types in [bash]. *)

    val complete_path : string -> string list
    val complete_absolute : string -> string list
    val complete_relative : string -> string list
    val complete_part : string -> string list
  end
end

module type Completion = sig @@ portable
  include module type of struct
    include Definitions
  end

  include S
end
