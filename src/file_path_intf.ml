(** Implements typed file paths. See [../doc/index.mdx] for an introduction. *)

open! Core

module Definitions = struct
  module type Types = Types.S
  module type Type = Types.Type
  module type Common = Common.S
  module type Part = Part.S
  module type Relative = Relative.S
  module type Absolute = Absolute.S
  module type Path = Path.S
  module type%template Operators = Operators.S [@alloc a] [@@alloc a = (stack, heap)]

  module type Stable = sig @@ portable
    module Types : Types
    module Part : Common.Stable with module Type := Types.Part
    module Relative : Common.Stable with module Type := Types.Relative
    module Absolute : Common.Stable with module Type := Types.Absolute
    include Common.Stable with module Type := Types.Path
  end

  module type S = sig @@ portable
    (** Everything below uses the types and subtyping relationships of [Types]. *)
    module Types : Types

    module Part : Part with module Types := Types
    module Relative : Relative with module Types := Types
    module Absolute : Absolute with module Types := Types
    include Path with module Types := Types

    module%template [@alloc a = (stack, heap)] Operators :
      Operators.S [@alloc a] with module Types := Types

    module Stable : Stable with module Types := Types
  end
end

module type File_path = sig @@ portable
  include module type of struct
    include Definitions
  end

  include S
end
