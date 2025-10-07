(** Implements typed file paths. See [../doc/index.mdx] for an introduction. *)

open! Core

module type Types = Types.S
module type Part = Part.S
module type Relative = Relative.S
module type Absolute = Absolute.S
module type Path = Path.S
module type Operators = Operators.S

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
  module Operators : Operators with module Types := Types
  module Stable : Stable with module Types := Types
end

module type File_path = sig @@ portable
  module type Types = Types
  module type Part = Part
  module type Relative = Relative
  module type Absolute = Absolute
  module type Path = Path
  module type Operators = Operators
  module type Stable = Stable
  module type S = S

  include S
end
