(** Implements typed file paths. See [../doc/index.mdx] for an introduction. *)

open! Core

module type Types = Types.S
module type Part = Part.S
module type Relative = Relative.S
module type Absolute = Absolute.S
module type Path = Path.S

module type Stable = sig
  module Types : Types.S
  module Part : Common.Stable with module Type := Types.Part
  module Relative : Common.Stable with module Type := Types.Relative
  module Absolute : Common.Stable with module Type := Types.Absolute
  include Common.Stable with module Type := Types.Path
end

module type S = sig
  (** Everything below uses the types and subtyping relationships of [Types]. *)
  module Types : Types.S

  module Part : Part.S with module Types := Types
  module Relative : Relative.S with module Types := Types
  module Absolute : Absolute.S with module Types := Types
  include Path.S with module Types := Types
  module Stable : Stable with module Types := Types
end

module type File_path = sig
  module type Types = Types
  module type Part = Part
  module type Relative = Relative
  module type Absolute = Absolute
  module type Path = Path
  module type Stable = Stable
  module type S = S

  include S
end
