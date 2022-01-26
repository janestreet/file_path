open! Core
module Types = Types
include Path
module Absolute = Absolute
module Relative = Relative
module Part = Part
module Operators = Operators

module Stable = struct
  include Path.Stable
  module Absolute = Absolute.Stable
  module Relative = Relative.Stable
  module Part = Part.Stable
end

include File_path_intf
