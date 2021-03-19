open! Core_kernel

(** A part represents a single file or directory name within a path. For example, in
    "/usr/bin/ls", there are three parts: "usr", "bin", and "ls".

    Valid part strings must be non-empty, and must contain neither null characters nor
    slash characters. All valid part strings are canonical. *)
module type S = sig
  module Types : Types.S

  (** Parts are a subtype of [Relative.t], [Path.t], and [string]. *)
  include Common.S with module Type := Types.Part

  include Quickcheckable.S with type t := t

  (** The current directory, i.e. [.]. *)
  val dot : t

  (** The parent directory, i.e. [..]. *)
  val dot_dot : t
end

module type Part = sig
  module type S = S

  include S with module Types := Types
  module Stable : Common.Stable with module Type := Types.Part
end
