open! Core

(** A part represents a single file or directory name within a path. For example, in
    "/usr/bin/ls", there are three parts: "usr", "bin", and "ls".

    Valid part strings must be non-empty, and must contain neither null characters nor
    slash characters. All valid part strings are canonical. *)
module type S = sig @@ portable
  module Types : Types.S

  (** Parts are a subtype of [Relative.t], [Path.t], and [string]. *)
  include Common.S with module Type := Types.Part

  include Quickcheckable.S with type t := t

  (** The current directory, i.e. [.]. *)
  val dot : t

  (** The parent directory, i.e. [..]. *)
  val dot_dot : t

  (** Adds the given string as a suffix of the path part. Raises if the string contains
      characters that are illegal for a path part.

      We use "_to_basename_" in the name for consistency with similar operations on other
      path types. A path part is its own basename. *)
  val append_to_basename_exn : t -> string -> t
end

module type Part = sig @@ portable
  module type S = S

  include S with module Types := Types
  module Stable : Common.Stable with module Type := Types.Part
end
