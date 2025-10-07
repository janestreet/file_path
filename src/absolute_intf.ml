open! Core

(** An absolute path represents one or more path parts relative to the root directory.

    Valid absolute path strings must be non-empty, must contain no null characters, and
    must start with a slash.

    Canonical absolute path strings must contain no consecutive slashes and must not end
    in a slash, except for root path [/]. *)
module type S = sig @@ portable
  module Types : Types.S
  open Types

  (** Absolute paths are a subtype of [Path.t] and therefore also of [string]. *)
  include Common.S with module Type := Types.Absolute

  include Quickcheckable.S with type t := t

  (** The root directory, i.e. [/]. *)
  val root : t

  (** Returns the final part of the given path, or [None] if given [root]. *)
  val basename : t -> Part.t option

  (** Returns the final part of the given path, or raises if given [root]. *)
  val basename_exn : t -> Part.t

  (** Returns the final part of the given path, or returns an error if given [root]. *)
  val basename_or_error : t -> Part.t Or_error.t

  (** Returns the final part of the given path, or [Part.dot] if given [root]. *)
  val basename_defaulting_to_dot : t -> Part.t

  (** Returns all parts of the given path but the final one, or [None] if given [root]. *)
  val dirname : t -> t option

  (** Returns all parts of the given path but the final one, or raises if given [root]. *)
  val dirname_exn : t -> t

  (** Returns all parts of the given path but the final one, or returns an error if given
      [root]. *)
  val dirname_or_error : t -> t Or_error.t

  (** Returns all parts of the given path but the final one, or [root] if given [root]. *)
  val dirname_defaulting_to_root : t -> t

  (** Like [Option.both (dirname t) (basename t)]. Allocates [Some] at most once. *)
  val dirname_and_basename : t -> (t * Part.t) option

  (** Adds the given string as a suffix of the path's basename. Raises if [t] is [root]
      and therefore has no basename, or if the string contains characters that are illegal
      for a path part. *)
  val append_to_basename_exn : t -> string -> t

  (** Adds a part to the end of the path. *)
  val append_part : t -> Part.t -> t

  (** Appends the parts of a relative path to the end of the absolute path. *)
  val append : t -> Relative.t -> t

  (** Reports if the parts of [prefix] are a non-strict prefix of the parts of the other
      argument. *)
  val is_prefix : t -> prefix:t -> bool

  (** Reports if the parts of [suffix] are a non-strict suffix of the parts of the other
      argument. *)
  val is_suffix : t -> suffix:Relative.t -> bool

  (** Returns all parts of the given path after [prefix], or [None] if [prefix] is not a
      prefix of the path's parts. If the path equals [prefix], returns [Relative.dot]. *)
  val chop_prefix : t -> prefix:t -> Relative.t option

  (** Returns all parts of the given path after [prefix], or raises if [prefix] is not a
      prefix of the path's parts. If the path equals [prefix], returns [Relative.dot]. *)
  val chop_prefix_exn : t -> prefix:t -> Relative.t

  (** Returns all parts of the given path after [prefix], or returns an error if [prefix]
      is not a prefix of the path's parts. If the path equals [prefix], returns
      [Relative.dot]. *)
  val chop_prefix_or_error : t -> prefix:t -> Relative.t Or_error.t

  (** Returns all parts of the given path before [suffix], or [None] if [suffix] is not a
      suffix of the path's parts. *)
  val chop_suffix : t -> suffix:Relative.t -> t option

  (** Returns all parts of the given path before [suffix], or raises if [suffix] is not a
      suffix of the path's parts. *)
  val chop_suffix_exn : t -> suffix:Relative.t -> t

  (** Returns all parts of the given path before [suffix], or returns an error if [suffix]
      is not a suffix of the path's parts. *)
  val chop_suffix_or_error : t -> suffix:Relative.t -> t Or_error.t

  (** Returns all parts of the given path before [suffix], or returns the path unchanged
      if [suffix] is not a suffix of the path's parts. *)
  val chop_suffix_if_exists : t -> suffix:Relative.t -> t

  (** Removes [.] parts from the given path. *)
  val simplify_dot : t -> t

  (** Removes [.] parts from the given path. Cancels out [..] parts with preceding parts
      (that are neither [.] nor [..]). Removes any [..] parts immediately following the
      leading [/]. Does not check the file system; in the presence of symlinks, the
      resulting path may not be equivalent. *)
  val simplify_dot_and_dot_dot_naively : t -> t

  (** Produces the parts of the path. *)
  val to_parts : t -> Part.t list

  (** Constructs an absolute path from the given parts. *)
  val of_parts : Part.t list -> t

  (** Equivalent to [List.length (to_parts t)], without allocating. *)
  val number_of_parts : t -> int
end

module type Absolute = sig @@ portable
  module type S = S

  include S with module Types := Types
  module Stable : Common.Stable with module Type := Types.Absolute
end
