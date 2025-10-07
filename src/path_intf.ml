open! Core

(** A path represents one or more path parts relative to either the root directory or an
    unspecified "current" directory.

    Valid path strings must be non-empty and must contain no null characters.

    Canonical path strings must contain no consecutive slashes and must not end in a
    slash, except for the root path [/]. *)
module type S = sig @@ portable
  module Types : Types.S
  open Types

  (** Paths are a subtype of [string]. *)
  include Common.S with module Type := Types.Path

  include Quickcheckable.S with type t := t

  (** The root directory, i.e. [/]. *)
  val root : t

  (** The current directory, i.e. [.]. *)
  val dot : t

  (** The parent directory, i.e. [..]. *)
  val dot_dot : t

  (** Returns the final part of the given path, or [None] if given [root]. *)
  val basename : t -> Part.t option

  (** Returns the final part of the given path, or raises if given [root]. *)
  val basename_exn : t -> Part.t

  (** Returns the final part of the given path, or returns an error if given [root]. *)
  val basename_or_error : t -> Part.t Or_error.t

  (** Returns the final part of the given path, or [Part.dot] if given [root]. *)
  val basename_defaulting_to_dot : t -> Part.t

  (** Returns all parts of the given path but the final one, or [None] if given [root] or
      a relative path of a single part. *)
  val dirname : t -> t option

  (** Returns all parts of the given path but the final one, or raises if given [root] or
      a relative path of a single part. *)
  val dirname_exn : t -> t

  (** Returns all parts of the given path but the final one, or returns an error if given
      [root] or a relative path of a single part. *)
  val dirname_or_error : t -> t Or_error.t

  (** Returns all parts of the given path but the final one, or [root] if given [root], or
      [dot] if given a relative path of a single part. *)
  val dirname_defaulting_to_dot_or_root : t -> t

  (** Like [Option.both (dirname t) (basename t)]. Allocates [Some] at most once. *)
  val dirname_and_basename : t -> (t * Part.t) option

  (** Adds the given string as a suffix of the path's basename. Raises if [t] is [root]
      and therefore has no basename, or if the string contains characters that are illegal
      for a path part. *)
  val append_to_basename_exn : t -> string -> t

  (** Adds a part to the end of the path. *)
  val append_part : t -> Part.t -> t

  (** Appends the parts of a relative path to the end of the path. *)
  val append : t -> Relative.t -> t

  (** Reports if the parts of [prefix] are a non-strict prefix of the parts of the other
      argument, and the paths are both absolute or both relative. *)
  val is_prefix : t -> prefix:t -> bool

  (** Reports if the parts of [suffix] are a non-strict suffix of the parts of the other
      argument. *)
  val is_suffix : t -> suffix:Relative.t -> bool

  (** Returns all parts of the given path after [prefix], or [None] if [prefix] is not a
      prefix of the path's parts. Always returns [None] if the path and [prefix] do not
      match in absolute/relative status. If the path equals [prefix], returns [dot]. *)
  val chop_prefix : t -> prefix:t -> Relative.t option

  (** Returns all parts of the given path after [prefix], or raises if [prefix] is not a
      prefix of the path's parts. Always raises if the path and [prefix] do not match in
      absolute/relative status. If the path equals [prefix], returns [dot]. *)
  val chop_prefix_exn : t -> prefix:t -> Relative.t

  (** Returns all parts of the given path after [prefix], or returns an error if [prefix]
      is not a prefix of the path's parts. Always returns an error if the path and
      [prefix] do not match in absolute/relative status. If the path equals [prefix],
      returns [dot]. *)
  val chop_prefix_or_error : t -> prefix:t -> Relative.t Or_error.t

  (** Returns all parts of the given path after [prefix], or returns the path unchanged if
      [prefix] is not a prefix of the path's parts. If the path equals [prefix], returns
      [dot]. *)
  val chop_prefix_if_exists : t -> prefix:t -> t

  (** Returns all parts of the given path before [suffix], or [None] if [suffix] is not a
      suffix of the path's parts. If the path equals [suffix], returns [dot]. *)
  val chop_suffix : t -> suffix:Relative.t -> t option

  (** Returns all parts of the given path before [suffix], or raises if [suffix] is not a
      suffix of the path's parts. If the path equals [suffix], returns [dot]. *)
  val chop_suffix_exn : t -> suffix:Relative.t -> t

  (** Returns all parts of the given path before [suffix], or returns an error if [suffix]
      is not a suffix of the path's parts. If the path equals [suffix], returns [dot]. *)
  val chop_suffix_or_error : t -> suffix:Relative.t -> t Or_error.t

  (** Returns all parts of the given path before [suffix], or returns the path unchanged
      if [suffix] is not a suffix of the path's parts. If the path equals [suffix],
      returns [dot]. *)
  val chop_suffix_if_exists : t -> suffix:Relative.t -> t

  (** Removes [.] parts from the given path. Returns [.] if the given path is a relative
      path consisting only of one or more [.] parts. *)
  val simplify_dot : t -> t

  (** Removes [.] parts from the given path. Cancels out [..] parts with preceding parts
      (that are neither [.] nor [..]). Removes any [..] parts immediately following a
      leading [/]. Does not check the file system; in the presence of symlinks, the
      resulting path may not be equivalent. Returns [.] if the given path is a relative
      path and all parts are canceled out. *)
  val simplify_dot_and_dot_dot_naively : t -> t

  (** Returns a relative path consisting of the given part. *)
  val of_part_relative : Part.t -> t

  (** Produces the parts of the path. *)
  val to_parts : t -> Part.t list

  (** Constructs an absolute path from the given parts. *)
  val of_parts_absolute : Part.t list -> t

  (** Returns a relative path consisting of the given one or more parts, or [None] if the
      list of parts is empty. *)
  val of_parts_relative : Part.t list -> t option

  (** Returns a relative path consisting of the given one or more parts, or raises if the
      list of parts is empty. *)
  val of_parts_relative_exn : Part.t list -> t

  (** Returns a relative path consisting of the given one or more parts, or returns an
      error if the list of parts is empty. *)
  val of_parts_relative_or_error : Part.t list -> t Or_error.t

  (** Returns a relative path consisting of the given one or more parts, or [dot] if the
      list of parts is empty. *)
  val of_parts_relative_defaulting_to_dot : Part.t list -> t

  (** Returns a relative path consisting of the given one or more parts. *)
  val of_parts_relative_nonempty : Part.t Nonempty_list.t -> t

  (** Equivalent to [List.length (to_parts t)], without allocating. *)
  val number_of_parts : t -> int

  (** If [t] is absolute, returns [t] as an absolute path. If [t] is relative, returns
      [Absolute.append under t]. *)
  val make_absolute : t -> under:Absolute.t -> Absolute.t

  (** If [t] is relative, returns [t] as a relative path. If [t] is absolute, returns
      [chop_prefix t ~prefix:if_under]. *)
  val make_relative : t -> if_under:Absolute.t -> Relative.t option

  (** If [t] is relative, returns [t] as a relative path. If [t] is absolute, returns
      [chop_prefix_exn t ~prefix:if_under]. *)
  val make_relative_exn : t -> if_under:Absolute.t -> Relative.t

  (** If [t] is relative, returns [t] as a relative path. If [t] is absolute, returns
      [chop_prefix_or_error t ~prefix:if_under]. *)
  val make_relative_or_error : t -> if_under:Absolute.t -> Relative.t Or_error.t

  (** If [t] is relative, returns [t] as a relative path. If [t] is absolute, returns
      [chop_prefix_if_exists t ~prefix:if_under]. *)
  val make_relative_if_possible : t -> if_under:Absolute.t -> t

  (** Reports if a path is absolute, i.e. starts with a slash. *)
  val is_absolute : t -> bool

  (** Reports if a path is relative, i.e. does not start with a slash. *)
  val is_relative : t -> bool

  (** Converts an absolute path to a path. Is the identity function. *)
  val of_absolute : Absolute.t -> t

  (** Converts a relative path to a path. Is the identity function. *)
  val of_relative : Relative.t -> t

  (** Returns [t] as an absolute path, or [None] if it is relative. *)
  val to_absolute : t -> Absolute.t option

  (** Returns [t] as a relative path, or [None] if it is absolute. *)
  val to_relative : t -> Relative.t option

  (** Returns [t] as an absolute path, or raises if it is relative. *)
  val to_absolute_exn : t -> Absolute.t

  (** Returns [t] as a relative path, or raises if it is absolute. *)
  val to_relative_exn : t -> Relative.t

  (** Returns [t] as an absolute path, or returns an error if it is relative. *)
  val to_absolute_or_error : t -> Absolute.t Or_error.t

  (** Returns [t] as a relative path, or returns an error if it is absolute. *)
  val to_relative_or_error : t -> Relative.t Or_error.t

  module Variant : sig
    (** Represents a path as an explicit variant of relative or absolute path. Useful for
        pattern matching on an otherwise abstract datatype. *)
    type t =
      | Relative of Relative.t
      | Absolute of Absolute.t
    [@@deriving compare ~localize, equal ~localize, quickcheck, sexp_of]

    include Invariant.S with type t := t
  end

  (** Converts a path to a variant. *)
  val to_variant : t -> Variant.t

  (** Converts a variant to a path. *)
  val of_variant : Variant.t -> t
end

module type Path = sig @@ portable
  module type S = S

  include S with module Types := Types
  module Stable : Common.Stable with module Type := Types.Path
end
