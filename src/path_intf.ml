open! Core

module Definitions = struct
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

    module Variant : sig
      (** Represents a path as an explicit variant of relative or absolute path. Useful
          for pattern matching on an otherwise abstract datatype. *)
      type t =
        | Relative of Relative.t
        | Absolute of Absolute.t
      [@@deriving compare ~localize, equal ~localize, globalize, quickcheck, sexp_of]

      include Invariant.S with type t := t
    end

    (** The root directory, i.e. [/]. *)
    val root : t

    (** The current directory, i.e. [.]. *)
    val dot : t

    (** The parent directory, i.e. [..]. *)
    val dot_dot : t

    (** Reports if the parts of [prefix] are a non-strict prefix of the parts of the other
        argument, and the paths are both absolute or both relative. *)
    val is_prefix : t @ local -> prefix:t @ local -> bool

    (** Reports if the parts of [suffix] are a non-strict suffix of the parts of the other
        argument. *)
    val is_suffix : t @ local -> suffix:Relative.t @ local -> bool

    (** Equivalent to [List.length (to_parts t)], without allocating. *)
    val number_of_parts : t @ local -> int

    [%%template:
    [@@@alloc a @ l = (stack_local, heap_global)]

    (** Returns the final part of the given path, or [None] if given [root]. *)
    val basename : t @ l -> Part.t option @ l
    [@@alloc a]

    (** Returns the final part of the given path, or raises if given [root]. *)
    val basename_exn : t @ l -> Part.t @ l
    [@@alloc a]

    (** Returns the final part of the given path, or returns an error if given [root]. *)
    val basename_or_error : t @ l -> Part.t Or_error.t @ l
    [@@alloc a]

    (** Returns the final part of the given path, or [Part.dot] if given [root]. *)
    val basename_defaulting_to_dot : t @ l -> Part.t @ l
    [@@alloc a]

    (** Returns all parts of the given path but the final one, or [None] if given [root]
        or a relative path of a single part. *)
    val dirname : t @ l -> t option @ l
    [@@alloc a]

    (** Returns all parts of the given path but the final one, or raises if given [root]
        or a relative path of a single part. *)
    val dirname_exn : t @ l -> t @ l
    [@@alloc a]

    (** Returns all parts of the given path but the final one, or returns an error if
        given [root] or a relative path of a single part. *)
    val dirname_or_error : t @ l -> t Or_error.t @ l
    [@@alloc a]

    (** Returns all parts of the given path but the final one, or [root] if given [root],
        or [dot] if given a relative path of a single part. *)
    val dirname_defaulting_to_dot_or_root : t @ l -> t @ l
    [@@alloc a]

    (** Like [Option.both (dirname t) (basename t)]. Allocates [Some] at most once. *)
    val dirname_and_basename : t @ l -> (t * Part.t) option @ l
    [@@alloc a]

    (** Adds the given string as a suffix of the path's basename. Raises if [t] is [root]
        and therefore has no basename, or if the string contains characters that are
        illegal for a path part. *)
    val append_to_basename_exn : t @ local -> string @ local -> t @ l
    [@@alloc a]

    (** Adds a part to the end of the path. *)
    val append_part : t @ local -> Part.t @ local -> t @ l
    [@@alloc a]

    (** Appends the parts of a relative path to the end of the path. *)
    val append : t @ local -> Relative.t @ local -> t @ l
    [@@alloc a]

    (** Returns all parts of the given path after [prefix], or [None] if [prefix] is not a
        prefix of the path's parts. Always returns [None] if the path and [prefix] do not
        match in absolute/relative status. If the path equals [prefix], returns [dot]. *)
    val chop_prefix : t @ l -> prefix:t @ local -> Relative.t option @ l
    [@@alloc a]

    (** Returns all parts of the given path after [prefix], or raises if [prefix] is not a
        prefix of the path's parts. Always raises if the path and [prefix] do not match in
        absolute/relative status. If the path equals [prefix], returns [dot]. *)
    val chop_prefix_exn : t @ l -> prefix:t @ local -> Relative.t @ l
    [@@alloc a]

    (** Returns all parts of the given path after [prefix], or returns an error if
        [prefix] is not a prefix of the path's parts. Always returns an error if the path
        and [prefix] do not match in absolute/relative status. If the path equals
        [prefix], returns [dot]. *)
    val chop_prefix_or_error : t @ l -> prefix:t @ local -> Relative.t Or_error.t @ l
    [@@alloc a]

    (** Returns all parts of the given path after [prefix], or returns the path unchanged
        if [prefix] is not a prefix of the path's parts. If the path equals [prefix],
        returns [dot]. *)
    val chop_prefix_if_exists : t @ l -> prefix:t @ local -> t @ l
    [@@alloc a]

    (** Returns all parts of the given path before [suffix], or [None] if [suffix] is not
        a suffix of the path's parts. If the path equals [suffix], returns [dot]. *)
    val chop_suffix : t @ l -> suffix:Relative.t @ local -> t option @ l
    [@@alloc a]

    (** Returns all parts of the given path before [suffix], or raises if [suffix] is not
        a suffix of the path's parts. If the path equals [suffix], returns [dot]. *)
    val chop_suffix_exn : t @ l -> suffix:Relative.t @ local -> t @ l
    [@@alloc a]

    (** Returns all parts of the given path before [suffix], or returns an error if
        [suffix] is not a suffix of the path's parts. If the path equals [suffix], returns
        [dot]. *)
    val chop_suffix_or_error : t @ l -> suffix:Relative.t @ local -> t Or_error.t @ l
    [@@alloc a]

    (** Returns all parts of the given path before [suffix], or returns the path unchanged
        if [suffix] is not a suffix of the path's parts. If the path equals [suffix],
        returns [dot]. *)
    val chop_suffix_if_exists : t @ l -> suffix:Relative.t @ local -> t @ l
    [@@alloc a]

    (** Removes [.] parts from the given path. Returns [.] if the given path is a relative
        path consisting only of one or more [.] parts. *)
    val simplify_dot : t @ l -> t @ l
    [@@alloc a]

    (** Removes [.] parts from the given path. Cancels out [..] parts with preceding parts
        (that are neither [.] nor [..]). Removes any [..] parts immediately following a
        leading [/]. Does not check the file system; in the presence of symlinks, the
        resulting path may not be equivalent. Returns [.] if the given path is a relative
        path and all parts are canceled out. *)
    val simplify_dot_and_dot_dot_naively : t @ l -> t @ l
    [@@alloc a]

    (** Returns a relative path consisting of the given part. *)
    val of_part_relative : Part.t @ l -> t @ l
    [@@mode l]

    (** Produces the parts of the path. *)
    val to_parts : t @ l -> Part.t list @ l
    [@@alloc a]

    (** Constructs an absolute path from the given parts. *)
    val of_parts_absolute : Part.t list @ local -> t @ l
    [@@alloc a]

    (** Returns a relative path consisting of the given one or more parts, or [None] if
        the list of parts is empty. *)
    val of_parts_relative : Part.t list @ l -> t option @ l
    [@@alloc a]

    (** Returns a relative path consisting of the given one or more parts, or raises if
        the list of parts is empty. *)
    val of_parts_relative_exn : Part.t list @ l -> t @ l
    [@@alloc a]

    (** Returns a relative path consisting of the given one or more parts, or returns an
        error if the list of parts is empty. *)
    val of_parts_relative_or_error : Part.t list @ l -> t Or_error.t @ l
    [@@alloc a]

    (** Returns a relative path consisting of the given one or more parts, or [dot] if the
        list of parts is empty. *)
    val of_parts_relative_defaulting_to_dot : Part.t list @ l -> t @ l
    [@@alloc a]

    (** Returns a relative path consisting of the given one or more parts. *)
    val of_parts_relative_nonempty : Part.t Nonempty_list.t @ l -> t @ l
    [@@alloc a]

    (** If [t] is absolute, returns [t] as an absolute path. If [t] is relative, returns
        [Absolute.append under t]. *)
    val make_absolute : t @ l -> under:Absolute.t @ local -> Absolute.t @ l
    [@@alloc a]

    (** If [t] is relative, returns [t] as a relative path. If [t] is absolute, returns
        [chop_prefix t ~prefix:if_under]. *)
    val make_relative : t @ l -> if_under:Absolute.t @ local -> Relative.t option @ l
    [@@alloc a]

    (** If [t] is relative, returns [t] as a relative path. If [t] is absolute, returns
        [chop_prefix_exn t ~prefix:if_under]. *)
    val make_relative_exn : t @ l -> if_under:Absolute.t @ local -> Relative.t @ l
    [@@alloc a]

    (** If [t] is relative, returns [t] as a relative path. If [t] is absolute, returns
        [chop_prefix_or_error t ~prefix:if_under]. *)
    val make_relative_or_error
      :  t @ l
      -> if_under:Absolute.t @ local
      -> Relative.t Or_error.t @ l
    [@@alloc a]

    (** If [t] is relative, returns [t] as a relative path. If [t] is absolute, returns
        [chop_prefix_if_exists t ~prefix:if_under]. *)
    val make_relative_if_possible : t @ l -> if_under:Absolute.t @ local -> t @ l
    [@@alloc a]

    (** Reports if a path is absolute, i.e. starts with a slash. *)
    val is_absolute : t @ local -> bool

    (** Reports if a path is relative, i.e. does not start with a slash. *)
    val is_relative : t @ local -> bool

    (** Converts an absolute path to a path. Is the identity function. *)
    val of_absolute : Absolute.t @ l -> t @ l
    [@@mode l]

    (** Converts a relative path to a path. Is the identity function. *)
    val of_relative : Relative.t @ l -> t @ l
    [@@mode l]

    (** Returns [t] as an absolute path, or [None] if it is relative. *)
    val to_absolute : t @ l -> Absolute.t option @ l
    [@@alloc a]

    (** Returns [t] as a relative path, or [None] if it is absolute. *)
    val to_relative : t @ l -> Relative.t option @ l
    [@@alloc a]

    (** Returns [t] as an absolute path, or raises if it is relative. *)
    val to_absolute_exn : t @ l -> Absolute.t @ l
    [@@mode l]

    (** Returns [t] as a relative path, or raises if it is absolute. *)
    val to_relative_exn : t @ l -> Relative.t @ l
    [@@mode l]

    (** Returns [t] as an absolute path, or returns an error if it is relative. *)
    val to_absolute_or_error : t @ l -> Absolute.t Or_error.t @ l
    [@@alloc a]

    (** Returns [t] as a relative path, or returns an error if it is absolute. *)
    val to_relative_or_error : t @ l -> Relative.t Or_error.t @ l
    [@@alloc a]

    (** Converts a path to a variant. *)
    val to_variant : t @ l -> Variant.t @ l
    [@@alloc a]

    (** Converts a variant to a path. *)
    val of_variant : Variant.t @ l -> t @ l [@@mode l]]
  end
end

module type Path = sig @@ portable
  include module type of struct
    include Definitions
  end

  include S with module Types := Types
  module Stable : Common.Stable with module Type := Types.Path
end
