open! Core

module Definitions = struct
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

    (** Reports if the parts of [prefix] are a non-strict prefix of the parts of the other
        argument. *)
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

    (** Returns all parts of the given path but the final one, or [None] if given [root]. *)
    val dirname : t @ l -> t option @ l
    [@@alloc a]

    (** Returns all parts of the given path but the final one, or raises if given [root]. *)
    val dirname_exn : t @ l -> t @ l
    [@@alloc a]

    (** Returns all parts of the given path but the final one, or returns an error if
        given [root]. *)
    val dirname_or_error : t @ l -> t Or_error.t @ l
    [@@alloc a]

    (** Returns all parts of the given path but the final one, or [root] if given [root]. *)
    val dirname_defaulting_to_root : t @ l -> t @ l
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

    (** Appends the parts of a relative path to the end of the absolute path. *)
    val append : t @ local -> Relative.t @ local -> t @ l
    [@@alloc a]

    (** Returns all parts of the given path after [prefix], or [None] if [prefix] is not a
        prefix of the path's parts. If the path equals [prefix], returns [Relative.dot]. *)
    val chop_prefix : t @ l -> prefix:t @ local -> Relative.t option @ l
    [@@alloc a]

    (** Returns all parts of the given path after [prefix], or raises if [prefix] is not a
        prefix of the path's parts. If the path equals [prefix], returns [Relative.dot]. *)
    val chop_prefix_exn : t @ l -> prefix:t @ local -> Relative.t @ l
    [@@alloc a]

    (** Returns all parts of the given path after [prefix], or returns an error if
        [prefix] is not a prefix of the path's parts. If the path equals [prefix], returns
        [Relative.dot]. *)
    val chop_prefix_or_error : t @ l -> prefix:t @ local -> Relative.t Or_error.t @ l
    [@@alloc a]

    (** Returns all parts of the given path before [suffix], or [None] if [suffix] is not
        a suffix of the path's parts. *)
    val chop_suffix : t @ l -> suffix:Relative.t @ local -> t option @ l
    [@@alloc a]

    (** Returns all parts of the given path before [suffix], or raises if [suffix] is not
        a suffix of the path's parts. *)
    val chop_suffix_exn : t @ l -> suffix:Relative.t @ local -> t @ l
    [@@alloc a]

    (** Returns all parts of the given path before [suffix], or returns an error if
        [suffix] is not a suffix of the path's parts. *)
    val chop_suffix_or_error : t @ l -> suffix:Relative.t @ local -> t Or_error.t @ l
    [@@alloc a]

    (** Returns all parts of the given path before [suffix], or returns the path unchanged
        if [suffix] is not a suffix of the path's parts. *)
    val chop_suffix_if_exists : t @ l -> suffix:Relative.t @ local -> t @ l
    [@@alloc a]

    (** Removes [.] parts from the given path. *)
    val simplify_dot : t @ l -> t @ l
    [@@alloc a]

    (** Removes [.] parts from the given path. Cancels out [..] parts with preceding parts
        (that are neither [.] nor [..]). Removes any [..] parts immediately following the
        leading [/]. Does not check the file system; in the presence of symlinks, the
        resulting path may not be equivalent. *)
    val simplify_dot_and_dot_dot_naively : t @ l -> t @ l
    [@@alloc a]

    (** Produces the parts of the path. *)
    val to_parts : t @ l -> Part.t list @ l
    [@@alloc a]

    (** Constructs an absolute path from the given parts. *)
    val of_parts : Part.t list @ local -> t @ l
    [@@alloc a]]
  end
end

module type Absolute = sig @@ portable
  include module type of struct
    include Definitions
  end

  include S with module Types := Types
  module Stable : Common.Stable with module Type := Types.Absolute
end
