open! Core

module Definitions = struct
  (** A relative path represents one or more path parts relative to some unspecified
      "current" directory.

      Valid relative path strings must be non-empty, must contain no null characters, and
      must not start with a slash.

      Canonical relative path strings must contain no consecutive slashes and must not end
      in a slash. *)
  module type S = sig @@ portable
    module Types : Types.S
    open Types

    (** Relative paths are a subtype of [Path.t] and therefore also of [string]. *)
    include Common.S with module Type := Types.Relative

    include Quickcheckable.S with type t := t

    (** The current directory, i.e. [.]. *)
    val dot : t

    (** The parent directory, i.e. [..]. *)
    val dot_dot : t

    (** Reports if the parts of [prefix] are a non-strict prefix of the parts of the other
        argument. *)
    val is_prefix : t @ local -> prefix:t @ local -> bool

    (** Reports if the parts of [suffix] are a non-strict suffix of the parts of the other
        argument. *)
    val is_suffix : t @ local -> suffix:t @ local -> bool

    (** Equivalent to [List.length (to_parts t)], without allocating. *)
    val number_of_parts : t @ local -> int

    [%%template:
    [@@@alloc a @ l = (stack_local, heap_global)]

    (** Returns the final part of the given path. *)
    val basename : t @ l -> Part.t @ l
    [@@alloc a]

    (** Returns all parts of the given path but the final one, or [None] if the path has
        only one part. *)
    val dirname : t @ l -> t option @ l
    [@@alloc a]

    (** Returns all parts of the given path but the final one, or raises if the path has
        only one part. *)
    val dirname_exn : t @ l -> t @ l
    [@@alloc a]

    (** Returns all parts of the given path but the final one, or returns an error if the
        path has only one part. *)
    val dirname_or_error : t @ l -> t Or_error.t @ l
    [@@alloc a]

    (** Returns all parts of the given path but the final one, or [dot] if the path has
        only one part. *)
    val dirname_defaulting_to_dot : t @ l -> t @ l
    [@@alloc a]

    (** Returns the first part of a multiple-part path, or [None] if given a single-part
        path. *)
    val top_dir : t @ l -> Part.t option @ l
    [@@alloc a]

    (** Returns the first part of a multiple-part path, or raises if given a single-part
        path. *)
    val top_dir_exn : t @ l -> Part.t @ l
    [@@alloc a]

    (** Returns the first part of a multiple-part path, or returns an error if given a
        single-part path. *)
    val top_dir_or_error : t @ l -> Part.t Or_error.t @ l
    [@@alloc a]

    (** Returns the first part of a multiple-part path, or [Part.dot] if given a
        single-part path. *)
    val top_dir_defaulting_to_dot : t @ l -> Part.t @ l
    [@@alloc a]

    (** Returns all but the first part of a multiple-part relative path, or [None] if
        given a single-part path. *)
    val all_but_top_dir : t @ l -> t option @ l
    [@@alloc a]

    (** Returns all but the first part of a multiple-part relative path, or raises if
        given a single-part path. *)
    val all_but_top_dir_exn : t @ l -> t @ l
    [@@alloc a]

    (** Returns all but the first part of a multiple-part relative path, or returns an
        error if given a single-part path. *)
    val all_but_top_dir_or_error : t @ l -> t Or_error.t @ l
    [@@alloc a]

    (** Returns all but the first part of a multiple-part relative path, or returns the
        path unchanged if given a single-part path. *)
    val all_but_top_dir_defaulting_to_self : t @ l -> t @ l
    [@@alloc a]

    (** Like [Option.both (top_dir t) (all_but_top_dir t)]. Allocates [Some] at most once. *)
    val top_dir_and_all_but_top_dir : t @ l -> (Part.t * t) option @ l
    [@@alloc a]

    (** Adds the given string as a suffix of the path's basename. Raises if the string
        contains characters that are illegal for a path part. *)
    val append_to_basename_exn : t @ local -> string @ local -> t @ l
    [@@alloc a]

    (** Adds a part to the beginning of the path. *)
    val prepend_part : Part.t @ local -> t @ local -> t @ l
    [@@alloc a]

    (** Adds a part to the end of the path. *)
    val append_part : t @ local -> Part.t @ local -> t @ l
    [@@alloc a]

    (** Appends the parts of two paths. *)
    val append : t @ local -> t @ local -> t @ l
    [@@alloc a]

    (** Returns all parts of the given path after [prefix], or [None] if [prefix] is not a
        prefix of the path's parts. If the path equals [prefix], returns [dot]. *)
    val chop_prefix : t @ l -> prefix:t @ local -> t option @ l
    [@@alloc a]

    (** Returns all parts of the given path after [prefix], or raises if [prefix] is not a
        prefix of the path's parts. If the path equals [prefix], returns [dot]. *)
    val chop_prefix_exn : t @ l -> prefix:t @ local -> t @ l
    [@@alloc a]

    (** Returns all parts of the given path after [prefix], or returns an error if
        [prefix] is not a prefix of the path's parts. If the path equals [prefix], returns
        [dot]. *)
    val chop_prefix_or_error : t @ l -> prefix:t @ local -> t Or_error.t @ l
    [@@alloc a]

    (** Returns all parts of the given path after [prefix], or returns the path unchanged
        if [prefix] is not a prefix of the path's parts. If the path equals [prefix],
        returns [dot]. *)
    val chop_prefix_if_exists : t @ l -> prefix:t @ local -> t @ l
    [@@alloc a]

    (** Returns all parts of the given path before [suffix], or [None] if [suffix] is not
        a suffix of the path's parts. If the path equals [suffix], returns [dot]. *)
    val chop_suffix : t @ l -> suffix:t @ local -> t option @ l
    [@@alloc a]

    (** Returns all parts of the given path before [suffix], or raises if [suffix] is not
        a suffix of the path's parts. If the path equals [suffix], returns [dot]. *)
    val chop_suffix_exn : t @ l -> suffix:t @ local -> t @ l
    [@@alloc a]

    (** Returns all parts of the given path before [suffix], or returns an error if
        [suffix] is not a suffix of the path's parts. If the path equals [suffix], returns
        [dot]. *)
    val chop_suffix_or_error : t @ l -> suffix:t @ local -> t Or_error.t @ l
    [@@alloc a]

    (** Returns all parts of the given path before [suffix], or returns the path unchanged
        if [suffix] is not a suffix of the path's parts. If the path equals [suffix],
        returns [dot]. *)
    val chop_suffix_if_exists : t @ l -> suffix:t @ local -> t @ l
    [@@alloc a]

    (** Removes [.] parts from the given path. Returns [.] if the given path consists only
        of one or more [.] parts. *)
    val simplify_dot : t @ l -> t @ l
    [@@alloc a]

    (** Removes [.] parts from the given path. Cancels out [..] parts with preceding parts
        (that are neither [.] nor [..]). Does not check the file system; in the presence
        of symlinks, the resulting path may not be equivalent. Returns [.] if all parts
        are canceled out. *)
    val simplify_dot_and_dot_dot_naively : t @ l -> t @ l
    [@@alloc a]

    (** Returns a path consisting of the single given part. *)
    val of_part : Part.t @ l -> t @ l
    [@@mode l]

    (** Produces the parts of the path. *)
    val to_parts : t @ l -> Part.t list @ l
    [@@alloc a]

    (** Produces the parts of the path. *)
    val to_parts_nonempty : t @ l -> Part.t Nonempty_list.t @ l
    [@@alloc a]

    (** Returns a relative path consisting of the given one or more parts, or [None] if
        the list of parts is empty. *)
    val of_parts : Part.t list @ l -> t option @ l
    [@@alloc a]

    (** Returns a relative path consisting of the given one or more parts, or raises if
        the list of parts is empty. *)
    val of_parts_exn : Part.t list @ l -> t @ l
    [@@alloc a]

    (** Returns a relative path consisting of the given one or more parts, or returns an
        error if the list of parts is empty. *)
    val of_parts_or_error : Part.t list @ l -> t Or_error.t @ l
    [@@alloc a]

    (** Returns a relative path consisting of the given one or more parts, or [dot] if the
        list of parts is empty. *)
    val of_parts_defaulting_to_dot : Part.t list @ l -> t @ l
    [@@alloc a]

    (** Returns a relative path consisting of the given one or more parts. *)
    val of_parts_nonempty : Part.t Nonempty_list.t @ l -> t @ l
    [@@alloc a]]
  end
end

module type Relative = sig @@ portable
  include module type of struct
    include Definitions
  end

  include S with module Types := Types
  module Stable : Common.Stable with module Type := Types.Relative
end
