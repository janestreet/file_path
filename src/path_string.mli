@@ portable

(** This module is for internal use only.

    It defines the shared logic for manipulating compound path strings used by [Path],
    [Relative], and [Absolute]. The string operations are defined generically for both
    relative and absolute paths where appropriate, so all three modules can call them. *)

open! Core

(** The root directory. *)
val root : string

(** The name for the current directory. *)
val dot : string

(** The name for the parent directory. *)
val dot_dot : string

(** Reports if a string is a valid path, i.e. non-empty and no null characters. *)
val is_valid : string -> bool

(** Reports if a string is a canonical path, i.e. no redundant or trailing slashes (except
    for the root path). *)
val is_canonical : string -> bool

(** Produces the canonical string for a path by eliminating redundant and trailing
    slashes. *)
val canonicalize : string -> string

(** Compares two paths lexicographically as lists of parts. *)
val%template compare : string @ m -> string @ m -> int
[@@mode m = (global, local)]

(** Reports if a path is absolute, i.e. starts with a slash. *)
val is_absolute : string -> bool

(** Reports if a path is relative, i.e. does not start with a slash. *)
val is_relative : string -> bool

(** Appends two paths, adding a slash between them (unless the first path is the root
    path). Asserts that the second path is relative. *)
val append : string -> string -> string

(** Appends a string suffix to a path. *)
val append_to_basename
  :  string
  -> suffix:string
  -> if_valid:(string -> 'a)
  -> if_invalid_path:(string -> suffix:string -> 'a)
  -> if_invalid_suffix:(string -> suffix:string -> 'a)
  -> 'a

(** Reports if the parts of [prefix] are a non-strict prefix of the parts of the other
    argument, and the paths are both absolute or both relative. *)
val is_prefix : string -> prefix:string -> bool

(** Reports if the parts of [suffix] are a non-strict suffix of the parts of the other
    argument. Asserts that [suffix] is relative. *)
val is_suffix : string -> suffix:string -> bool

(** Produces the parts of the path. *)
val to_parts : string -> part_of_string:(string -> 'part) -> 'part list

(** Constructs an absolute path from the given parts. *)
val of_parts_absolute : string list -> string

(** Equivalent to [List.length (to_parts string)], without allocating. *)
val number_of_parts : string -> int

(** Removes [.] parts from the given path. Returns [.] if the given path is a relative
    path consisting only of one or more [.] parts. *)
val simplify_dot : string -> string

(** Removes [.] parts from the given path and cancels out [..] parts with preceding parts
    (that are neither [.] nor [..]). Does not check the file system; in the presence of
    symlinks, the resulting path may not be equivalent. Returns [.] if the given path is a
    relative path and all parts are canceled out. *)
val simplify_dot_and_dot_dot_naively : string -> string

(** The functions below all take callbacks so that success and failure can be
    distinguished without unnecessary allocation. In the success case, the given string
    represents the result. In the failure case, the given string(s) are the original
    input, passed so that the callback does not need to be an allocated closure. *)

(** Calls [if_some] with the final part of the given path, or [if_none] if given the root
    path. *)
val basename : string -> if_some:(string -> 'a) -> if_none:(string -> 'a) -> 'a

(** Calls [if_some] with all parts of the given path but the final one, or [if_none] if
    given the root path or a relative path of a single part. *)
val dirname : string -> if_some:(string -> 'a) -> if_none:(string -> 'a) -> 'a

(** Where [dirname] and [basename] would both call [if_some], calls [if_some] with both
    arguments. Otherwise calls [if_none]. *)
val dirname_and_basename
  :  string
  -> if_some:(dirname:string -> basename:string -> 'a)
  -> if_none:(string -> 'a)
  -> 'a

(** Calls [if_some] with the first part of a multiple-part relative path, or [if_none] if
    given a single-part path. Asserts that the path is relative. *)
val top_dir : string -> if_some:(string -> 'a) -> if_none:(string -> 'a) -> 'a

(** Calls [if_some] with all but the first part of a multiple-part relative path, or
    [if_none] if given a single-part path. Asserts that the path is relative. *)
val all_but_top_dir : string -> if_some:(string -> 'a) -> if_none:(string -> 'a) -> 'a

(** Where [top_dir] and [all_but_top_dir] would both call [if_some], calls [if_some] with
    both arguments. Otherwise calls [if_none]. *)
val top_dir_and_all_but_top_dir
  :  string
  -> if_some:(top_dir:string -> all_but_top_dir:string -> 'a)
  -> if_none:(string -> 'a)
  -> 'a

(** Calls [if_some] with all parts of the given path after [prefix], or [if_none] if
    [prefix] is not a prefix of the path's parts. Always calls [if_none] if the path and
    [prefix] do not match in absolute/relative status. If the path equals [prefix], calls
    [if_some "."] *)
val chop_prefix
  :  string
  -> prefix:string
  -> if_some:(string -> 'a)
  -> if_none:(string -> prefix:string -> 'a)
  -> 'a

(** Calls [if_some] with all parts of the given path before [suffix], or [if_none] if
    [suffix] is not a suffix of the path's parts. Asserts that suffix is relative. If the
    path equals [suffix], calls [if_some "."]. *)
val chop_suffix
  :  string
  -> suffix:string
  -> if_some:(string -> 'a)
  -> if_none:(string -> suffix:string -> 'a)
  -> 'a

(** Calls [if_some] with a relative path consisting of the given one or more parts, or
    [if_none] if the list of parts is empty. *)
val of_parts_relative
  :  string list
  -> if_some:(string -> 'a)
  -> if_none:(unit -> 'a)
  -> 'a

module Quickcheckable_part : Quickcheckable.S with type t = string
module Quickcheckable_relative : Quickcheckable.S with type t = string
module Quickcheckable_absolute : Quickcheckable.S with type t = string
module Quickcheckable_path : Quickcheckable.S with type t = string
