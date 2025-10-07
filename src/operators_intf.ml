open! Core

(** Infix and prefix operators for path operations.

    All operators are synonyms for functions exported elsewhere in this library via
    ordinary identifiers.

    We indicate the different type of paths with characters. The first three are valid
    prefix operator characters.

    - '~' means a relative path with no leading slash
    - '!' means an absolute path with a leading slash
    - '?' means a path that may be relative or absolute
    - '$' means a string

    - '/' means a compound path that may contain slashes, or path concatenation with a
      slash
    - '.' means a path part with no slashes
    - '^' means concatenation with an ordinary string that does not represent a path *)
module type S = sig @@ portable
  module Types : Types.S
  open Types

  (** Prefix operators: conversions

      These use two characters indicating the type of path:
      1. absolute, relative, or either
      2. compound or single-part

      If constructing a path, that is all.

      If consuming a path, they use a third character indicating the return type. *)

  (** [of_string] synonyms: just indicate the path type being constructed *)

  val ( ~/ ) : string -> Relative.t
  val ( !/ ) : string -> Absolute.t
  val ( ?/ ) : string -> Path.t
  val ( ~. ) : string -> Part.t

  (** [to_string] synonyms: end in '$' for a string return type *)

  val ( ~/$ ) : Relative.t -> string
  val ( !/$ ) : Absolute.t -> string
  val ( ?/$ ) : Path.t -> string
  val ( ~.$ ) : Part.t -> string

  (** Up-conversions: end in '?' or '~' to indicate return type *)

  val ( !/? ) : Absolute.t -> Path.t
  val ( ~/? ) : Relative.t -> Path.t
  val ( ~.? ) : Part.t -> Path.t
  val ( ~.~ ) : Part.t -> Relative.t

  (** Infix operators: [append*] synonyms

      These use three characters:
      1. '/' indicating some kind of path concatenation
      2. type of left argument and result
      3. type of right argument and kind of concatenation

      These operators are left-associative and all share the same precedence. *)

  (** [append] synonyms: end in '/' for a compound right argument *)

  val ( /~/ ) : Relative.t -> Relative.t -> Relative.t
  val ( /!/ ) : Absolute.t -> Relative.t -> Absolute.t
  val ( /?/ ) : Path.t -> Relative.t -> Path.t

  (** [append_part] synonyms: end in '.' for a single-part right argument *)

  val ( /~. ) : Relative.t -> Part.t -> Relative.t
  val ( /!. ) : Absolute.t -> Part.t -> Absolute.t
  val ( /?. ) : Path.t -> Part.t -> Path.t

  (** [append_to_basename_exn] synonyms: end in '^' for string concatenation *)

  val ( /~^ ) : Relative.t -> string -> Relative.t
  val ( /!^ ) : Absolute.t -> string -> Absolute.t
  val ( /?^ ) : Path.t -> string -> Path.t
  val ( /.^ ) : Part.t -> string -> Part.t
end

module type Operators = sig @@ portable
  module type S = S

  include S with module Types := Types
end
