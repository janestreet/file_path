open! Core

module Definitions = struct
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
  module type%template [@alloc a @ l = (stack_local, heap_global)] S = sig @@ portable
    module Types : Types.S
    open Types

    (** Prefix operators: conversions

        These use two characters indicating the type of path:
        1. absolute, relative, or either
        2. compound or single-part

        If constructing a path, that is all.

        If consuming a path, they use a third character indicating the return type. *)

    (** [of_string] synonyms: just indicate the path type being constructed *)

    val ( ~/ ) : string @ l -> Relative.t @ l
    val ( !/ ) : string @ l -> Absolute.t @ l
    val ( ?/ ) : string @ l -> Path.t @ l
    val ( ~. ) : string @ l -> Part.t @ l

    (** [to_string] synonyms: end in '$' for a string return type *)

    val ( ~/$ ) : Relative.t @ l -> string @ l
    val ( !/$ ) : Absolute.t @ l -> string @ l
    val ( ?/$ ) : Path.t @ l -> string @ l
    val ( ~.$ ) : Part.t @ l -> string @ l

    (** Up-conversions: end in '?' or '~' to indicate return type *)

    val ( !/? ) : Absolute.t @ l -> Path.t @ l
    val ( ~/? ) : Relative.t @ l -> Path.t @ l
    val ( ~.? ) : Part.t @ l -> Path.t @ l
    val ( ~.~ ) : Part.t @ l -> Relative.t @ l

    (** Infix operators: [append*] synonyms

        These use three characters:
        1. '/' indicating some kind of path concatenation
        2. type of left argument and result
        3. type of right argument and kind of concatenation

        These operators are left-associative and all share the same precedence. *)

    (** [append] synonyms: end in '/' for a compound right argument *)

    val ( /~/ ) : Relative.t @ local -> Relative.t @ local -> Relative.t @ l
    val ( /!/ ) : Absolute.t @ local -> Relative.t @ local -> Absolute.t @ l
    val ( /?/ ) : Path.t @ local -> Relative.t @ local -> Path.t @ l

    (** [append_part] synonyms: end in '.' for a single-part right argument *)

    val ( /~. ) : Relative.t @ local -> Part.t @ local -> Relative.t @ l
    val ( /!. ) : Absolute.t @ local -> Part.t @ local -> Absolute.t @ l
    val ( /?. ) : Path.t @ local -> Part.t @ local -> Path.t @ l

    (** [append_to_basename_exn] synonyms: end in '^' for string concatenation *)

    val ( /~^ ) : Relative.t @ local -> string @ local -> Relative.t @ l
    val ( /!^ ) : Absolute.t @ local -> string @ local -> Absolute.t @ l
    val ( /?^ ) : Path.t @ local -> string @ local -> Path.t @ l
    val ( /.^ ) : Part.t @ local -> string @ local -> Part.t @ l
  end
end

module type Operators = sig @@ portable
  include module type of struct
    include Definitions
  end

  module%template [@alloc a = (stack, heap)] O : S [@alloc a] with module Types := Types
end
