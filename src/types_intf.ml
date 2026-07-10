open! Core

module Definitions = struct
  module type Type = sig @@ portable
    (** Path types are represented as strings. *)
    type t = private string
    [@@deriving
      equal ~localize, compare ~localize, globalize, hash, sexp_of ~stackify, sexp_grammar]

    include%template Comparator.S [@mode portable] with type t := t

    (** Equivalent to [(t :> string)]. *)
    val%template to_string : t @ m -> string @ m
    [@@alloc __ @ m = (stack_local, heap_global)]

    module Expert : sig
      (** Used internally for performance purposes, this converts a valid string in
          canonical form for the given path type into a [t]. Calling this on invalid or
          non-canonical strings is safe (i.e. should not cause segfaults) but path
          accessors may return nonsense values. *)
      val%template unchecked_of_canonical_string : string @ m -> t @ m
      [@@alloc __ @ m = (stack_local, heap_global)]
    end
  end

  module type S = sig @@ portable
    (** A path is a string. *)
    module Path : Type with type t = private string

    (** An absolute path is a path. *)
    module Absolute : Type with type t = private Path.t

    (** A relative path is a path. *)
    module Relative : Type with type t = private Path.t

    (** A path part is a relative path. *)
    module Part : Type with type t = private Relative.t
  end
end

module type Types = sig @@ portable
  include module type of struct
    include Definitions
  end

  include S
end
