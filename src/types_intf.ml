open! Core

module type Type = sig @@ portable
  (** Path types are represented as strings. *)
  type t = private string
  [@@deriving equal ~localize, compare ~localize, hash, sexp_of, sexp_grammar]

  include Comparator.S [@mode portable] with type t := t

  (** Equivalent to [(t :> string)]. *)
  val to_string : t -> string

  module Expert : sig
    (** Used internally for performance purposes, this converts a valid string in
        canonical form for the given path type into a [t]. Calling this on invalid or
        non-canonical strings is safe (i.e. should not cause segfaults) but path accessors
        may return nonsense values. *)
    val unchecked_of_canonical_string : string -> t
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

module type Types = sig @@ portable
  module type Type = Type
  module type S = S

  include S
end
