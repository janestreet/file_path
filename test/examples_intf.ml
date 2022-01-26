(** This file constructs examples for expect test coverage. In [Helpers], we supplement
    these examples with quickcheck tests. Having manually-constructed examples gives us
    some idea of what cases we have covered, and gives us a deterministic set of inputs
    for which the expect tests can print outputs. *)

open! Core

(** Inputs to functions like [Path.is_prefix]. *)
module With_prefix = struct
  type 'a t =
    { t : 'a
    ; prefix : 'a
    }
  [@@deriving compare, equal, quickcheck, sexp_of]
end

(** Inputs to functions like [Path.chop_suffix]. *)
module With_suffix = struct
  type 'a t =
    { t : 'a
    ; suffix : File_path.Relative.t
    }
  [@@deriving compare, equal, quickcheck, sexp_of]
end

(** All modules need these examples. *)
module type Common = sig
  type t

  val strings_for_of_string : string list
  val for_append_to_basename : (t * string) list
  val for_compare : t list
  val for_conversion : t list
end

(** We need examples of lists of parts. *)
module type Part = sig
  include Common

  val lists_for_conversion : t list list
end

(** Every compound path type needs these examples. *)
module type Compound = sig
  include Common

  val for_basename_and_dirname : t list
  val for_append_part : (t * File_path.Part.t) list
  val for_chop_prefix : t With_prefix.t list
  val for_chop_suffix : t With_suffix.t list
  val for_append : (t * File_path.Relative.t) list
  val for_simplify : t list
end

(** Absolute paths have no additional examples. *)
module type Absolute = sig
  include Compound
end

(** Relative paths support top directory access and prepending. *)
module type Relative = sig
  include Compound

  val for_top_dir : t list
  val for_prepend_part : (File_path.Part.t * t) list
end

(** Generic paths need examples for absolute/relative conversions. *)
module type Path = sig
  include Compound

  val for_make_absolute : (t * File_path.Absolute.t) list
  val for_make_relative : (t * File_path.Absolute.t) list
  val variant_for_conversion : File_path.Variant.t list
end

module type Examples = sig
  module With_prefix : sig
    include module type of struct
      include With_prefix
    end

    include Invariant.S1 with type 'a t := 'a t
  end

  module With_suffix : sig
    include module type of struct
      include With_suffix
    end

    include Invariant.S1 with type 'a t := 'a t
  end

  module Part : Part with type t := File_path.Part.t
  module Relative : Relative with type t := File_path.Relative.t
  module Absolute : Absolute with type t := File_path.Absolute.t
  include Path with type t := File_path.t
end
