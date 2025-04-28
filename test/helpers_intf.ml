(** We codify several testing patterns here to make [File_path] testing rigorous. Every
    expect test in this library should call one of these helpers. *)

open! Core

(** An arbitrary input type used by a test. *)
module type Type = sig
  type t [@@deriving compare, equal, quickcheck, sexp_of]

  include Invariant.S with type t := t
end

(** A path type (e.g. File_path.Part.t, or File_path.t). *)
module type Path_type = sig
  type t [@@deriving quickcheck]

  include Identifiable.S with type t := t
  include Invariant.S with type t := t

  module Expert : sig
    val unchecked_of_canonical_string : string -> t
  end
end

(** One version of stable serializations for a path type, including containers. *)
module type Version = sig
  type t [@@deriving hash]

  include Stable with type t := t
  include Hashable.Stable.V1.S with type key := t

  include
    Comparable.Stable.V1.S
    with type comparable := t
     and type comparator_witness := comparator_witness
end

module type Helpers = sig
  module type Type = Type
  module type Path_type = Path_type

  (** Tests the given list of constants. Prints them and tests that they pass the path
      type's invariant. *)
  val test_constants : (module Path_type with type t = 'a) -> 'a list -> unit

  (** Tests the type's comparison. Makes sure sorting a list is consistent regardless of
      input order. *)
  val test_compare : (module Path_type with type t = 'a) -> 'a list -> unit

  (** Tests string conversions for a path type. Makes sure they round-trip, satisfy
      invariants, raise appropriately, and only allocate when necessary. *)
  val test_of_string : (module Path_type) -> string list -> unit

  (** Tests the invariant, unchecked "expert" construction, and consistency with
      [of_string]'s raising / canonicalizing behavior. *)
  val test_invariant : (module Path_type) -> string list -> unit

  (** Makes sure container types are constructed appropriately. (e.g., not just
      [module Map = String.Map]). *)
  val test_containers : (module Path_type with type t = 'a) -> 'a list -> unit

  (** Tests a boolean function. Prints [true] and [false] examples. Makes sure both cases
      are covered. Tests [correctness] for each input. *)
  val test_predicate
    :  input:(module Type with type t = 'input)
    -> examples:'input list
    -> correctness:('input -> bool -> unit)
    -> ('input -> bool)
    -> unit

  (** Tests a generic function with the given input and output types. Tests [correctness]
      for each input. *)
  val test
    :  input:(module Type with type t = 'input)
    -> output:(module Type with type t = 'output)
    -> examples:'input list
    -> correctness:('input -> 'output -> unit)
    -> ('input -> 'output)
    -> unit

  (** All tests sharing a [Bin_shape_universe.t] must have unique bin_shape digests.

      Even if not referred to explicitly by clients, this is exported to clarify the
      behavior of [test_stable*] below. *)
  module Bin_shape_universe : sig
    type t

    (** Creates a fresh [t]. *)
    val create : unit -> t

    (** A single shared [t], allocated on demand. *)
    val default : t Lazy.t
  end

  (** Tests stable serializations and round-trips for the given examples, and uniqueness
      of bin-shape digests. *)
  val test_stable_version
    :  ?bin_shape_universe:Bin_shape_universe.t
         (** defaults to [force Bin_shape_universe.default] *)
    -> Source_code_position.t
    -> (module Version with type t = 'a)
    -> 'a list
    -> unit

  (** Tests stable serializations for containers with the given examples, and uniqueness
      of bin-shape digests (instantiated at [int] for polymorphic types). *)
  val test_stable_containers
    :  ?bin_shape_universe:Bin_shape_universe.t
         (** defaults to [force Bin_shape_universe.default] *)
    -> Source_code_position.t
    -> (module Version with type t = 'a)
    -> 'a list
    -> unit

  (** Functors wrapping the [Type] signature in polymorphic types. *)

  module Option (Type : Type) : Type with type t = Type.t option
  module List (Type : Type) : Type with type t = Type.t list
  module Nonempty_list (Type : Type) : Type with type t = Type.t Nonempty_list.t
  module Or_error (Type : Type) : Type with type t = Type.t Or_error.t
  module Tuple2 (A : Type) (B : Type) : Type with type t = A.t * B.t
  module With_prefix (Type : Type) : Type with type t = Type.t Examples.With_prefix.t
  module With_suffix (Type : Type) : Type with type t = Type.t Examples.With_suffix.t
end
