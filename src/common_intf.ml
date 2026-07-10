open! Core

module Definitions = struct
  (** Common functionality shared by path types. *)
  module type S = sig @@ portable
    module Type : Types.Type

    type t = Type.t
    type comparator_witness = Type.comparator_witness

    include Types.Type with type t := t and type comparator_witness := comparator_witness

    (** Path types have straightforward [to_string] and [sexp_of_t] behavior. [of_string]
        and [t_of_sexp] raise on invalid input (e.g. containing null characters), and
        guarantee canonical output (e.g. no redundant slashes).

        [to_string] is the identity function. [of_string] returns its input when the input
        is a valid string in canonical form. *)
    include%template
      Identifiable.S
      [@alloc stack] [@mode local portable]
      with type t := t
       and type comparator_witness := comparator_witness

    val%template of_string : string @ l -> t @ l
    [@@alloc __ @ l = (stack_local, heap_global)]

    include Invariant.S with type t := t
    include Quickcheckable.S with type t := t

    (** Command-line argument. Supports tab-completion. *)
    val arg_type : t Command.Arg_type.t
  end

  (** Stable path type serialization includes bin-io and sexp serialization, along with
      stable set, map, hash table, and hash set serializations. *)
  module type Version = sig @@ portable
    type t
    [@@deriving equal ~localize, compare ~localize, hash, sexp_grammar, stable_witness]

    include Stable_comparable.V1 with type t := t
    include Hashable.Stable.V1.S with type key := t
  end

  module type Stable = sig @@ portable
    module Type : Types.Type

    module V1 :
      Version with type t = Type.t and type comparator_witness = Type.comparator_witness
  end

  (** For internal use. Must be implemented to define a path type. *)
  module type Basis = sig @@ portable
    val module_name : string
    val caller_identity : Bin_prot.Shape.Uuid.t
    val is_valid : string @ local -> bool
    val is_canonical : string @ local -> bool

    val%template canonicalize : string @ l -> string @ l
    [@@alloc __ @ l = (stack_local, heap_global)]

    val autocomplete : string -> string list

    module Quickcheckable_string : Quickcheckable.S with type t = string
  end
end

module type Common = sig @@ portable
  include module type of struct
    include Definitions
  end

  (** For internal use. Defines new path types. *)
  module Make (T : Types.Type) (Basis : Basis) : sig @@ portable
    include S with module Type := T
    module Stable : Stable with module Type := T
  end
end
