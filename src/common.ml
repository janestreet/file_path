open! Core
open Stable_witness.Export
include Common_intf

module Make (T : Types.Type) (Basis : Basis) = struct
  module Stable = struct
    (* Our stable serializations are not straightforward, as they parse and canonicalize
       strings. Rather than opening only [Core.Core_stable], we use as much of [Core] as
       we need, and make sure to write thorough tests for the stable serializations. *)

    module V1 = struct
      include T

      let[@cold] raise_invalid ~name string =
        raise_s
          [%sexp
            (Printf.sprintf "%s.%s: invalid string" Basis.module_name name : string)
            , (string : string)]
      ;;

      let[@cold] raise_non_canonical ~name string =
        raise_s
          [%sexp
            (Printf.sprintf "%s.%s: non-canonical representation" Basis.module_name name
             : string)
            , (string : string)]
      ;;

      let of_string string =
        if not (Basis.is_valid string)
        then raise_invalid ~name:"of_string" string
        else if not (Basis.is_canonical string)
        then Expert.unchecked_of_canonical_string (Basis.canonicalize string)
        else Expert.unchecked_of_canonical_string string
      ;;

      let invariant t =
        let string = to_string t in
        if not (Basis.is_valid string)
        then raise_invalid ~name:"invariant" string
        else if not (Basis.is_canonical string)
        then raise_non_canonical ~name:"invariant" string
      ;;

      include Binable.Of_stringable_with_uuid [@mode portable] (struct
          type nonrec t = t

          let of_string = of_string
          let to_string = to_string
          let caller_identity = Basis.caller_identity
        end)

      include Sexpable.Of_stringable [@mode portable] (struct
          type nonrec t = t

          let of_string = of_string
          let to_string = to_string
        end)

      (* Path types are serialized using [of_string/to_string], and derive their stability
         from the fact that string is a primitive type. *)
      let stable_witness =
        Stable_witness.of_serializable [%stable_witness: string] of_string to_string
      ;;

      include Identifiable.Make_using_comparator [@mode portable] (struct
          type nonrec t = t [@@deriving bin_io, compare ~localize, hash, sexp]
          type nonrec comparator_witness = comparator_witness

          let of_string = of_string
          let to_string = to_string
          let comparator = comparator
          let module_name = Basis.module_name
        end)

      (* Include [T] again to make sure we export the fast versions of any underlying
         operations. *)
      include T
    end
  end

  include Stable.V1

  let arg_type =
    (Command.Arg_type.create [@mode portable])
      of_string
      ~complete:(fun (_ : Univ_map.t) ~part ->
        try Basis.autocomplete part with
        | (_ : exn)
        (* don't mask exceptions during inline tests *)
          when not Ppx_inline_test_lib.am_running -> [])
  ;;
end
