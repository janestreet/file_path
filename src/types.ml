open! Core
include Types_intf.Definitions

module T = struct
  type t = string
  [@@deriving equal ~localize, globalize, hash, sexp_of ~stackify, sexp_grammar]

  let%template to_string t = t [@exclave_if_stack a] [@@alloc a = (stack, heap)]
  let%template[@mode m = (global, local)] compare = (Path_string.compare [@mode m])

  include%template (val (Comparator.make [@mode portable]) ~compare ~sexp_of_t)

  module Expert = struct
    let%template unchecked_of_canonical_string t = t [@exclave_if_stack a]
    [@@alloc a = (stack, heap)]
    ;;
  end
end

module Path = T
module Absolute = T
module Relative = T
module Part = T
