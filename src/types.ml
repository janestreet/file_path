open! Core
include Types_intf

module T = struct
  type t = string [@@deriving equal ~localize, hash, sexp_of, sexp_grammar]

  let to_string = Fn.id
  let%template[@mode m = (global, local)] compare = (Path_string.compare [@mode m])

  include (val (Comparator.make [@mode portable]) ~compare ~sexp_of_t)

  module Expert = struct
    let unchecked_of_canonical_string = Fn.id
  end
end

module Path = T
module Absolute = T
module Relative = T
module Part = T
