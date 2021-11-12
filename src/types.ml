open! Core
include Types_intf

module T = struct
  type t = string [@@deriving equal, hash, sexp_of, sexp_grammar]

  let to_string = Fn.id
  let compare = Path_string.compare

  include (val Comparator.make ~compare ~sexp_of_t)

  module Expert = struct
    let unchecked_of_canonical_string = Fn.id
  end
end

module Path = T
module Absolute = T
module Relative = T
module Part = T
