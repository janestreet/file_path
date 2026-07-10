open! Core
include Operators_intf.Definitions

(* The operators are synonyms for existing functions. No actual code should go in this
   file, all operators should also be exported by full names. *)

module%template [@alloc a @ l = (stack_local, heap_global)] O = struct
  let ( ~/ ) = (Relative.of_string [@alloc a])
  let ( !/ ) = (Absolute.of_string [@alloc a])
  let ( ?/ ) = (Path.of_string [@alloc a])
  let ( ~. ) = (Part.of_string [@alloc a])
  let ( !/$ ) = (Absolute.to_string [@alloc a])
  let ( ~/$ ) = (Relative.to_string [@alloc a])
  let ( ?/$ ) = (Path.to_string [@alloc a])
  let ( ~.$ ) = (Part.to_string [@alloc a])
  let ( !/? ) = (Path.of_absolute [@mode l])
  let ( ~/? ) = (Path.of_relative [@mode l])
  let ( ~.? ) = (Path.of_part_relative [@mode l])
  let ( ~.~ ) = (Relative.of_part [@mode l])
  let ( /~/ ) = (Relative.append [@alloc a])
  let ( /!/ ) = (Absolute.append [@alloc a])
  let ( /?/ ) = (Path.append [@alloc a])
  let ( /~. ) = (Relative.append_part [@alloc a])
  let ( /!. ) = (Absolute.append_part [@alloc a])
  let ( /?. ) = (Path.append_part [@alloc a])
  let ( /~^ ) = (Relative.append_to_basename_exn [@alloc a])
  let ( /!^ ) = (Absolute.append_to_basename_exn [@alloc a])
  let ( /?^ ) = (Path.append_to_basename_exn [@alloc a])
  let ( /.^ ) = (Part.append_to_basename_exn [@alloc a])
end
