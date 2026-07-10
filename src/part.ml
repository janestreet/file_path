open! Core
include Part_intf.Definitions

let char_is_valid = function
  | '/' | '\000' -> false
  | _ -> true
;;

include
  Common.Make
    (Types.Part)
    (struct
      let module_name = "File_path.Part"

      let caller_identity =
        Bin_prot.Shape.Uuid.of_string "ae95a826-e43c-43fc-8605-a0faceffdc20"
      ;;

      let%template is_valid string =
        (not (String.is_empty string))
        && (String.for_all [@mode local]) string ~f:char_is_valid
      ;;

      let is_canonical (_ : string) = true
      let%template[@alloc a = (stack, heap)] canonicalize = Fn.id
      let autocomplete = Completion.complete_part

      module Quickcheckable_string = Path_string.Quickcheckable_part
    end)

let dot = Expert.unchecked_of_canonical_string Path_string.dot
let dot_dot = Expert.unchecked_of_canonical_string Path_string.dot_dot

[%%template
[@@@alloc a @ l = (stack_local, heap_global)]

let[@alloc a] append_to_basename_exn path suffix =
  (Path_string.append_to_basename [@alloc a])
    ((to_string [@alloc stack]) path)
    ~suffix
    ~if_valid:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_invalid_path:(fun _ ~suffix:_ -> (* root path is not a part *) assert false)
    ~if_invalid_suffix:(fun path ~suffix ->
      raise_s
        [%sexp
          "File_path.Part.append_to_basename_exn: suffix contains invalid characters"
          , { path = (globalize_string path : string)
            ; suffix = (globalize_string suffix : string)
            }]) [@nontail] [@exclave_if_stack a]
;;]
