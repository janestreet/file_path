open! Core
include Part_intf

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

      let is_valid string =
        (not (String.is_empty string)) && String.for_all string ~f:char_is_valid
      ;;

      let is_canonical (_ : string) = true
      let canonicalize = Fn.id
      let autocomplete = Completion.complete_part
    end)

let dot = Expert.unchecked_of_canonical_string Path_string.dot
let dot_dot = Expert.unchecked_of_canonical_string Path_string.dot_dot

let append_to_basename_exn path suffix =
  Path_string.append_to_basename
    (to_string path)
    ~suffix
    ~if_valid:Expert.unchecked_of_canonical_string
    ~if_invalid_path:(fun _ ~suffix:_ -> (* root path is not a part *) assert false)
    ~if_invalid_suffix:(fun path ~suffix ->
      raise_s
        [%sexp
          "File_path.Part.append_to_basename_exn: suffix contains invalid characters"
          , { path : string; suffix : string }])
;;

include
  Quickcheckable.Of_quickcheckable [@mode portable]
    (Path_string.Quickcheckable_part)
    (struct
      type nonrec t = t

      let of_quickcheckable = Expert.unchecked_of_canonical_string
      let to_quickcheckable = to_string
    end)
