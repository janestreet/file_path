open! Core
include Absolute_intf.Definitions

include
  Common.Make
    (Types.Absolute)
    (struct
      let module_name = "File_path.Absolute"

      let caller_identity =
        Bin_prot.Shape.Uuid.of_string "da1084bb-65fc-4c6f-9f0c-72ef5d47adde"
      ;;

      let is_valid string = Path_string.is_absolute string && Path_string.is_valid string
      let is_canonical = Path_string.is_canonical

      let%template canonicalize = (Path_string.canonicalize [@alloc a])
      [@@alloc a = (stack, heap)]
      ;;

      let autocomplete = Completion.complete_absolute

      module Quickcheckable_string = Path_string.Quickcheckable_absolute
    end)

let root = Expert.unchecked_of_canonical_string Path_string.root
let default_to_root _ = root

[%%template
let is_prefix t ~prefix =
  Path_string.is_prefix
    ((to_string [@alloc stack]) t)
    ~prefix:((to_string [@alloc stack]) prefix) [@nontail]
;;

let is_suffix t ~suffix =
  Path_string.is_suffix
    ((to_string [@alloc stack]) t)
    ~suffix:(Relative.(to_string [@alloc stack]) suffix) [@nontail]
;;

let number_of_parts t =
  Path_string.number_of_parts ((to_string [@alloc stack]) t) [@nontail]
;;

[@@@alloc a @ l = (stack_local, heap_global)]

let[@alloc a] basename t =
  (Path_string.basename [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(fun string ->
      Some ((Part.Expert.unchecked_of_canonical_string [@alloc a]) string)
      [@exclave_if_stack a])
    ~if_none:(fun _ -> None) [@exclave_if_stack a]
;;

let[@alloc a] basename_exn t =
  (Path_string.basename [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(Part.Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(function
    | "/" -> raise_s [%sexp "File_path.Absolute.basename_exn: root path"]
    | string ->
      raise_s
        [%sexp
          "File_path.Absolute.basename_exn: path contains no slash"
          , (globalize_string string : string)])
    [@exclave_if_stack a]
;;

let[@alloc a] basename_or_error t =
  (Path_string.basename [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(fun string ->
      Ok ((Part.Expert.unchecked_of_canonical_string [@alloc a]) string)
      [@exclave_if_stack a])
    ~if_none:(function
      | "/" -> Or_error.error_s [%sexp "File_path.Absolute.basename_or_error: root path"]
      | string ->
        Or_error.error_s
          [%sexp
            "File_path.Absolute.basename_or_error: path contains no slash"
            , (globalize_string string : string)]) [@exclave_if_stack a]
;;

let[@alloc a] basename_defaulting_to_dot t =
  (Path_string.basename [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(Part.Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(fun _ -> Part.dot)
  [@exclave_if_stack a]
;;

let[@alloc a] dirname t =
  (Path_string.dirname [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(fun string ->
      Some ((Expert.unchecked_of_canonical_string [@alloc a]) string)
      [@exclave_if_stack a])
    ~if_none:(fun _ -> None) [@exclave_if_stack a]
;;

let[@alloc a] dirname_exn t =
  (Path_string.dirname [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(function
    | "/" -> raise_s [%sexp "File_path.Absolute.dirname_exn: root path"]
    | string ->
      raise_s
        [%sexp
          "File_path.Absolute.dirname_exn: path contains no slash"
          , (globalize_string string : string)])
    [@exclave_if_stack a]
;;

let[@alloc a] dirname_or_error t =
  (Path_string.dirname [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(fun string ->
      Ok ((Expert.unchecked_of_canonical_string [@alloc a]) string) [@exclave_if_stack a])
    ~if_none:(function
      | "/" -> Or_error.error_s [%sexp "File_path.Absolute.dirname_or_error: root path"]
      | string ->
        Or_error.error_s
          [%sexp
            "File_path.Absolute.dirname_or_error: path contains no slash"
            , (globalize_string string : string)]) [@exclave_if_stack a]
;;

let[@alloc a] dirname_defaulting_to_root t =
  (Path_string.dirname [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:default_to_root [@exclave_if_stack a]
;;

let[@alloc a] dirname_and_basename t =
  (Path_string.dirname_and_basename [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(fun ~dirname ~basename ->
      Some
        ( (Expert.unchecked_of_canonical_string [@alloc a]) dirname
        , (Part.Expert.unchecked_of_canonical_string [@alloc a]) basename )
      [@exclave_if_stack a])
    ~if_none:(fun _ -> None) [@exclave_if_stack a]
;;

let[@alloc a] append_to_basename_exn path suffix =
  (Path_string.append_to_basename [@alloc a])
    ((to_string [@alloc stack]) path)
    ~suffix
    ~if_valid:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_invalid_path:(fun path ~suffix ->
      raise_s
        [%sexp
          "File_path.Absolute.append_to_basename_exn: root path has no basename"
          , { path = (globalize_string path : string)
            ; suffix = (globalize_string suffix : string)
            }])
    ~if_invalid_suffix:(fun path ~suffix ->
      raise_s
        [%sexp
          "File_path.Absolute.append_to_basename_exn: suffix contains invalid characters"
          , { path = (globalize_string path : string)
            ; suffix = (globalize_string suffix : string)
            }]) [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] append_part t part =
  (Expert.unchecked_of_canonical_string [@alloc a])
    ((Path_string.append [@alloc a])
       ((to_string [@alloc stack]) t)
       ((Part.to_string [@alloc stack]) part)) [@exclave_if_stack a]
;;

let[@alloc a] append prefix suffix =
  (Expert.unchecked_of_canonical_string [@alloc a])
    ((Path_string.append [@alloc a])
       ((to_string [@alloc stack]) prefix)
       ((Relative.to_string [@alloc stack]) suffix)) [@exclave_if_stack a]
;;

let[@alloc a] chop_prefix t ~prefix =
  (Path_string.chop_prefix [@alloc a])
    ((to_string [@alloc a]) t)
    ~prefix:((to_string [@alloc stack]) prefix)
    ~if_some:(fun string ->
      Some ((Relative.Expert.unchecked_of_canonical_string [@alloc a]) string)
      [@exclave_if_stack a])
    ~if_none:(fun _ ~prefix:_ -> None) [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] chop_prefix_exn t ~prefix =
  (Path_string.chop_prefix [@alloc a])
    ((to_string [@alloc a]) t)
    ~prefix:((to_string [@alloc stack]) prefix)
    ~if_some:(Relative.Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(fun path ~prefix ->
      raise_s
        [%sexp
          "File_path.Absolute.chop_prefix_exn: not a prefix"
          , { path = (globalize_string path : string)
            ; prefix = (globalize_string prefix : string)
            }])
  [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] chop_prefix_or_error t ~prefix =
  (Path_string.chop_prefix [@alloc a])
    ((to_string [@alloc a]) t)
    ~prefix:((to_string [@alloc stack]) prefix)
    ~if_some:(fun string ->
      Ok ((Relative.Expert.unchecked_of_canonical_string [@alloc a]) string)
      [@exclave_if_stack a])
    ~if_none:(fun path ~prefix ->
      Or_error.error_s
        [%sexp
          "File_path.Absolute.chop_prefix_or_error: not a prefix"
          , { path = (globalize_string path : string)
            ; prefix = (globalize_string prefix : string)
            }]) [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] chop_suffix t ~suffix =
  (Path_string.chop_suffix [@alloc a])
    ((to_string [@alloc a]) t)
    ~suffix:((Relative.to_string [@alloc stack]) suffix)
    ~if_some:(fun string ->
      Some ((Expert.unchecked_of_canonical_string [@alloc a]) string)
      [@exclave_if_stack a])
    ~if_none:(fun _ ~suffix:_ -> None) [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] chop_suffix_exn t ~suffix =
  (Path_string.chop_suffix [@alloc a])
    ((to_string [@alloc a]) t)
    ~suffix:((Relative.to_string [@alloc stack]) suffix)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(fun path ~suffix ->
      raise_s
        [%sexp
          "File_path.Absolute.chop_suffix_exn: not a suffix"
          , { path = (globalize_string path : string)
            ; suffix = (globalize_string suffix : string)
            }])
  [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] chop_suffix_or_error t ~suffix =
  (Path_string.chop_suffix [@alloc a])
    ((to_string [@alloc a]) t)
    ~suffix:((Relative.to_string [@alloc stack]) suffix)
    ~if_some:(fun string ->
      Ok ((Expert.unchecked_of_canonical_string [@alloc a]) string) [@exclave_if_stack a])
    ~if_none:(fun path ~suffix ->
      Or_error.error_s
        [%sexp
          "File_path.Absolute.chop_suffix_or_error: not a suffix"
          , { path = (globalize_string path : string)
            ; suffix = (globalize_string suffix : string)
            }]) [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] chop_suffix_if_exists t ~suffix =
  (Path_string.chop_suffix [@alloc a])
    ((to_string [@alloc a]) t)
    ~suffix:((Relative.to_string [@alloc stack]) suffix)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(fun path ~suffix:_ ->
      (Expert.unchecked_of_canonical_string [@alloc a]) path [@exclave_if_stack a])
  [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] simplify_dot t =
  (Expert.unchecked_of_canonical_string [@alloc a])
    ((Path_string.simplify_dot [@alloc a]) ((to_string [@alloc a]) t))
  [@exclave_if_stack a]
;;

let[@alloc a] simplify_dot_and_dot_dot_naively t =
  (Expert.unchecked_of_canonical_string [@alloc a])
    ((Path_string.simplify_dot_and_dot_dot_naively [@alloc a]) ((to_string [@alloc a]) t))
  [@exclave_if_stack a]
;;

let[@alloc a] to_parts t =
  (Path_string.to_parts [@alloc a])
    ((to_string [@alloc a]) t)
    ~part_of_string:(Part.Expert.unchecked_of_canonical_string [@alloc a])
  [@exclave_if_stack a]
;;

let[@alloc a] of_parts parts =
  (Expert.unchecked_of_canonical_string [@alloc a])
    ((Path_string.of_parts_absolute [@alloc a]) (parts : Part.t list :> string list))
  [@exclave_if_stack a]
;;]
