open! Core
include Relative_intf.Definitions

include
  Common.Make
    (Types.Relative)
    (struct
      let module_name = "File_path.Relative"

      let caller_identity =
        Bin_prot.Shape.Uuid.of_string "2f102865-a5e8-43a8-b1a1-e52c43241076"
      ;;

      let is_valid string = Path_string.is_relative string && Path_string.is_valid string
      let is_canonical = Path_string.is_canonical

      let%template canonicalize = (Path_string.canonicalize [@alloc a])
      [@@alloc a = (stack, heap)]
      ;;

      let autocomplete = Completion.complete_relative

      module Quickcheckable_string = Path_string.Quickcheckable_relative
    end)

let dot = (Part.dot :> t)
let dot_dot = (Part.dot_dot :> t)
let default_to_dot _ = dot

[%%template
let is_prefix t ~prefix =
  Path_string.is_prefix
    ((to_string [@alloc stack]) t)
    ~prefix:((to_string [@alloc stack]) prefix) [@nontail]
;;

let is_suffix t ~suffix =
  Path_string.is_suffix
    ((to_string [@alloc stack]) t)
    ~suffix:((to_string [@alloc stack]) suffix) [@nontail]
;;

let number_of_parts t =
  Path_string.number_of_parts ((to_string [@alloc stack]) t) [@nontail]
;;

[@@@alloc a @ l = (stack_local, heap_global)]

let[@mode l] of_part part = (part : Part.t :> t)

let[@alloc a] basename t =
  (Path_string.basename [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(Part.Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(fun _ ->
      (* all relative paths have at least one part *)
      assert false)
  [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] dirname_exn t =
  (Path_string.dirname [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(fun string ->
      raise_s
        [%sexp
          "File_path.Relative.dirname_exn: path contains no slash"
          , (globalize_string string : string)])
  [@exclave_if_stack a]
;;

let[@alloc a] dirname_or_error t =
  (Path_string.dirname [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(fun string ->
      Ok ((Expert.unchecked_of_canonical_string [@alloc a]) string) [@exclave_if_stack a])
    ~if_none:(fun string ->
      Or_error.error_s
        [%sexp
          "File_path.Relative.dirname_or_error: path contains no slash"
          , (globalize_string string : string)]) [@exclave_if_stack a]
;;

let[@alloc a] dirname t =
  (Path_string.dirname [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(fun string ->
      Some ((Expert.unchecked_of_canonical_string [@alloc a]) string)
      [@exclave_if_stack a])
    ~if_none:(fun _ -> None) [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] dirname_defaulting_to_dot t =
  (Path_string.dirname [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:default_to_dot [@exclave_if_stack a]
;;

let[@alloc a] top_dir t =
  (Path_string.top_dir [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(fun string ->
      Some ((Part.Expert.unchecked_of_canonical_string [@alloc a]) string)
      [@exclave_if_stack a])
    ~if_none:(fun _ -> None) [@exclave_if_stack a]
;;

let[@alloc a] top_dir_exn t =
  (Path_string.top_dir [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(Part.Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(fun string ->
      raise_s
        [%sexp
          "File_path.Relative.top_dir_exn: path contains no slash"
          , (globalize_string string : string)])
  [@exclave_if_stack a]
;;

let[@alloc a] top_dir_or_error t =
  (Path_string.top_dir [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(fun string ->
      Ok ((Part.Expert.unchecked_of_canonical_string [@alloc a]) string)
      [@exclave_if_stack a])
    ~if_none:(fun string ->
      Or_error.error_s
        [%sexp
          "File_path.Relative.top_dir_or_error: path contains no slash"
          , (globalize_string string : string)]) [@exclave_if_stack a]
;;

let[@alloc a] top_dir_defaulting_to_dot t =
  (Path_string.top_dir [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(Part.Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(fun _ -> Part.dot)
  [@exclave_if_stack a]
;;

let[@alloc a] all_but_top_dir t =
  (Path_string.all_but_top_dir [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(fun string ->
      Some ((Expert.unchecked_of_canonical_string [@alloc a]) string)
      [@exclave_if_stack a])
    ~if_none:(fun _ -> None) [@exclave_if_stack a]
;;

let[@alloc a] all_but_top_dir_exn t =
  (Path_string.all_but_top_dir [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(fun string ->
      raise_s
        [%sexp
          "File_path.Relative.all_but_top_dir_exn: path contains no slash"
          , (globalize_string string : string)])
  [@exclave_if_stack a]
;;

let[@alloc a] all_but_top_dir_or_error t =
  (Path_string.all_but_top_dir [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(fun string ->
      Ok ((Expert.unchecked_of_canonical_string [@alloc a]) string) [@exclave_if_stack a])
    ~if_none:(fun string ->
      Or_error.error_s
        [%sexp
          "File_path.Relative.all_but_top_dir_or_error: path contains no slash"
          , (globalize_string string : string)]) [@exclave_if_stack a]
;;

let[@alloc a] all_but_top_dir_defaulting_to_self t =
  (Path_string.all_but_top_dir [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(Expert.unchecked_of_canonical_string [@alloc a]) [@exclave_if_stack a]
;;

let[@alloc a] top_dir_and_all_but_top_dir t =
  (Path_string.top_dir_and_all_but_top_dir [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(fun ~top_dir ~all_but_top_dir ->
      Some
        ( (Part.Expert.unchecked_of_canonical_string [@alloc a]) top_dir
        , (Expert.unchecked_of_canonical_string [@alloc a]) all_but_top_dir )
      [@exclave_if_stack a])
    ~if_none:(fun _ -> None) [@exclave_if_stack a]
;;

let[@alloc a] append_to_basename_exn path suffix =
  (Path_string.append_to_basename [@alloc a])
    ((to_string [@alloc stack]) path)
    ~suffix
    ~if_valid:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_invalid_path:(fun _ ~suffix:_ -> (* root path is not relative *) assert false)
    ~if_invalid_suffix:(fun path ~suffix ->
      raise_s
        [%sexp
          "File_path.Relative.append_to_basename_exn: suffix contains invalid characters"
          , { path = (globalize_string path : string)
            ; suffix = (globalize_string suffix : string)
            }]) [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] prepend_part part t =
  (Expert.unchecked_of_canonical_string [@alloc a])
    ((Path_string.append [@alloc a])
       ((Part.to_string [@alloc stack]) part)
       ((to_string [@alloc stack]) t)) [@exclave_if_stack a]
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
       ((to_string [@alloc stack]) suffix)) [@exclave_if_stack a]
;;

let[@alloc a] chop_prefix t ~prefix =
  (Path_string.chop_prefix [@alloc a])
    ((to_string [@alloc a]) t)
    ~prefix:((to_string [@alloc stack]) prefix)
    ~if_some:(fun string ->
      Some ((Expert.unchecked_of_canonical_string [@alloc a]) string)
      [@exclave_if_stack a])
    ~if_none:(fun _ ~prefix:_ -> None) [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] chop_prefix_exn t ~prefix =
  (Path_string.chop_prefix [@alloc a])
    ((to_string [@alloc a]) t)
    ~prefix:((to_string [@alloc stack]) prefix)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(fun path ~prefix ->
      raise_s
        [%sexp
          "File_path.Relative.chop_prefix_exn: not a prefix"
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
      Ok ((Expert.unchecked_of_canonical_string [@alloc a]) string) [@exclave_if_stack a])
    ~if_none:(fun path ~prefix ->
      Or_error.error_s
        [%sexp
          "File_path.Relative.chop_prefix_or_error: not a prefix"
          , { path = (globalize_string path : string)
            ; prefix = (globalize_string prefix : string)
            }]) [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] chop_prefix_if_exists t ~prefix =
  (Path_string.chop_prefix [@alloc a])
    ((to_string [@alloc a]) t)
    ~prefix:((to_string [@alloc stack]) prefix)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(fun path ~prefix:_ ->
      (Expert.unchecked_of_canonical_string [@alloc a]) path [@exclave_if_stack a])
  [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] chop_suffix t ~suffix =
  (Path_string.chop_suffix [@alloc a])
    ((to_string [@alloc a]) t)
    ~suffix:((to_string [@alloc stack]) suffix)
    ~if_some:(fun string ->
      Some ((Expert.unchecked_of_canonical_string [@alloc a]) string)
      [@exclave_if_stack a])
    ~if_none:(fun _ ~suffix:_ -> None) [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] chop_suffix_exn t ~suffix =
  (Path_string.chop_suffix [@alloc a])
    ((to_string [@alloc a]) t)
    ~suffix:((to_string [@alloc stack]) suffix)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(fun path ~suffix ->
      raise_s
        [%sexp
          "File_path.Relative.chop_suffix_exn: not a suffix"
          , { path = (globalize_string path : string)
            ; suffix = (globalize_string suffix : string)
            }])
  [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] chop_suffix_or_error t ~suffix =
  (Path_string.chop_suffix [@alloc a])
    ((to_string [@alloc a]) t)
    ~suffix:((to_string [@alloc stack]) suffix)
    ~if_some:(fun string ->
      Ok ((Expert.unchecked_of_canonical_string [@alloc a]) string) [@exclave_if_stack a])
    ~if_none:(fun path ~suffix ->
      Or_error.error_s
        [%sexp
          "File_path.Relative.chop_suffix_or_error: not a suffix"
          , { path = (globalize_string path : string)
            ; suffix = (globalize_string suffix : string)
            }]) [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] chop_suffix_if_exists t ~suffix =
  (Path_string.chop_suffix [@alloc a])
    ((to_string [@alloc a]) t)
    ~suffix:((to_string [@alloc stack]) suffix)
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

let[@alloc a] to_parts_nonempty t =
  (Nonempty_list.of_list_exn [@mode l]) ((to_parts [@alloc a]) t) [@exclave_if_stack a]
;;

let[@alloc a] of_parts parts =
  (Path_string.of_parts_relative [@alloc a])
    (parts : Part.t list :> string list)
    ~if_some:(fun string ->
      Some ((Expert.unchecked_of_canonical_string [@alloc a]) string)
      [@exclave_if_stack a])
    ~if_none:(fun () -> None) [@exclave_if_stack a]
;;

let[@alloc a] of_parts_exn parts =
  (Path_string.of_parts_relative [@alloc a])
    (parts : Part.t list :> string list)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(fun () -> raise_s [%sexp "File_path.Relative.of_parts_exn: empty list"])
  [@exclave_if_stack a]
;;

let[@alloc a] of_parts_or_error parts =
  (Path_string.of_parts_relative [@alloc a])
    (parts : Part.t list :> string list)
    ~if_some:(fun string ->
      Ok ((Expert.unchecked_of_canonical_string [@alloc a]) string) [@exclave_if_stack a])
    ~if_none:(fun () ->
      Or_error.error_s [%sexp "File_path.Relative.of_parts_or_error: empty list"])
  [@exclave_if_stack a]
;;

let[@alloc a] of_parts_defaulting_to_dot parts =
  (Path_string.of_parts_relative [@alloc a])
    (parts : Part.t list :> string list)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:default_to_dot [@exclave_if_stack a]
;;

let[@alloc a] of_parts_nonempty parts =
  (of_parts_exn [@alloc a])
    ((Nonempty_list.to_list [@alloc a]) parts) [@nontail] [@exclave_if_stack a]
;;]
