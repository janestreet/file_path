open! Core
include Relative_intf

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
      let canonicalize = Path_string.canonicalize
      let autocomplete = Completion.complete_relative
    end)

let of_part part = (part : Part.t :> t)
let dot = of_part Part.dot
let dot_dot = of_part Part.dot_dot

let basename t =
  Path_string.basename
    (to_string t)
    ~if_some:Part.Expert.unchecked_of_canonical_string
    ~if_none:(fun _ ->
      (* all relative paths have at least one part *)
      assert false)
;;

let dirname_exn t =
  Path_string.dirname
    (to_string t)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun string ->
      raise_s
        [%sexp
          "File_path.Relative.dirname_exn: path contains no slash", (string : string)])
;;

let dirname_or_error t =
  Path_string.dirname
    (to_string t)
    ~if_some:(fun string -> Ok (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun string ->
      Or_error.error_s
        [%sexp
          "File_path.Relative.dirname_or_error: path contains no slash", (string : string)])
;;

let dirname t =
  Path_string.dirname
    (to_string t)
    ~if_some:(fun string -> Some (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun _ -> None)
;;

let dirname_defaulting_to_dot t =
  Path_string.dirname
    (to_string t)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun _ -> dot)
;;

let top_dir t =
  Path_string.top_dir
    (to_string t)
    ~if_some:(fun string -> Some (Part.Expert.unchecked_of_canonical_string string))
    ~if_none:(fun _ -> None)
;;

let top_dir_exn t =
  Path_string.top_dir
    (to_string t)
    ~if_some:Part.Expert.unchecked_of_canonical_string
    ~if_none:(fun string ->
      raise_s
        [%sexp
          "File_path.Relative.top_dir_exn: path contains no slash", (string : string)])
;;

let top_dir_or_error t =
  Path_string.top_dir
    (to_string t)
    ~if_some:(fun string -> Ok (Part.Expert.unchecked_of_canonical_string string))
    ~if_none:(fun string ->
      Or_error.error_s
        [%sexp
          "File_path.Relative.top_dir_or_error: path contains no slash", (string : string)])
;;

let top_dir_defaulting_to_dot t =
  Path_string.top_dir
    (to_string t)
    ~if_some:Part.Expert.unchecked_of_canonical_string
    ~if_none:(fun _ -> Part.dot)
;;

let all_but_top_dir t =
  Path_string.all_but_top_dir
    (to_string t)
    ~if_some:(fun string -> Some (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun _ -> None)
;;

let all_but_top_dir_exn t =
  Path_string.all_but_top_dir
    (to_string t)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun string ->
      raise_s
        [%sexp
          "File_path.Relative.all_but_top_dir_exn: path contains no slash"
          , (string : string)])
;;

let all_but_top_dir_or_error t =
  Path_string.all_but_top_dir
    (to_string t)
    ~if_some:(fun string -> Ok (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun string ->
      Or_error.error_s
        [%sexp
          "File_path.Relative.all_but_top_dir_or_error: path contains no slash"
          , (string : string)])
;;

let all_but_top_dir_defaulting_to_self t =
  Path_string.all_but_top_dir
    (to_string t)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:Expert.unchecked_of_canonical_string
;;

let top_dir_and_all_but_top_dir t =
  Path_string.top_dir_and_all_but_top_dir
    (to_string t)
    ~if_some:(fun ~top_dir ~all_but_top_dir ->
      Some
        ( Part.Expert.unchecked_of_canonical_string top_dir
        , Expert.unchecked_of_canonical_string all_but_top_dir ))
    ~if_none:(fun _ -> None)
;;

let append_to_basename_exn path suffix =
  Path_string.append_to_basename
    (to_string path)
    ~suffix
    ~if_valid:Expert.unchecked_of_canonical_string
    ~if_invalid_path:(fun _ ~suffix:_ -> (* root path is not relative *) assert false)
    ~if_invalid_suffix:(fun path ~suffix ->
      raise_s
        [%sexp
          "File_path.Relative.append_to_basename_exn: suffix contains invalid characters"
          , { path : string; suffix : string }])
;;

let prepend_part part t =
  Expert.unchecked_of_canonical_string
    (Path_string.append (Part.to_string part) (to_string t))
;;

let append_part t part =
  Expert.unchecked_of_canonical_string
    (Path_string.append (to_string t) (Part.to_string part))
;;

let append prefix suffix =
  Expert.unchecked_of_canonical_string
    (Path_string.append (to_string prefix) (to_string suffix))
;;

let is_prefix t ~prefix = Path_string.is_prefix (to_string t) ~prefix:(to_string prefix)

let chop_prefix t ~prefix =
  Path_string.chop_prefix
    (to_string t)
    ~prefix:(to_string prefix)
    ~if_some:(fun string -> Some (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun _ ~prefix:_ -> None)
;;

let chop_prefix_exn t ~prefix =
  Path_string.chop_prefix
    (to_string t)
    ~prefix:(to_string prefix)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun path ~prefix ->
      raise_s
        [%sexp
          "File_path.Relative.chop_prefix_exn: not a prefix"
          , { path : string; prefix : string }])
;;

let chop_prefix_or_error t ~prefix =
  Path_string.chop_prefix
    (to_string t)
    ~prefix:(to_string prefix)
    ~if_some:(fun string -> Ok (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun path ~prefix ->
      Or_error.error_s
        [%sexp
          "File_path.Relative.chop_prefix_or_error: not a prefix"
          , { path : string; prefix : string }])
;;

let chop_prefix_if_exists t ~prefix =
  Path_string.chop_prefix
    (to_string t)
    ~prefix:(to_string prefix)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun path ~prefix:_ -> Expert.unchecked_of_canonical_string path)
;;

let is_suffix t ~suffix = Path_string.is_suffix (to_string t) ~suffix:(to_string suffix)

let chop_suffix t ~suffix =
  Path_string.chop_suffix
    (to_string t)
    ~suffix:(to_string suffix)
    ~if_some:(fun string -> Some (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun _ ~suffix:_ -> None)
;;

let chop_suffix_exn t ~suffix =
  Path_string.chop_suffix
    (to_string t)
    ~suffix:(to_string suffix)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun path ~suffix ->
      raise_s
        [%sexp
          "File_path.Relative.chop_suffix_exn: not a suffix"
          , { path : string; suffix : string }])
;;

let chop_suffix_or_error t ~suffix =
  Path_string.chop_suffix
    (to_string t)
    ~suffix:(to_string suffix)
    ~if_some:(fun string -> Ok (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun path ~suffix ->
      Or_error.error_s
        [%sexp
          "File_path.Relative.chop_suffix_or_error: not a suffix"
          , { path : string; suffix : string }])
;;

let chop_suffix_if_exists t ~suffix =
  Path_string.chop_suffix
    (to_string t)
    ~suffix:(to_string suffix)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun path ~suffix:_ -> Expert.unchecked_of_canonical_string path)
;;

let simplify_dot t =
  Expert.unchecked_of_canonical_string (Path_string.simplify_dot (to_string t))
;;

let simplify_dot_and_dot_dot_naively t =
  Expert.unchecked_of_canonical_string
    (Path_string.simplify_dot_and_dot_dot_naively (to_string t))
;;

let to_parts t =
  Path_string.to_parts
    (to_string t)
    ~part_of_string:Part.Expert.unchecked_of_canonical_string
;;

let to_parts_nonempty t = Nonempty_list.of_list_exn (to_parts t)

let of_parts parts =
  Path_string.of_parts_relative
    (parts : Part.t list :> string list)
    ~if_some:(fun string -> Some (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun () -> None)
;;

let of_parts_exn parts =
  Path_string.of_parts_relative
    (parts : Part.t list :> string list)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun () -> raise_s [%sexp "File_path.Relative.of_parts_exn: empty list"])
;;

let of_parts_or_error parts =
  Path_string.of_parts_relative
    (parts : Part.t list :> string list)
    ~if_some:(fun string -> Ok (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun () ->
      Or_error.error_s [%sexp "File_path.Relative.of_parts_or_error: empty list"])
;;

let of_parts_defaulting_to_dot parts =
  Path_string.of_parts_relative
    (parts : Part.t list :> string list)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun () -> dot)
;;

let of_parts_nonempty parts = of_parts_exn (Nonempty_list.to_list parts)
let number_of_parts t = Path_string.number_of_parts (to_string t)

include
  Quickcheckable.Of_quickcheckable [@mode portable]
    (Path_string.Quickcheckable_relative)
    (struct
      type nonrec t = t

      let of_quickcheckable = Expert.unchecked_of_canonical_string
      let to_quickcheckable = to_string
    end)
