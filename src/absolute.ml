open! Core
include Absolute_intf

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
      let canonicalize = Path_string.canonicalize
      let autocomplete = Completion.complete_absolute
    end)

let root = Expert.unchecked_of_canonical_string Path_string.root

let basename t =
  Path_string.basename
    (to_string t)
    ~if_some:(fun string -> Some (Part.Expert.unchecked_of_canonical_string string))
    ~if_none:(fun _ -> None)
;;

let basename_exn t =
  Path_string.basename
    (to_string t)
    ~if_some:Part.Expert.unchecked_of_canonical_string
    ~if_none:(function
    | "/" -> raise_s [%sexp "File_path.Absolute.basename_exn: root path"]
    | string ->
      raise_s
        [%sexp
          "File_path.Absolute.basename_exn: path contains no slash", (string : string)])
;;

let basename_or_error t =
  Path_string.basename
    (to_string t)
    ~if_some:(fun string -> Ok (Part.Expert.unchecked_of_canonical_string string))
    ~if_none:(function
      | "/" -> Or_error.error_s [%sexp "File_path.Absolute.basename_or_error: root path"]
      | string ->
        Or_error.error_s
          [%sexp
            "File_path.Absolute.basename_or_error: path contains no slash"
            , (string : string)])
;;

let basename_defaulting_to_dot t =
  Path_string.basename
    (to_string t)
    ~if_some:Part.Expert.unchecked_of_canonical_string
    ~if_none:(fun _ -> Part.dot)
;;

let dirname t =
  Path_string.dirname
    (to_string t)
    ~if_some:(fun string -> Some (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun _ -> None)
;;

let dirname_exn t =
  Path_string.dirname
    (to_string t)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(function
    | "/" -> raise_s [%sexp "File_path.Absolute.dirname_exn: root path"]
    | string ->
      raise_s
        [%sexp
          "File_path.Absolute.dirname_exn: path contains no slash", (string : string)])
;;

let dirname_or_error t =
  Path_string.dirname
    (to_string t)
    ~if_some:(fun string -> Ok (Expert.unchecked_of_canonical_string string))
    ~if_none:(function
      | "/" -> Or_error.error_s [%sexp "File_path.Absolute.dirname_or_error: root path"]
      | string ->
        Or_error.error_s
          [%sexp
            "File_path.Absolute.dirname_or_error: path contains no slash"
            , (string : string)])
;;

let dirname_defaulting_to_root t =
  Path_string.dirname
    (to_string t)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun _ -> root)
;;

let dirname_and_basename t =
  Path_string.dirname_and_basename
    (to_string t)
    ~if_some:(fun ~dirname ~basename ->
      Some
        ( Expert.unchecked_of_canonical_string dirname
        , Part.Expert.unchecked_of_canonical_string basename ))
    ~if_none:(fun _ -> None)
;;

let append_to_basename_exn path suffix =
  Path_string.append_to_basename
    (to_string path)
    ~suffix
    ~if_valid:Expert.unchecked_of_canonical_string
    ~if_invalid_path:(fun path ~suffix ->
      raise_s
        [%sexp
          "File_path.Absolute.append_to_basename_exn: root path has no basename"
          , { path : string; suffix : string }])
    ~if_invalid_suffix:(fun path ~suffix ->
      raise_s
        [%sexp
          "File_path.Absolute.append_to_basename_exn: suffix contains invalid characters"
          , { path : string; suffix : string }])
;;

let append_part t part =
  Expert.unchecked_of_canonical_string
    (Path_string.append (to_string t) (Part.to_string part))
;;

let append prefix suffix =
  Expert.unchecked_of_canonical_string
    (Path_string.append (to_string prefix) (Relative.to_string suffix))
;;

let is_prefix t ~prefix = Path_string.is_prefix (to_string t) ~prefix:(to_string prefix)

let chop_prefix t ~prefix =
  Path_string.chop_prefix
    (to_string t)
    ~prefix:(to_string prefix)
    ~if_some:(fun string -> Some (Relative.Expert.unchecked_of_canonical_string string))
    ~if_none:(fun _ ~prefix:_ -> None)
;;

let chop_prefix_exn t ~prefix =
  Path_string.chop_prefix
    (to_string t)
    ~prefix:(to_string prefix)
    ~if_some:Relative.Expert.unchecked_of_canonical_string
    ~if_none:(fun path ~prefix ->
      raise_s
        [%sexp
          "File_path.Absolute.chop_prefix_exn: not a prefix"
          , { path : string; prefix : string }])
;;

let chop_prefix_or_error t ~prefix =
  Path_string.chop_prefix
    (to_string t)
    ~prefix:(to_string prefix)
    ~if_some:(fun string -> Ok (Relative.Expert.unchecked_of_canonical_string string))
    ~if_none:(fun path ~prefix ->
      Or_error.error_s
        [%sexp
          "File_path.Absolute.chop_prefix_or_error: not a prefix"
          , { path : string; prefix : string }])
;;

let is_suffix t ~suffix =
  Path_string.is_suffix (to_string t) ~suffix:(Relative.to_string suffix)
;;

let chop_suffix t ~suffix =
  Path_string.chop_suffix
    (to_string t)
    ~suffix:(Relative.to_string suffix)
    ~if_some:(fun string -> Some (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun _ ~suffix:_ -> None)
;;

let chop_suffix_exn t ~suffix =
  Path_string.chop_suffix
    (to_string t)
    ~suffix:(Relative.to_string suffix)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun path ~suffix ->
      raise_s
        [%sexp
          "File_path.Absolute.chop_suffix_exn: not a suffix"
          , { path : string; suffix : string }])
;;

let chop_suffix_or_error t ~suffix =
  Path_string.chop_suffix
    (to_string t)
    ~suffix:(Relative.to_string suffix)
    ~if_some:(fun string -> Ok (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun path ~suffix ->
      Or_error.error_s
        [%sexp
          "File_path.Absolute.chop_suffix_or_error: not a suffix"
          , { path : string; suffix : string }])
;;

let chop_suffix_if_exists t ~suffix =
  Path_string.chop_suffix
    (to_string t)
    ~suffix:(Relative.to_string suffix)
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

let of_parts parts =
  Expert.unchecked_of_canonical_string
    (Path_string.of_parts_absolute (parts : Part.t list :> string list))
;;

let number_of_parts t = Path_string.number_of_parts (to_string t)

include
  Quickcheckable.Of_quickcheckable [@mode portable]
    (Path_string.Quickcheckable_absolute)
    (struct
      type nonrec t = t

      let of_quickcheckable = Expert.unchecked_of_canonical_string
      let to_quickcheckable = to_string
    end)
