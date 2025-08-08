open! Core
include Path_intf

include
  Common.Make
    (Types.Path)
    (struct
      let module_name = "File_path"

      let caller_identity =
        Bin_prot.Shape.Uuid.of_string "352d9090-46ea-43c6-b938-5ae8c477c04f"
      ;;

      let is_valid = Path_string.is_valid
      let is_canonical = Path_string.is_canonical
      let canonicalize = Path_string.canonicalize
      let autocomplete = Completion.complete_path
    end)

let is_absolute t = Path_string.is_absolute (to_string t)
let is_relative t = Path_string.is_relative (to_string t)
let of_absolute absolute = (absolute : Absolute.t :> t)
let of_relative relative = (relative : Relative.t :> t)
let unchecked_to_absolute t = Absolute.Expert.unchecked_of_canonical_string (to_string t)
let unchecked_to_relative t = Relative.Expert.unchecked_of_canonical_string (to_string t)
let to_absolute t = if is_absolute t then Some (unchecked_to_absolute t) else None
let to_relative t = if is_relative t then Some (unchecked_to_relative t) else None

let to_absolute_exn t =
  if is_absolute t
  then unchecked_to_absolute t
  else raise_s [%sexp "File_path.to_absolute_exn: path is relative", (t : t)]
;;

let to_relative_exn t =
  if is_relative t
  then unchecked_to_relative t
  else raise_s [%sexp "File_path.to_relative_exn: path is absolute", (t : t)]
;;

let to_absolute_or_error t =
  if is_absolute t
  then Ok (unchecked_to_absolute t)
  else
    Or_error.error_s [%sexp "File_path.to_absolute_or_error: path is relative", (t : t)]
;;

let to_relative_or_error t =
  if is_relative t
  then Ok (unchecked_to_relative t)
  else
    Or_error.error_s [%sexp "File_path.to_relative_or_error: path is absolute", (t : t)]
;;

let root = of_absolute Absolute.root
let dot = of_relative Relative.dot
let dot_dot = of_relative Relative.dot_dot

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
    | "/" -> raise_s [%sexp "File_path.basename_exn: root path"]
    | string ->
      raise_s [%sexp "File_path.basename_exn: path contains no slash", (string : string)])
;;

let basename_or_error t =
  Path_string.basename
    (to_string t)
    ~if_some:(fun string -> Ok (Part.Expert.unchecked_of_canonical_string string))
    ~if_none:(function
      | "/" -> Or_error.error_s [%sexp "File_path.basename_or_error: root path"]
      | string ->
        Or_error.error_s
          [%sexp "File_path.basename_or_error: path contains no slash", (string : string)])
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
    | "/" -> raise_s [%sexp "File_path.dirname_exn: root path"]
    | string ->
      raise_s [%sexp "File_path.dirname_exn: path contains no slash", (string : string)])
;;

let dirname_or_error t =
  Path_string.dirname
    (to_string t)
    ~if_some:(fun string -> Ok (Expert.unchecked_of_canonical_string string))
    ~if_none:(function
      | "/" -> Or_error.error_s [%sexp "File_path.dirname_or_error: root path"]
      | string ->
        Or_error.error_s
          [%sexp "File_path.dirname_or_error: path contains no slash", (string : string)])
;;

let dirname_defaulting_to_dot_or_root t =
  Path_string.dirname
    (to_string t)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun string -> if Path_string.is_absolute string then root else dot)
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
          "File_path.append_to_basename_exn: root path has no basename"
          , { path : string; suffix : string }])
    ~if_invalid_suffix:(fun path ~suffix ->
      raise_s
        [%sexp
          "File_path.append_to_basename_exn: suffix contains invalid characters"
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
          "File_path.chop_prefix_exn: not a prefix", { path : string; prefix : string }])
;;

let chop_prefix_or_error t ~prefix =
  Path_string.chop_prefix
    (to_string t)
    ~prefix:(to_string prefix)
    ~if_some:(fun string -> Ok (Relative.Expert.unchecked_of_canonical_string string))
    ~if_none:(fun path ~prefix ->
      Or_error.error_s
        [%sexp
          "File_path.chop_prefix_or_error: not a prefix"
          , { path : string; prefix : string }])
;;

let chop_prefix_if_exists t ~prefix =
  Path_string.chop_prefix
    (to_string t)
    ~prefix:(to_string prefix)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun path ~prefix:_ -> Expert.unchecked_of_canonical_string path)
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
          "File_path.chop_suffix_exn: not a suffix", { path : string; suffix : string }])
;;

let chop_suffix_or_error t ~suffix =
  Path_string.chop_suffix
    (to_string t)
    ~suffix:(Relative.to_string suffix)
    ~if_some:(fun string -> Ok (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun path ~suffix ->
      Or_error.error_s
        [%sexp
          "File_path.chop_suffix_or_error: not a suffix"
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

let of_part_relative part = Expert.unchecked_of_canonical_string (Part.to_string part)

let to_parts t =
  Path_string.to_parts
    (to_string t)
    ~part_of_string:Part.Expert.unchecked_of_canonical_string
;;

let of_parts_absolute parts =
  Expert.unchecked_of_canonical_string
    (Path_string.of_parts_absolute (parts : Part.t list :> string list))
;;

let of_parts_relative parts =
  Path_string.of_parts_relative
    (parts : Part.t list :> string list)
    ~if_some:(fun string -> Some (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun () -> None)
;;

let of_parts_relative_exn parts =
  Path_string.of_parts_relative
    (parts : Part.t list :> string list)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun () -> raise_s [%sexp "File_path.of_parts_relative_exn: empty list"])
;;

let of_parts_relative_or_error parts =
  Path_string.of_parts_relative
    (parts : Part.t list :> string list)
    ~if_some:(fun string -> Ok (Expert.unchecked_of_canonical_string string))
    ~if_none:(fun () ->
      Or_error.error_s [%sexp "File_path.of_parts_relative_or_error: empty list"])
;;

let of_parts_relative_defaulting_to_dot parts =
  Path_string.of_parts_relative
    (parts : Part.t list :> string list)
    ~if_some:Expert.unchecked_of_canonical_string
    ~if_none:(fun () -> dot)
;;

let of_parts_relative_nonempty parts = of_parts_relative_exn (Nonempty_list.to_list parts)
let number_of_parts t = Path_string.number_of_parts (to_string t)

let make_absolute t ~under =
  if is_absolute t
  then unchecked_to_absolute t
  else Absolute.append under (unchecked_to_relative t)
;;

let make_relative t ~if_under =
  if is_relative t
  then Some (unchecked_to_relative t)
  else
    Path_string.chop_prefix
      (to_string t)
      ~prefix:(Absolute.to_string if_under)
      ~if_some:(fun string -> Some (Relative.Expert.unchecked_of_canonical_string string))
      ~if_none:(fun _ ~prefix:_ -> None)
;;

let make_relative_exn t ~if_under =
  if is_relative t
  then unchecked_to_relative t
  else
    Path_string.chop_prefix
      (to_string t)
      ~prefix:(Absolute.to_string if_under)
      ~if_some:Relative.Expert.unchecked_of_canonical_string
      ~if_none:(fun string ~prefix ->
        raise_s
          [%sexp
            "File_path.make_relative_exn: cannot make path relative"
            , { path = (string : string); if_under = (prefix : string) }])
;;

let make_relative_or_error t ~if_under =
  if is_relative t
  then Ok (unchecked_to_relative t)
  else
    Path_string.chop_prefix
      (to_string t)
      ~prefix:(Absolute.to_string if_under)
      ~if_some:(fun string -> Ok (Relative.Expert.unchecked_of_canonical_string string))
      ~if_none:(fun string ~prefix ->
        Or_error.error_s
          [%sexp
            "File_path.make_relative_or_error: cannot make path relative"
            , { path = (string : string); if_under = (prefix : string) }])
;;

let make_relative_if_possible t ~if_under =
  if is_relative t
  then t
  else
    Path_string.chop_prefix
      (to_string t)
      ~prefix:(Absolute.to_string if_under)
      ~if_some:Expert.unchecked_of_canonical_string
      ~if_none:(fun string ~prefix:_ -> Expert.unchecked_of_canonical_string string)
;;

module Variant = struct
  type t =
    | Relative of Relative.t
    | Absolute of Absolute.t
  [@@deriving compare ~localize, equal ~localize, quickcheck ~portable, sexp_of]

  let invariant t =
    Invariant.invariant t sexp_of_t (fun () ->
      match t with
      | Relative relative -> Relative.invariant relative
      | Absolute absolute -> Absolute.invariant absolute)
  ;;
end

let of_variant x =
  match (x : Variant.t) with
  | Relative relative -> of_relative relative
  | Absolute absolute -> of_absolute absolute
;;

let to_variant t : Variant.t =
  if is_absolute t
  then Absolute (unchecked_to_absolute t)
  else Relative (unchecked_to_relative t)
;;

include
  Quickcheckable.Of_quickcheckable [@mode portable]
    (Path_string.Quickcheckable_path)
    (struct
      type nonrec t = t

      let of_quickcheckable = Expert.unchecked_of_canonical_string
      let to_quickcheckable = to_string
    end)
