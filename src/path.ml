open! Core
include Path_intf.Definitions

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

      let%template canonicalize = (Path_string.canonicalize [@alloc a])
      [@@alloc a = (stack, heap)]
      ;;

      let autocomplete = Completion.complete_path

      module Quickcheckable_string = Path_string.Quickcheckable_path
    end)

module Variant = struct
  type t =
    | Relative of Relative.t
    | Absolute of Absolute.t
  [@@deriving
    compare ~localize, equal ~localize, globalize, quickcheck ~portable, sexp_of]

  let invariant t =
    Invariant.invariant t sexp_of_t (fun () ->
      match t with
      | Relative relative -> Relative.invariant relative
      | Absolute absolute -> Absolute.invariant absolute)
  ;;
end

let root = (Absolute.root :> t)
let dot = (Relative.dot :> t)
let dot_dot = (Relative.dot_dot :> t)
let default_to_dot_or_root string = if Path_string.is_absolute string then root else dot
let default_to_dot _ = dot

[%%template
let is_absolute t = Path_string.is_absolute ((to_string [@alloc stack]) t) [@nontail]
let is_relative t = Path_string.is_relative ((to_string [@alloc stack]) t) [@nontail]

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

let[@mode l] of_absolute absolute =
  (absolute : Absolute.t :> t) [@exclave_if_local l ~reasons:[ May_return_regional ]]
;;

let[@mode l] of_relative relative =
  (relative : Relative.t :> t) [@exclave_if_local l ~reasons:[ May_return_regional ]]
;;

let[@mode l] unchecked_to_absolute t =
  (Absolute.Expert.unchecked_of_canonical_string [@alloc a])
    ((to_string [@alloc a]) t) [@exclave_if_local l ~reasons:[ May_return_local ]]
;;

let[@mode l] unchecked_to_relative t =
  (Relative.Expert.unchecked_of_canonical_string [@alloc a])
    ((to_string [@alloc a]) t) [@exclave_if_local l ~reasons:[ May_return_local ]]
;;

let[@alloc a] to_absolute t =
  (if is_absolute t then Some ((unchecked_to_absolute [@mode l]) t) else None)
  [@exclave_if_stack a]
;;

let[@alloc a] to_relative t =
  (if is_relative t then Some ((unchecked_to_relative [@mode l]) t) else None)
  [@exclave_if_stack a]
;;

let[@mode l] to_absolute_exn (t @ l) =
  if is_absolute t
  then (unchecked_to_absolute [@mode l]) t [@exclave_if_local l]
  else raise_s [%sexp "File_path.to_absolute_exn: path is relative", (globalize t : t)]
;;

let[@mode l] to_relative_exn (t @ l) =
  if is_relative t
  then (unchecked_to_relative [@mode l]) t [@exclave_if_local l]
  else raise_s [%sexp "File_path.to_relative_exn: path is absolute", (globalize t : t)]
;;

let[@alloc a] to_absolute_or_error t =
  (if is_absolute t
   then Ok ((unchecked_to_absolute [@mode l]) t)
   else
     Or_error.error_s
       [%sexp "File_path.to_absolute_or_error: path is relative", (globalize t : t)])
  [@exclave_if_stack a]
;;

let[@alloc a] to_relative_or_error t =
  (if is_relative t
   then Ok ((unchecked_to_relative [@mode l]) t)
   else
     Or_error.error_s
       [%sexp "File_path.to_relative_or_error: path is absolute", (globalize t : t)])
  [@exclave_if_stack a]
;;

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
    | "/" -> raise_s [%sexp "File_path.basename_exn: root path"]
    | string ->
      raise_s
        [%sexp
          "File_path.basename_exn: path contains no slash"
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
      | "/" -> Or_error.error_s [%sexp "File_path.basename_or_error: root path"]
      | string ->
        Or_error.error_s
          [%sexp
            "File_path.basename_or_error: path contains no slash"
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
    | "/" -> raise_s [%sexp "File_path.dirname_exn: root path"]
    | string ->
      raise_s
        [%sexp
          "File_path.dirname_exn: path contains no slash"
          , (globalize_string string : string)])
    [@exclave_if_stack a]
;;

let[@alloc a] dirname_or_error t =
  (Path_string.dirname [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(fun string ->
      Ok ((Expert.unchecked_of_canonical_string [@alloc a]) string) [@exclave_if_stack a])
    ~if_none:(function
      | "/" -> Or_error.error_s [%sexp "File_path.dirname_or_error: root path"]
      | string ->
        Or_error.error_s
          [%sexp
            "File_path.dirname_or_error: path contains no slash"
            , (globalize_string string : string)]) [@exclave_if_stack a]
;;

let[@alloc a] dirname_defaulting_to_dot_or_root t =
  (Path_string.dirname [@alloc a])
    ((to_string [@alloc a]) t)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:default_to_dot_or_root [@exclave_if_stack a]
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
          "File_path.append_to_basename_exn: root path has no basename"
          , { path = (globalize_string path : string)
            ; suffix = (globalize_string suffix : string)
            }] [@nontail])
    ~if_invalid_suffix:(fun path ~suffix ->
      raise_s
        [%sexp
          "File_path.append_to_basename_exn: suffix contains invalid characters"
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
          "File_path.chop_prefix_exn: not a prefix"
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
          "File_path.chop_prefix_or_error: not a prefix"
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
          "File_path.chop_suffix_exn: not a suffix"
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
          "File_path.chop_suffix_or_error: not a suffix"
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

let[@mode l] of_part_relative part =
  (Expert.unchecked_of_canonical_string [@alloc a])
    ((Part.to_string [@alloc a]) part)
  [@exclave_if_local l ~reasons:[ May_return_regional ]]
;;

let[@alloc a] to_parts t =
  (Path_string.to_parts [@alloc a])
    ((to_string [@alloc a]) t)
    ~part_of_string:(Part.Expert.unchecked_of_canonical_string [@alloc a])
  [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] of_parts_absolute parts =
  (Expert.unchecked_of_canonical_string [@alloc a])
    ((Path_string.of_parts_absolute [@alloc a]) (parts : Part.t list :> string list))
  [@exclave_if_stack a]
;;

let[@alloc a] of_parts_relative parts =
  (Path_string.of_parts_relative [@alloc a])
    (parts : Part.t list :> string list)
    ~if_some:(fun string ->
      Some ((Expert.unchecked_of_canonical_string [@alloc a]) string)
      [@exclave_if_stack a])
    ~if_none:(fun () -> None) [@exclave_if_stack a]
;;

let[@alloc a] of_parts_relative_exn parts =
  (Path_string.of_parts_relative [@alloc a])
    (parts : Part.t list :> string list)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:(fun () -> raise_s [%sexp "File_path.of_parts_relative_exn: empty list"])
  [@exclave_if_stack a]
;;

let[@alloc a] of_parts_relative_or_error parts =
  (Path_string.of_parts_relative [@alloc a])
    (parts : Part.t list :> string list)
    ~if_some:(fun string ->
      Ok ((Expert.unchecked_of_canonical_string [@alloc a]) string) [@exclave_if_stack a])
    ~if_none:(fun () ->
      Or_error.error_s [%sexp "File_path.of_parts_relative_or_error: empty list"])
  [@exclave_if_stack a]
;;

let[@alloc a] of_parts_relative_defaulting_to_dot parts =
  (Path_string.of_parts_relative [@alloc a])
    (parts : Part.t list :> string list)
    ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
    ~if_none:default_to_dot [@exclave_if_stack a]
;;

let[@alloc a] of_parts_relative_nonempty parts =
  (of_parts_relative_exn [@alloc a])
    ((Nonempty_list.to_list [@alloc a]) parts) [@nontail] [@exclave_if_stack a]
;;

let[@alloc a] make_absolute t ~under =
  (if is_absolute t
   then (unchecked_to_absolute [@mode l]) t
   else (Absolute.append [@alloc a]) under ((unchecked_to_relative [@mode l]) t))
  [@exclave_if_stack a]
;;

let[@alloc a] make_relative t ~if_under =
  (if is_relative t
   then Some ((unchecked_to_relative [@mode l]) t)
   else
     (Path_string.chop_prefix [@alloc a])
       ((to_string [@alloc a]) t)
       ~prefix:((Absolute.to_string [@alloc stack]) if_under)
       ~if_some:(fun string ->
         Some ((Relative.Expert.unchecked_of_canonical_string [@alloc a]) string)
         [@exclave_if_stack a])
       ~if_none:(fun _ ~prefix:_ -> None) [@nontail])
  [@exclave_if_stack a]
;;

let[@alloc a] make_relative_exn t ~if_under =
  (if is_relative t
   then (unchecked_to_relative [@mode l]) t
   else
     (Path_string.chop_prefix [@alloc a])
       ((to_string [@alloc a]) t)
       ~prefix:((Absolute.to_string [@alloc stack]) if_under)
       ~if_some:(Relative.Expert.unchecked_of_canonical_string [@alloc a])
       ~if_none:(fun string ~prefix ->
         raise_s
           [%sexp
             "File_path.make_relative_exn: cannot make path relative"
             , { path = (globalize_string string : string)
               ; if_under = (globalize_string prefix : string)
               }])
     [@nontail])
  [@exclave_if_stack a]
;;

let[@alloc a] make_relative_or_error t ~if_under =
  (if is_relative t
   then Ok ((unchecked_to_relative [@mode l]) t)
   else
     (Path_string.chop_prefix [@alloc a])
       ((to_string [@alloc a]) t)
       ~prefix:((Absolute.to_string [@alloc stack]) if_under)
       ~if_some:(fun string ->
         Ok ((Relative.Expert.unchecked_of_canonical_string [@alloc a]) string)
         [@exclave_if_stack a])
       ~if_none:(fun string ~prefix ->
         Or_error.error_s
           [%sexp
             "File_path.make_relative_or_error: cannot make path relative"
             , { path = (globalize_string string : string)
               ; if_under = (globalize_string prefix : string)
               }]) [@nontail])
  [@exclave_if_stack a]
;;

let[@alloc a] make_relative_if_possible t ~if_under =
  (if is_relative t
   then t
   else
     (Path_string.chop_prefix [@alloc a])
       ((to_string [@alloc a]) t)
       ~prefix:((Absolute.to_string [@alloc stack]) if_under)
       ~if_some:(Expert.unchecked_of_canonical_string [@alloc a])
       ~if_none:(fun string ~prefix:_ ->
         (Expert.unchecked_of_canonical_string [@alloc a]) string [@exclave_if_stack a])
     [@nontail])
  [@exclave_if_stack a]
;;

let[@mode l] of_variant x =
  match (x : Variant.t) with
  | Relative relative -> (of_relative [@mode l]) relative [@exclave_if_local l]
  | Absolute absolute -> (of_absolute [@mode l]) absolute [@exclave_if_local l]
;;

let[@alloc a] to_variant t : Variant.t =
  (if is_absolute t
   then Absolute ((unchecked_to_absolute [@mode l]) t)
   else Relative ((unchecked_to_relative [@mode l]) t))
  [@exclave_if_stack a]
;;]
