(* Our implementation of command-line completion simulates bash's tab-completion as
   closely as we can, restricting output based on the relevant [File_path] type.

   We are not as concerned with low-level performance in this module as in [Path_string],
   since the cost of fork/exec and i/o likely dominates the cost of auto-completion. *)

open! Core
include Completion_intf

module Sys = struct
  let getenv = Sys.getenv

  open struct
    (* For testing, do all i/o relative to $ROOT_FOR_FILE_PATH_TESTING. *)
    let rooted path =
      if String.is_prefix path ~prefix:"/"
      then (
        match getenv "ROOT_FOR_FILE_PATH_TESTING" with
        | Some root -> root ^ path
        | None -> path)
      else path
    ;;
  end

  let readdir path =
    let path = rooted path in
    try Stdlib.Sys.readdir path with
    | (_ : exn) -> [||]
  ;;

  let is_directory path =
    let path = rooted path in
    try Stdlib.Sys.is_directory path with
    | (_ : exn) -> false
  ;;
end

module Compgen = struct
  (* This module simulates [compgen -f] from [bash], closely enough for our purposes. *)

  let rec remove_duplicate_slashes string =
    if String.is_substring string ~substring:"//"
    then
      remove_duplicate_slashes (String.substr_replace_all string ~pattern:"//" ~with_:"/")
    else string
  ;;

  let split_dir_and_name string =
    match String.rsplit2 string ~on:'/' with
    | None -> None, string
    | Some (dir, part) ->
      let maybe_dir = if String.is_empty dir then Some "/" else Some dir in
      maybe_dir, part
  ;;

  let matching_names dir ~part =
    Sys.readdir dir
    |> Array.to_list
    |> List.append (if String.is_prefix part ~prefix:"." then [ "."; ".." ] else [])
    |> List.filter ~f:(String.is_prefix ~prefix:part)
    |> List.sort ~compare:String.compare
  ;;

  let run arg =
    let arg = remove_duplicate_slashes arg in
    let maybe_dir, part = split_dir_and_name arg in
    let dir = Option.value maybe_dir ~default:"." in
    let names = matching_names dir ~part in
    match maybe_dir with
    | None -> names
    | Some dir -> List.map names ~f:(fun string -> Filename.concat dir string)
  ;;
end

module Escaping = struct
  (* character set borrowed from unix implementation of [Core.Sys.quote], 2020-11. *)
  let must_escape_char = function
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_' | '-' | ':' | '.' | '/' | ',' | '+' | '=' | '%' | '@' -> false
    | _ -> true
  ;;

  let escape_char char =
    String.of_char_list (if must_escape_char char then [ '\\'; char ] else [ char ])
  ;;

  let escape string = String.concat_map string ~f:escape_char

  (* bash's tab-completion behaves oddly with escaping via quotes, so we only bother to
     escape and unescape via backslashes. *)

  let rec unescape_chars_loop chars ~acc =
    match chars with
    | [] -> acc
    | '\\' :: escaped ->
      (match escaped with
       | '\n' :: rest ->
         (* backslash + newline just continues on the next line *)
         unescape_chars_loop rest ~acc
       | char :: rest ->
         (* any other character is escaped *)
         unescape_chars_loop rest ~acc:(char :: acc)
       | [] -> acc)
    | char :: rest -> unescape_chars_loop rest ~acc:(char :: acc)
  ;;

  let unescape_permissive string =
    string
    |> String.to_list
    |> unescape_chars_loop ~acc:[]
    |> List.rev
    |> String.of_char_list
  ;;
end

let add_slash_if_directory string =
  if String.is_suffix string ~suffix:"/"
  then string
  else if Sys.is_directory string
  then string ^ "/"
  else string
;;

let add_slashes_to_directories strings ~allow_trailing_slash =
  if allow_trailing_slash then List.map strings ~f:add_slash_if_directory else strings
;;

let basename_preserving_slash string =
  let path, slash =
    match String.chop_suffix string ~suffix:"/" with
    | Some prefix -> prefix, true
    | None -> string, false
  in
  match String.rsplit2 path ~on:'/' with
  | Some (_, name) -> if slash then name ^ "/" else name
  | None -> string
;;

let ensure_no_common_prefix strings =
  if String.common_prefix_length strings = 0
  then strings
  else
    (* This is an unpleasant hack, but it works around a bash heuristic.

       Let's say we are autocompleting "/bin/f", and the matching paths are "/bin/find"
       and "/bin/fgrep". We want to present the user with the choices "find" and "fgrep".

       If we do that naively, bash will compare the argument "/bin/f" with the common
       prefix of the choices, "f". Since the common prefix is non-empty and differs from
       the argument, bash will assume the argument should be replaced before presenting
       any menu, and it replaces the whole argument with "f".

       Bash doesn't do this with its own native completion, but unfortunately there
       doesn't appear to be a way to turn off this part of the heuristic from inside
       custom completion.

       The arg type in [Core.Filename] works around this by using the full path. This
       makes the menus verbose and repetitive.

       Here, we present the options "", "find", and "fgrep". Bash finds no common prefix,
       so it presents the menu, with some extra junk whitespace due to the empty entry.
       It's slightly ugly but it's the closest behavior we can get to bash's native "just
       show the basenames" completion menus. *)
    "" :: strings
;;

let complete_empty_result ~arg =
  if Sys.is_directory arg
  then (* empty directory, complete the argument *)
    [ arg ]
  else (* nonexistent path or nonsense input, do nothing *)
    []
;;

let complete_single_result string =
  if String.is_suffix string ~suffix:"/"
  then
    (* extend the argument to include the slash, allow further completion *)
    [ string; string ^ "." ]
  else (* complete the argument, this is not a directory *)
    [ string ]
;;

let complete_multiple_results list ~arg =
  assert (List.length list > 1);
  let common_prefix = String.common_prefix list in
  if String.is_prefix common_prefix ~prefix:arg && not (String.equal common_prefix arg)
  then (* extend the argument to include the common prefix *)
    list
  else
    (* present a menu of options, basenames only *)
    list
    |> List.map ~f:basename_preserving_slash
    |> List.sort ~compare:String.compare
    |> ensure_no_common_prefix
;;

let complete_generic arg ~allow_trailing_slash =
  match arg |> Compgen.run |> add_slashes_to_directories ~allow_trailing_slash with
  | [] -> complete_empty_result ~arg
  | [ path ] -> complete_single_result path
  | multiple -> complete_multiple_results multiple ~arg
;;

let is_absolute_path_like string =
  String.is_prefix string ~prefix:"/" || String.is_prefix string ~prefix:"~"
;;

let complete_path_unescaped arg = complete_generic arg ~allow_trailing_slash:true

let complete_absolute_unescaped arg =
  if String.is_empty arg
  then complete_single_result "/"
  else if is_absolute_path_like arg
  then complete_generic arg ~allow_trailing_slash:true
  else []
;;

let complete_relative_unescaped arg =
  if is_absolute_path_like arg
  then []
  else complete_generic arg ~allow_trailing_slash:true
;;

let complete_part_unescaped arg =
  if is_absolute_path_like arg || String.mem arg '/'
  then []
  else complete_generic arg ~allow_trailing_slash:false
;;

(* strip escaping; strip and record use of [~] as [$HOME] *)
let translate original =
  let unescaped = Escaping.unescape_permissive original in
  match Sys.getenv "HOME" with
  | Some home
  (* only treat unescaped [~] as meaning [$HOME] *)
    when String.is_prefix original ~prefix:"~" ->
    if String.equal unescaped "~"
    then Some home, home
    else (
      match String.chop_prefix unescaped ~prefix:"~/" with
      | Some suffix -> Some home, Filename.concat home suffix
      | _ -> None, unescaped)
  | _ -> None, unescaped
;;

(* restore any necessary escaping and use of [~] as [$HOME] *)
let untranslate ~home string =
  match home with
  | None -> Escaping.escape string
  | Some prefix ->
    if String.equal string prefix
    then "~"
    else (
      match String.chop_prefix string ~prefix:(prefix ^ "/") with
      | Some suffix -> "~/" ^ Escaping.escape suffix
      | None -> Escaping.escape string)
;;

let with_escaping complete arg =
  let home, arg = translate arg in
  List.map (complete arg) ~f:(untranslate ~home)
;;

let complete_path = with_escaping complete_path_unescaped
let complete_absolute = with_escaping complete_absolute_unescaped
let complete_relative = with_escaping complete_relative_unescaped
let complete_part = with_escaping complete_part_unescaped
