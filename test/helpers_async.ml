open! Core
open! Async
open! Expect_test_helpers_core
open! Expect_test_helpers_async
include Helpers_async_intf

let populate paths =
  Deferred.List.iter ~how:`Sequential paths ~f:(fun path ->
    if String.is_suffix path ~suffix:"/"
    then run "mkdir" [ "-p"; path ]
    else (
      let%bind () = run "mkdir" [ "-p"; Filename.dirname path ] in
      run "touch" [ path ]))
;;

let compare_by_number_of_slashes a b =
  Comparable.lift Int.compare ~f:(String.count ~f:(Char.equal '/')) a b
;;

let apply_if_changed f x =
  let y = f x in
  if String.equal x y then None else Some y
;;

let replace_path_prefix string ~pattern ~with_ =
  match String.chop_prefix string ~prefix:pattern with
  | None -> string
  | Some suffix ->
    if String.is_empty suffix || String.is_prefix suffix ~prefix:"/"
    then with_ ^ suffix
    else string
;;

let home_to_tilde string = replace_path_prefix string ~pattern:"/home" ~with_:"~"
let tilde_to_home string = replace_path_prefix string ~pattern:"~" ~with_:"/home"

let escape string =
  (* Brute force conversion of [Sys.quote]-style escaping to backslash escaping. *)
  String.concat_map string ~f:(fun char ->
    let raw = String.of_char char in
    if String.equal raw (Sys.quote raw) then raw else "\\" ^ raw)
;;

let unescape string =
  match
    List.fold (String.to_list string) ~init:(`Normal, []) ~f:(fun (mode, acc) char ->
      match mode, char with
      | `Normal, '\\' -> `Backslash, acc
      | `Normal, char -> `Normal, char :: acc
      | `Backslash, char -> `Normal, char :: acc)
  with
  | `Normal, acc -> Ok (String.of_char_list (List.rev acc))
  | `Backslash, _ -> error_s [%sexp "unterminated backslash escape"]
;;

let with_double_slash_example paths =
  List.concat
    [ paths
    ; paths
      |> List.max_elt ~compare:compare_by_number_of_slashes
      |> Option.map ~f:(String.substr_replace_all ~pattern:"/" ~with_:"//")
      |> Option.to_list
    ]
;;

let with_absolute_examples paths =
  List.concat [ paths; List.map paths ~f:(sprintf "/%s") ]
;;

let with_home_examples paths =
  List.concat [ paths; List.filter_map paths ~f:(apply_if_changed home_to_tilde) ]
;;

let every_prefix string =
  List.init (String.length string + 1) ~f:(fun length -> String.prefix string length)
;;

let with_all_prefixes strings =
  strings
  |> List.concat_map ~f:every_prefix
  |> List.dedup_and_sort ~compare:String.compare
;;

let with_escaped strings =
  List.concat
    [ List.map strings ~f:escape
    ; List.find strings ~f:(fun unescaped ->
        let escaped = escape unescaped in
        not (String.equal escaped unescaped))
      |> Option.to_list
    ]
;;

let args_of_paths paths =
  paths
  |> with_double_slash_example
  |> with_escaped
  |> with_absolute_examples
  |> with_home_examples
  |> with_all_prefixes
;;

(* This is a messy and inefficient way to capture the stdout output of a command. It is
   currently handy because [Command_test_helpers] only tests completion by printing. It
   would be handier to just get a string list as output. *)
let output_of ~expect_output f =
  let prior_output = expect_output () in
  let result = Or_error.try_with f in
  let pending_output = expect_output () in
  print_string prior_output;
  match result with
  | Ok () -> pending_output
  | Error error ->
    error |> Error.tag_s ~tag:[%sexp { pending_output : string }] |> Error.raise
;;

let completions param arg ~expect_output =
  (* [Command_test_helpers.complete] prints each completion on a line, followed by a final
     line containing the exit status of the simulated command. *)
  output_of ~expect_output (fun () -> Command_test_helpers.complete param ~args:[ arg ])
  |> String.split_lines
  |> List.drop_last_exn
;;

let with_env ~key ~data f =
  let original = Unix.getenv key in
  (Unix.putenv [@ocaml.alert "-unsafe_multidomain"]) ~key ~data;
  Exn.protect ~f ~finally:(fun () ->
    match original with
    | None -> (Unix.unsetenv [@ocaml.alert "-unsafe_multidomain"]) key
    | Some string -> (Unix.putenv [@ocaml.alert "-unsafe_multidomain"]) ~key ~data:string)
;;

let rec remove_duplicate_slashes string =
  if String.is_substring string ~substring:"//"
  then
    remove_duplicate_slashes (String.substr_replace_all string ~pattern:"//" ~with_:"/")
  else string
;;

let validate (type a) (module Path : Path with type t = a) string =
  Or_error.try_with (fun () -> ignore (Path.of_string string : Path.t))
;;

module Bash_action = struct
  type t =
    | Empty
    | Choose of string list
    | Extend of string
    | Finish of string
  [@@deriving equal]

  (* For readability, we uniformly quote printed inputs and outputs of completion using
     [sprintf "%S"]. *)
  let to_string_hum = function
    | Empty -> "Empty"
    | Choose strings ->
      strings
      |> List.map ~f:(sprintf "%S")
      |> String.concat ~sep:", "
      |> sprintf "Choose: %s"
    | Extend string -> sprintf "Extend: %S" string
    | Finish string -> sprintf "Finish: %S" string
  ;;

  (* Simulates bash's heuristics after running a custom completion script. *)
  let of_completion ~arg completion =
    match completion with
    | [] -> Empty
    | [ single ] -> Finish (single : string)
    | multiple ->
      let prefix = String.common_prefix multiple in
      if String.is_empty prefix || String.equal prefix arg
      then Choose (multiple : string list)
      else Extend (prefix : string)
  ;;

  let check_no_slash_except_trailing_slash names =
    require
      (List.for_all names ~f:(fun name ->
         not (String.mem (String.chop_suffix_if_exists name ~suffix:"/") '/')))
      ~if_false_then_print_s:(lazy [%sexp "menu item contains non-trailing slash"])
  ;;

  let check_completion_extends_input ~escaped_inputs ~output =
    List.find escaped_inputs ~f:(fun escaped_input ->
      match unescape escaped_input with
      | Error _ -> false
      | Ok input -> not (String.is_prefix output ~prefix:(remove_duplicate_slashes input)))
    |> Option.iter ~f:(fun input ->
      print_cr [%sexp "completion changed the input", (input : string)])
  ;;

  let check path_m action ~args =
    match action with
    | Empty -> ()
    | Choose names ->
      (match names |> List.map ~f:unescape |> Or_error.combine_errors with
       | Error error -> print_cr [%sexp "invalid escape or quotation", (error : Error.t)]
       | Ok unescaped_names -> check_no_slash_except_trailing_slash unescaped_names)
    | Extend string | Finish string ->
      (match unescape string with
       | Error error -> print_cr [%sexp "invalid escape or quotation", (error : Error.t)]
       | Ok unescaped ->
         (match validate path_m (tilde_to_home unescaped) with
          | Error error -> print_cr [%sexp "invalid completion", (error : Error.t)]
          | Ok () -> check_completion_extends_input ~escaped_inputs:args ~output:unescaped))
  ;;
end

let should_print_action action = not (Bash_action.equal action Empty)

let should_print_string path_m string =
  String.is_empty string || Result.is_ok (validate path_m (tilde_to_home string))
;;

let should_print path_m strings action =
  should_print_action action || List.exists strings ~f:(should_print_string path_m)
;;

let complete_arg param arg ~tmp ~expect_output =
  with_env ~key:"ROOT_FOR_FILE_PATH_TESTING" ~data:tmp (fun () ->
    with_env ~key:"HOME" ~data:"/home" (fun () ->
      arg |> completions param ~expect_output |> Bash_action.of_completion ~arg))
;;

(* any list six or longer, show first two + ellipsis + last two *)
let with_ellipsis list ~ellipsis =
  match list, List.rev list with
  | ( head1 :: head2 :: _head3 :: _head4 :: _head5 :: _head6 :: _
    , tail1 :: tail2 :: _tail3 :: _tail4 :: _tail5 :: _tail6 :: _ ) ->
    [ head1; head2; ellipsis; tail2; tail1 ]
  | _ -> list
;;

let complete_paths path_m param paths ~tmp ~expect_output =
  paths
  |> args_of_paths
  |> List.map ~f:(fun arg -> complete_arg param arg ~tmp ~expect_output, arg)
  |> List.Assoc.group ~equal:Bash_action.equal
  |> List.iter ~f:(fun (action, args) ->
    if should_print path_m args action
    then (
      print_newline ();
      args
      |> List.map ~f:(sprintf "%S") (* quote as in [Bash_action.to_string_hum] *)
      |> with_ellipsis ~ellipsis:"..."
      |> List.iter ~f:print_endline;
      print_endline (Bash_action.to_string_hum action));
    Bash_action.check path_m action ~args)
;;

let paths =
  [ ".fe/"
  ; "app/tool/jbuild"
  ; "app/tool/tool.ml"
  ; "app/tool/tool.mli"
  ; "app/tool/tool_intf.ml"
  ; "lib/code/jbuild"
  ; "lib/code/code.ml"
  ; "lib/code/code.mli"
  ; "lib/code/code_intf.ml"
  ; "libmap.sexp"
  ; "home/file"
  ; "home/dir/"
  ; "home/s\\ash/"
  ; "home/qu\"te/"
  ; "home/sp ce/"
  ; "home/t'ck/"
  ]
;;

let test_arg_type (type a) (module Path : Path with type t = a) ~expect_output =
  within_temp_dir (fun () ->
    let%bind tmp = Sys.getcwd () in
    let%bind () = populate paths in
    let param =
      let open Command.Param in
      anon ("PATH" %: Path.arg_type)
    in
    complete_paths (module Path) param paths ~tmp ~expect_output;
    return ())
;;
