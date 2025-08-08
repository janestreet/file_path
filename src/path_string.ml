(* Unless noted otherwise, functions should be correct for all valid, canonical path
   strings, and should be safe for all strings. In other words, given an invalid or
   non-canonical string, there should be no segfaults, but it is okay to return a nonsense
   value or raise.

   Where possible, we avoid unnecessary allocation. This means for most functions, we do
   not allocate anything other than the result, and if the result is equal to the input we
   try to avoid allocating at all.

   We use unsafe string and bytes operations where possible to improve performance.

   When an exported function has helpers, we (usually) define them in the same top-level
   [let] as the function, then define the function itself. We define the function locally
   by its own name at the end of this block of definitions, rather than just writing an
   anonymous function, because sometimes that improves backtraces.

   A lot of this logic is finicky. We rely on comprehensive tests of [File_path] to ensure
   correctness of this module. *)

open! Core

let slash_char = '/'
let slash_string = "/"
let root = slash_string
let root_length = String.length root
let dot = "."
let dot_dot = ".."
let dot_dot_length = String.length dot_dot
let null = '\000'
let char_is_valid c = not (Char.equal c null)
let char_is_slash c = Char.equal c slash_char
let char_is_valid_for_part c = char_is_valid c && not (char_is_slash c)
let is_root string = String.equal string root

(* Compare paths lexicographically as lists of parts. Compares parts inline,
   character-by-character, to avoid allocating an actual [Part.t]. *)
let%template[@mode local] compare =
  let char_value char =
    (* Comparing slash as less than all other characters gives the same effect as
       comparing a list of parts separated by slash. *)
    if char_is_slash char then -1 else Char.to_int char
  in
  let rec compare_from ~pos ~a ~b ~len_a ~len_b =
    (* First check for string end. *)
    if pos = len_a
    then if pos = len_b then 0 else -1
    else if pos = len_b
    then 1
    else (
      (* If neither string has ended, compare the next character. *)
      let char_a = String.unsafe_get a pos in
      let char_b = String.unsafe_get b pos in
      match Int.compare (char_value char_a) (char_value char_b) with
      | 0 -> compare_from ~pos:(pos + 1) ~a ~b ~len_a ~len_b
      | c -> c)
  in
  let[@mode local] compare a b =
    compare_from ~pos:0 ~a ~b ~len_a:(String.length a) ~len_b:(String.length b)
  in
  compare [@mode local]
;;

let%template compare = [%eta2 compare [@mode local]]

(* Obviously, [is_valid] must be correct for even invalid strings. *)
let is_valid =
  let is_valid string =
    (not (String.is_empty string)) && String.for_all string ~f:char_is_valid
  in
  is_valid
;;

(* Obviously, [is_canonical] must be correct for all valid strings. *)
let is_canonical =
  let rec is_canonical_from string ~pos ~len ~saw_slash =
    if pos = len
    then if saw_slash then len = root_length else true
    else (
      let is_slash = Char.equal slash_char (String.unsafe_get string pos) in
      if is_slash && saw_slash
      then false
      else is_canonical_from string ~pos:(pos + 1) ~len ~saw_slash:is_slash)
  in
  let is_canonical string =
    is_canonical_from string ~pos:0 ~len:(String.length string) ~saw_slash:false
  in
  is_canonical
;;

let is_absolute string = String.is_prefix string ~prefix:root
let is_relative string = not (is_absolute string)

let append_to_basename path ~suffix ~if_valid ~if_invalid_path ~if_invalid_suffix =
  if is_root path
  then if_invalid_path path ~suffix
  else if String.for_all suffix ~f:char_is_valid_for_part
  then if_valid (path ^ suffix)
  else if_invalid_suffix path ~suffix
;;

let append prefix suffix =
  (* Appending an absolute suffix makes no sense. *)
  assert (is_relative suffix);
  if is_root prefix
  then root ^ suffix
  else (
    (* We implement three-way concatenation (i.e. prefix + slash + suffix) longhand to
       avoid allocating intermediate results. *)
    let prefix_len = String.length prefix in
    let suffix_len = String.length suffix in
    let bytes = Bytes.create (prefix_len + 1 + suffix_len) in
    Bytes.From_string.unsafe_blit
      ~dst:bytes
      ~dst_pos:0
      ~src:prefix
      ~src_pos:0
      ~len:prefix_len;
    Bytes.unsafe_set bytes prefix_len slash_char;
    Bytes.From_string.unsafe_blit
      ~dst:bytes
      ~dst_pos:(prefix_len + 1)
      ~src:suffix
      ~src_pos:0
      ~len:suffix_len;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes)
;;

(* A helper for finding first or last slash characters. Returns -1 on failure. *)
let rec find_slash string ~pos ~stop_at ~increment =
  if pos = stop_at
  then -1
  else if char_is_slash (String.unsafe_get string pos)
  then pos
  else find_slash string ~pos:(pos + increment) ~stop_at ~increment
;;

let initial_slash_index string =
  find_slash string ~pos:0 ~stop_at:(String.length string) ~increment:1
;;

let final_slash_index string =
  find_slash string ~pos:(String.length string - 1) ~stop_at:(-1) ~increment:(-1)
;;

let basename_at string ~non_negative_final_slash_index:index =
  String.drop_prefix string (index + 1)
;;

let dirname_at string ~non_negative_final_slash_index:index =
  if index = 0 then root else String.prefix string index
;;

let basename string ~if_some ~if_none =
  if String.equal string root
  then if_none string
  else (
    let index = final_slash_index string in
    if index < 0
    then if_some string
    else if_some (basename_at string ~non_negative_final_slash_index:index))
;;

let dirname string ~if_some ~if_none =
  if String.equal string root
  then if_none string
  else (
    let index = final_slash_index string in
    if index < 0
    then if_none string
    else if_some (dirname_at string ~non_negative_final_slash_index:index))
;;

let dirname_and_basename string ~if_some ~if_none =
  if String.equal string root
  then if_none string
  else (
    let index = final_slash_index string in
    if index < 0
    then if_none string
    else
      if_some
        ~dirname:(dirname_at string ~non_negative_final_slash_index:index)
        ~basename:(basename_at string ~non_negative_final_slash_index:index))
;;

let topdir_at string ~non_negative_final_slash_index:index = String.prefix string index

let all_but_top_dir_at string ~non_negative_final_slash_index:index =
  String.drop_prefix string (index + 1)
;;

let top_dir string ~if_some ~if_none =
  (* The top directory of an absolute path is root, it is not worth an accessor. *)
  assert (is_relative string);
  let index = initial_slash_index string in
  if index < 0
  then if_none string
  else if_some (topdir_at string ~non_negative_final_slash_index:index)
;;

let all_but_top_dir string ~if_some ~if_none =
  (* All but the top directory of an absolute path is just a relative path with the
     leading slash chopped off, it is not worth an accessor. *)
  assert (is_relative string);
  let index = initial_slash_index string in
  if index < 0
  then if_none string
  else if_some (all_but_top_dir_at string ~non_negative_final_slash_index:index)
;;

let top_dir_and_all_but_top_dir string ~if_some ~if_none =
  (* As [top_dir] and [all_but_top_dir_at], does not apply to absolute paths. *)
  assert (is_relative string);
  let index = initial_slash_index string in
  if index < 0
  then if_none string
  else
    if_some
      ~top_dir:(topdir_at string ~non_negative_final_slash_index:index)
      ~all_but_top_dir:(all_but_top_dir_at string ~non_negative_final_slash_index:index)
;;

module Prefix_kind = struct
  type t =
    | Equal (* prefix without suffix *)
    | Root_prefix (* the only prefix that ends in a slash *)
    | Strict_prefix (* prefix and suffix separated by slash *)
    | Not_a_prefix
end

let prefix_kind string ~prefix : Prefix_kind.t =
  if String.is_prefix string ~prefix
  then (
    let string_len = String.length string in
    let prefix_len = String.length prefix in
    if Int.equal string_len prefix_len
    then (* [a/b] is a trivial prefix of [a/b] *)
      Equal
    else if is_root prefix
    then (* [/] is a prefix of [/a/b] *)
      Root_prefix
    else if char_is_slash (String.unsafe_get string prefix_len)
    then (* [a/b] is a prefix of [a/b/c] *)
      Strict_prefix
    else (* [a/b] is not a prefix of [a/bc] *)
      Not_a_prefix)
  else Not_a_prefix
;;

let is_prefix string ~prefix =
  match prefix_kind string ~prefix with
  | Equal | Strict_prefix | Root_prefix -> true
  | Not_a_prefix -> false
;;

let chop_prefix string ~prefix ~if_some ~if_none =
  match prefix_kind string ~prefix with
  | Equal -> if_some dot
  | Root_prefix -> if_some (String.drop_prefix string (String.length prefix))
  | Strict_prefix -> if_some (String.drop_prefix string (String.length prefix + 1))
  | Not_a_prefix -> if_none string ~prefix
;;

module Suffix_kind = struct
  type t =
    | Equal (* suffix without prefix *)
    | Suffix_of_root (* the only prefix that ends in a slash *)
    | Strict_suffix (* prefix and suffix separated by slash *)
    | Not_a_suffix
end

let suffix_kind string ~suffix : Suffix_kind.t =
  (* It makes no sense for an absolute path to be a suffix. *)
  assert (is_relative suffix);
  if String.is_suffix string ~suffix
  then (
    let string_len = String.length string in
    let suffix_len = String.length suffix in
    if Int.equal string_len suffix_len
    then (* [a/b] is a trivial suffix of [a/b] *)
      Equal
    else (
      let prefix_len = string_len - suffix_len in
      if char_is_slash (String.unsafe_get string (prefix_len - 1))
      then
        if prefix_len = 1
        then (* [a/b] is a suffix of [/a/b] *)
          Suffix_of_root
        else (* [b/c] is a suffix of [a/b/c] *)
          Strict_suffix
      else (* [b/c] is not a suffix of [ab/c] *)
        Not_a_suffix))
  else Not_a_suffix
;;

let is_suffix string ~suffix =
  match suffix_kind string ~suffix with
  | Equal | Suffix_of_root | Strict_suffix -> true
  | Not_a_suffix -> false
;;

let chop_suffix string ~suffix ~if_some ~if_none =
  match suffix_kind string ~suffix with
  | Equal -> if_some dot
  | Suffix_of_root -> if_some root
  | Strict_suffix -> if_some (String.drop_suffix string (String.length suffix + 1))
  | Not_a_suffix -> if_none string ~suffix
;;

(* [String.equal substring (String.sub string pos len)] without allocating *)
let substring_equals string ~pos ~len ~substring =
  String.length substring = len && String.is_substring_at string ~pos ~substring
;;

module Foldr_mode = struct
  (* How to normalize path parts. *)
  type t =
    | All_parts
    | Simplify_dot
    | Simplify_dot_and_dot_dot_naively
end

(* Right-to-left fold over parts in a path. *)
let foldr =
  (* Loop from [start] (inclusive), where the current part ends at [until] (exclusive). *)
  let rec foldr_up_to string ~foldr_mode ~start ~until ~depth ~state ~acc ~f =
    if start = -1 || char_is_slash (String.unsafe_get string start)
    then foldr_up_to_part_end string ~foldr_mode ~start ~until ~depth ~state ~acc ~f
    else foldr_up_to string ~foldr_mode ~start:(start - 1) ~until ~depth ~state ~acc ~f
  (* Accumulates the part between [start] and [until] (both exclusive), if applicable. *)
  and foldr_up_to_part_end string ~foldr_mode ~start ~until ~depth ~state ~acc ~f =
    let pos = start + 1 in
    let len = until - pos in
    if len = 0
    then
      (* consecutive slashes or trailing slash, no part here *)
      foldr_up_to_part_start string ~foldr_mode ~start ~depth ~state ~acc ~f
    else (
      match (foldr_mode : Foldr_mode.t) with
      | (Simplify_dot | Simplify_dot_and_dot_dot_naively)
        when substring_equals string ~pos ~len ~substring:dot ->
        (* discard [.] *)
        foldr_up_to_part_start string ~foldr_mode ~start ~depth ~state ~acc ~f
      | Simplify_dot_and_dot_dot_naively
        when substring_equals string ~pos ~len ~substring:dot_dot ->
        (* record a [..] to be canceled *)
        let depth = depth + 1 in
        foldr_up_to_part_start string ~foldr_mode ~start ~depth ~state ~acc ~f
      | Simplify_dot_and_dot_dot_naively when depth > 0 ->
        (* cancel a [..] *)
        let depth = depth - 1 in
        foldr_up_to_part_start string ~foldr_mode ~start ~depth ~state ~acc ~f
      | All_parts | Simplify_dot | Simplify_dot_and_dot_dot_naively ->
        (* accumulate a part normally *)
        let acc = f string ~pos ~len ~state ~acc in
        foldr_up_to_part_start string ~foldr_mode ~start ~depth ~state ~acc ~f)
  (* Resumes processing after (possibly) accumulating a part. *)
  and foldr_up_to_part_start string ~foldr_mode ~start ~depth ~state ~acc ~f =
    if start = -1
    then
      if is_absolute string
      then (* [/..] = [/] *) acc
      else foldr_dot_dots ~depth ~state ~acc ~f
    else (
      let start = start - 1
      and until = start in
      foldr_up_to string ~foldr_mode ~start ~until ~depth ~state ~acc ~f)
  (* Accumulates uncanceled [..]s. *)
  and foldr_dot_dots ~depth ~state ~acc ~f =
    if depth = 0
    then acc
    else (
      let depth = depth - 1 in
      let acc = f dot_dot ~pos:0 ~len:dot_dot_length ~state ~acc in
      foldr_dot_dots ~depth ~state ~acc ~f)
  in
  let foldr string ~foldr_mode ~state ~acc ~f =
    let len = String.length string in
    foldr_up_to string ~foldr_mode ~start:(len - 1) ~until:len ~depth:0 ~state ~acc ~f
  in
  foldr
;;

(* Rewrites a string using a [Foldr.t], concatenating all of the folded parts. Produces
   [root] or [dot] if there are no parts, depending on whether the path is absolute or
   relative. Assumes the [Foldr.t] accumulates a subset of the input's parts, and returns
   the original string if the result has the same length as the input. *)
let rewrite =
  (* Counts the characters in a part plus its preceding slash. *)
  let count_slash_part _ ~pos:_ ~len ~state:() ~acc = 1 + len + acc in
  (* Writes a part into a [Bytes.t], with a preceding slash if it fits. *)
  let write_slash_part src ~pos:src_pos ~len ~state:dst ~acc:dst_pos =
    let dst_pos = dst_pos - len in
    Bytes.From_string.unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len;
    if dst_pos = 0
    then dst_pos
    else (
      let dst_pos = dst_pos - 1 in
      Bytes.unsafe_set dst dst_pos slash_char;
      dst_pos)
  in
  let rewrite foldr_mode string =
    (* For relative strings, we decrement the length to effectively remove the leading
       slash of the first part. *)
    let len_with_leading_slash =
      foldr string ~foldr_mode ~state:() ~acc:0 ~f:count_slash_part
    in
    if len_with_leading_slash = 0
    then if is_absolute string then root else dot
    else (
      let len =
        if is_absolute string then len_with_leading_slash else len_with_leading_slash - 1
      in
      if len = String.length string
      then (* If the path is unchanged, do not allocate a copy. *)
        string
      else (
        let bytes = Bytes.create len in
        let pos = foldr string ~foldr_mode ~state:bytes ~acc:len ~f:write_slash_part in
        (* Make sure the two character counts line up. *)
        assert (pos = 0);
        Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes))
  in
  rewrite
;;

let canonicalize = rewrite All_parts
let simplify_dot = rewrite Simplify_dot
let simplify_dot_and_dot_dot_naively = rewrite Simplify_dot_and_dot_dot_naively

let to_parts =
  let add_part string ~pos ~len ~state:part_of_string ~acc =
    let part_string = String.sub string ~pos ~len in
    part_of_string part_string :: acc
  in
  let to_parts string ~part_of_string =
    foldr string ~foldr_mode:All_parts ~state:part_of_string ~acc:[] ~f:add_part
  in
  to_parts
;;

let of_parts_relative parts ~if_some ~if_none =
  if List.is_empty parts
  then if_none ()
  else if_some (String.concat ~sep:slash_string parts)
;;

let of_parts_absolute parts =
  if List.is_empty parts
  then root
  else
    (* Adding an empty string to the parts is a bit of a hack to force a leading slash,
       and is also a small amount of intermediate allocation. This is still cleaner and
       simpler than reimplementing all of [String.concat] just to improve this. *)
    String.concat ~sep:slash_string ("" :: parts)
;;

let number_of_parts =
  let count_part _ ~pos:_ ~len:_ ~state:() ~acc = acc + 1 in
  let number_of_parts string =
    foldr string ~foldr_mode:All_parts ~state:() ~acc:0 ~f:count_part
  in
  number_of_parts
;;

module Quickcheckable_part = struct
  type t = string

  let char_generator =
    (Base_quickcheck.Generator.filter [@mode portable])
      Base_quickcheck.Generator.char
      ~f:(function
      | '/' | '\000' -> false
      | _ -> true)
  ;;

  let quickcheck_generator =
    (Base_quickcheck.Generator.union [@mode portable])
      [ (Base_quickcheck.Generator.string_non_empty_of [@mode portable]) char_generator
      ; (Base_quickcheck.Generator.of_list [@mode portable]) [ dot; dot_dot ]
      ]
  ;;

  let quickcheck_observer = Base_quickcheck.Observer.string
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Quickcheckable_relative = struct
  type t = string

  include
    Quickcheckable.Of_quickcheckable_filtered [@mode portable]
      (struct
        type t = Quickcheckable_part.t list [@@deriving quickcheck ~portable]
      end)
      (struct
        type t = string

        let to_quickcheckable t = to_parts t ~part_of_string:Fn.id

        let of_quickcheckable list =
          of_parts_relative
            list
            ~if_some:(fun string -> Some string)
            ~if_none:(fun () -> None)
        ;;
      end)
end

module Quickcheckable_absolute = struct
  type t = string

  include
    Quickcheckable.Of_quickcheckable [@mode portable]
      (struct
        type t = Quickcheckable_part.t list [@@deriving quickcheck ~portable]
      end)
      (struct
        type t = string

        let to_quickcheckable t = to_parts t ~part_of_string:Fn.id
        let of_quickcheckable list = of_parts_absolute list
      end)
end

module Quickcheckable_path = struct
  type t = string

  include
    Quickcheckable.Of_quickcheckable [@mode portable]
      (struct
        type t = (Quickcheckable_relative.t, Quickcheckable_absolute.t) Either.t
        [@@deriving quickcheck ~portable]
      end)
      (struct
        type t = string

        let to_quickcheckable t = if is_relative t then First t else Second t
        let of_quickcheckable = Either.value
      end)
end
