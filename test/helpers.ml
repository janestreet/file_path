open! Core
open Expect_test_helpers_core
include Helpers_intf

let () =
  (* [File_path] has verbose tests that read better with less wasted vertical space. The
     extra alignment and so forth in the default style is not worth it. *)
  Dynamic.set_root sexp_style Sexp_style.simple_pretty
;;

let quickcheck =
  (* We run a lot of tests on this library, mostly of shallow properties. We don't need
     terribly extensive coverage. It's better to save time in continuous integration. *)
  quickcheck ~trials:1000
;;

let test_constants (type t) (module Path_type : Path_type with type t = t) list =
  List.iter list ~f:(fun t ->
    print_s [%sexp (t : Path_type.t)];
    require_does_not_raise (fun () -> Path_type.invariant t))
;;

let test_compare (type t) (module Path_type : Path_type with type t = t) list =
  let test ~verbose (list, permuted) =
    let sorted = List.sort ~compare:Path_type.compare list in
    if verbose then print_s [%sexp (sorted : Path_type.t list)];
    let resorted = List.sort permuted ~compare:Path_type.compare in
    require_equal
      (module struct
        type t = Path_type.t list [@@deriving equal, sexp_of]
      end)
      sorted
      resorted
      ~message:"inconsistent sorting"
  in
  test ~verbose:true (list, List.rev list);
  quickcheck
    ~sexp_of:[%sexp_of: Path_type.t list * Path_type.t list]
    ~shrinker:[%quickcheck.shrinker: Path_type.t list * Path_type.t list]
    (let%bind.Quickcheck list = [%quickcheck.generator: Path_type.t list] in
     let%map.Quickcheck permuted = List.gen_permutations list in
     list, permuted)
    ~f:(test ~verbose:false)
;;

let test_containers (type t) (module Path_type : Path_type with type t = t) list =
  print_and_check_container_sexps
    (module Path_type)
    (List.sort list ~compare:Path_type.compare)
;;

let test_of_string (module Path_type : Path_type) strings =
  let test ~verbose string =
    match Path_type.of_string string with
    | exception exn -> if verbose then print_s [%sexp "!", (exn : exn)]
    | t ->
      let round_trip = require_no_allocation (fun () -> Path_type.to_string t) in
      if verbose
      then
        if String.equal string round_trip
        then print_s [%sexp "=", (string : string)]
        else print_s [%sexp "~", (string : string), (round_trip : string)];
      require_does_not_raise (fun () -> Path_type.invariant t);
      if String.equal string round_trip
      then (
        require
          (phys_equal string round_trip)
          ~if_false_then_print_s:(lazy [%message "unnecessarily copied string"]);
        require_no_allocation (fun () ->
          ignore (Sys.opaque_identity (Path_type.of_string string) : Path_type.t)));
      let t_round_trip =
        require_no_allocation (fun () -> Path_type.of_string round_trip)
      in
      require_equal
        (module Path_type)
        t
        t_round_trip
        ~if_false_then_print_s:
          (lazy [%message "to_string -> of_string does not round-trip"])
  in
  List.iter strings ~f:(test ~verbose:true);
  quickcheck
    ~sexp_of:String.sexp_of_t
    String.quickcheck_generator
    ~shrinker:String.quickcheck_shrinker
    ~f:(test ~verbose:false)
;;

let test_invariant (module Path_type : Path_type) strings =
  let test ~verbose string =
    let t_unchecked = Path_type.Expert.unchecked_of_canonical_string string in
    match Path_type.invariant t_unchecked with
    | () ->
      if verbose then print_s [%sexp "=", (t_unchecked : Path_type.t)];
      require_compare_equal
        (module Path_type)
        t_unchecked
        (Path_type.of_string string)
        ~message:"[unchecked_of_canonical_string] and [of_string] are inconsistent"
    | exception exn ->
      if verbose then print_s ~hide_positions:true [%sexp "!", (exn : exn)];
      (match Path_type.of_string string with
       | exception _ -> ()
       | t ->
         if String.equal string (Path_type.to_string t)
         then
           print_cr
             [%sexp
               "[invariant] and [of_string] are inconsistent"
               , { string : string; t : Path_type.t; exn : exn }]
         else
           (* non-canonical string, it's okay if [invariant] raised but [of_string]
              succeeds *)
           ())
  in
  List.iter strings ~f:(test ~verbose:true);
  quickcheck
    ~sexp_of:String.sexp_of_t
    String.quickcheck_generator
    ~shrinker:String.quickcheck_shrinker
    ~f:(test ~verbose:false)
;;

let test_predicate
  (type input)
  ~input:(module Input : Type with type t = input)
  ~examples
  ~correctness
  predicate
  =
  let success, failure = List.partition_tf examples ~f:predicate in
  print_s [%sexp { success : Input.t list; failure : Input.t list }];
  if List.is_empty success then print_cr [%sexp "did not produce [true]"];
  if List.is_empty failure then print_cr [%sexp "did not produce [false]"];
  List.iter success ~f:(fun x -> correctness x true);
  List.iter failure ~f:(fun x -> correctness x false);
  quickcheck
    ~sexp_of:Input.sexp_of_t
    Input.quickcheck_generator
    ~shrinker:Input.quickcheck_shrinker
    ~f:(fun x -> correctness x (predicate x))
;;

let test
  (type input output)
  ~input:(module Input : Type with type t = input)
  ~output:(module Output : Type with type t = output)
  ~examples
  ~correctness
  function_to_test
  =
  assert (not (List.is_empty examples));
  let test ~verbose example =
    let result = function_to_test example in
    if verbose then print_s [%sexp (example : Input.t), "->", (result : Output.t)];
    require_does_not_raise (fun () -> Output.invariant result);
    correctness example result
  in
  List.iter examples ~f:(test ~verbose:true);
  quickcheck
    ~sexp_of:Input.sexp_of_t
    Input.quickcheck_generator
    ~shrinker:Input.quickcheck_shrinker
    ~f:(test ~verbose:false)
;;

module Bin_shape_universe = struct
  type t = { bin_shape_digests : Source_code_position.t String.Table.t }

  let create () = { bin_shape_digests = String.Table.create () }
  let default = lazy (create ())

  let test_bin_shape ?name ?(quiet = false) t bin_shape here =
    let bin_shape_digest = Bin_prot.Shape.eval_to_digest_string bin_shape in
    Hashtbl.update t.bin_shape_digests bin_shape_digest ~f:(function
      | None ->
        if not quiet
        then
          print_s
            [%sexp { name : (string option[@sexp.option]); bin_shape_digest : string }];
        here
      | Some where ->
        print_cr
          ~here
          [%sexp
            "duplicate bin_shape_digest"
            , { name : (string option[@sexp.option])
              ; bin_shape_digest : string
              ; where : Source_code_position.t
              }];
        where)
  ;;

  let test_container_bin_shape ?name ?quiet t container_bin_shape here =
    test_bin_shape ?name ?quiet t (container_bin_shape Int.bin_shape_t) here
  ;;
end

let test_stable_version
  (type t)
  ?(bin_shape_universe = force Bin_shape_universe.default)
  here
  (module Version : Version with type t = t)
  list
  =
  Bin_shape_universe.test_bin_shape
    bin_shape_universe
    Version.bin_shape_t
    here
    ~quiet:true;
  print_and_check_stable_type ~here (module Version) list
;;

let test_stable_containers
  (type t)
  ?(bin_shape_universe = force Bin_shape_universe.default)
  here
  (module Version : Version with type t = t)
  list
  =
  Bin_shape_universe.test_container_bin_shape
    bin_shape_universe
    Version.Map.bin_shape_t
    here
    ~name:"Map";
  Bin_shape_universe.test_bin_shape
    bin_shape_universe
    Version.Set.bin_shape_t
    here
    ~name:"Set";
  Bin_shape_universe.test_container_bin_shape
    bin_shape_universe
    Version.Table.bin_shape_t
    here
    ~name:"Table";
  Bin_shape_universe.test_bin_shape
    bin_shape_universe
    Version.Hash_set.bin_shape_t
    here
    ~name:"Hash_set";
  print_and_check_container_sexps ~here (module Version) list
;;

module Option (Type : Type) = struct
  type t = Type.t option [@@deriving compare, equal, quickcheck, sexp_of]

  let invariant = Option.invariant Type.invariant
end

module List (Type : Type) = struct
  type t = Type.t list [@@deriving compare, equal, quickcheck, sexp_of]

  let invariant = List.invariant Type.invariant
end

module Nonempty_list (Type : Type) = struct
  type t = Type.t Nonempty_list.t [@@deriving compare, equal, quickcheck, sexp_of]

  let invariant = Nonempty_list.invariant Type.invariant
end

module With_prefix (Type : Type) = struct
  type t = Type.t Examples.With_prefix.t [@@deriving compare, equal, quickcheck, sexp_of]

  let invariant = Examples.With_prefix.invariant Type.invariant
end

module With_suffix (Type : Type) = struct
  type t = Type.t Examples.With_suffix.t [@@deriving compare, equal, quickcheck, sexp_of]

  let invariant = Examples.With_suffix.invariant Type.invariant
end

module Tuple2 (A : Type) (B : Type) = struct
  type t = A.t * B.t [@@deriving compare, equal, quickcheck, sexp_of]

  let invariant (a, b) =
    A.invariant a;
    B.invariant b
  ;;
end

module Or_error (Type : Type) = struct
  type t = Type.t Or_error.t [@@deriving compare, equal, sexp_of]

  let invariant = Or_error.invariant Type.invariant

  let of_option = function
    | Some t -> Ok t
    | None -> Error (Error.of_string "error")
  ;;

  let to_option = Or_error.ok

  let quickcheck_generator =
    [%quickcheck.generator: Type.t option] |> Quickcheck.Generator.map ~f:of_option
  ;;

  let quickcheck_observer =
    [%quickcheck.observer: Type.t option] |> Quickcheck.Observer.unmap ~f:to_option
  ;;

  let quickcheck_shrinker =
    [%quickcheck.shrinker: Type.t option]
    |> Quickcheck.Shrinker.map ~f:of_option ~f_inverse:to_option
  ;;
end
