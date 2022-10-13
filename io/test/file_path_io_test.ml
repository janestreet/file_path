open! Core
open! Async
open Expect_test_helpers_core
open Expect_test_helpers_async
open File_path.Operators

module type IO = sig
  include File_path_io.S
  include Monad.S with type 'a t := 'a io

  (** Convert I/O computations to a deferred. *)
  val async : (unit -> 'a io) -> 'a Deferred.t
end

module Test_file_path_io (IO : IO) : File_path_io.S with type 'a io := 'a IO.io = struct
  let executable_name = IO.executable_name

  let%expect_test "[executable_name]" =
    require_equal
      [%here]
      (module String)
      Sys.executable_name
      (File_path.to_string (force IO.executable_name));
    [%expect {| |}];
    return ()
  ;;

  let default_temp_dir = IO.default_temp_dir

  let%expect_test "[default_temp_dir]" =
    require_equal
      [%here]
      (module String)
      Filename.temp_dir_name
      (File_path.to_string (force IO.default_temp_dir));
    [%expect {| |}];
    return ()
  ;;

  let read_file = IO.read_file
  let write_file = IO.write_file

  let%expect_test "[read_file] and [write_file]" =
    within_temp_dir (fun () ->
      let path = File_path.of_string "file.txt" in
      let write_contents = "you do the hokey pokey and you turn yourself around" in
      let%bind () = IO.async (fun () -> write_file path ~contents:write_contents) in
      let%bind read_contents = IO.async (fun () -> read_file path) in
      require_equal [%here] (module String) write_contents read_contents;
      [%expect {| |}];
      return ())
  ;;

  open struct
    (* helpers for testing write atomicity *)

    let test_atomic_write_once ~size ~iteration =
      let path = File_path.of_string (sprintf "file.%d.%d.txt" size iteration) in
      let check_atomic =
        Deferred.repeat_until_finished () (fun () ->
          (* stat the file until it exists *)
          match%map
            Monitor.try_with ~extract_exn:true (fun () ->
              Unix.stat (File_path.to_string path))
          with
          (* if it exists, check its length *)
          | Ok stat ->
            if Int64.equal stat.size (Int64.of_int size)
            then (* full size? write appears to be atomic *)
              `Finished (Ok ())
            else
              (* partial size? write is not atomic *)
              `Finished
                (Or_error.error_s
                   [%message
                     "write was not atomic"
                       ~expect_size:(size : int)
                       ~actual_size:(stat.size : int64)])
          (* doesn't exist yet? retry *)
          | Error (Unix.Unix_error (ENOENT, _, _)) -> `Repeat ()
          (* fail on any other kind of error *)
          | Error exn ->
            `Finished (Or_error.error_s [%message "unexpected exception" (exn : exn)]))
      in
      let write_file =
        (* write to the file concurrently with checking atomicity *)
        IO.async (fun () -> write_file path ~contents:(String.make size '.'))
      in
      let%map result = check_atomic
      and () = write_file in
      result
    ;;
  end

  let%expect_test "[write_file] atomically" =
    (* try writing files of size 1024 through 1048576 until we hit a failure *)
    let min_size_pow2 = 10 in
    let max_size_pow2 = 20 in
    (* try each size ten times *)
    let max_iteration = 10 in
    (* this gives us around a hundred chances to fail, since failure (if present) is
       nondeterministic *)
    let%bind () =
      within_temp_dir (fun () ->
        Deferred.repeat_until_finished min_size_pow2 (fun size_pow2 ->
          let%map result =
            Deferred.repeat_until_finished 1 (fun iteration ->
              let%map result =
                let size = 1 lsl size_pow2 in
                test_atomic_write_once ~size ~iteration
              in
              (* repeat until failure or done with the current size *)
              match result with
              | Ok () when iteration < max_iteration -> `Repeat (iteration + 1)
              | result -> `Finished result)
          in
          (* repeat until failure or done with all sizes *)
          match result with
          | Ok () when size_pow2 < max_size_pow2 -> `Repeat (size_pow2 + 1)
          | result ->
            (* when finished, check for success *)
            require_ok [%here] result;
            `Finished ()))
    in
    [%expect {| |}];
    return ()
  ;;

  open struct
    (* Helpers for load/save tests. *)

    let test_load_save m original ~load ~save =
      within_temp_dir (fun () ->
        let path = File_path.of_string "data.sexp" in
        let%bind () = IO.async (fun () -> save path original) in
        let%bind contents = IO.async (fun () -> read_file path) in
        print_string contents;
        let%bind round_trip = IO.async (fun () -> load path) in
        require_equal [%here] m original round_trip;
        return ())
    ;;
  end

  let load_sexp = IO.load_sexp
  let save_sexp = IO.save_sexp

  let%expect_test "[load_sexp] and [save_sexp]" =
    let test string =
      test_load_save (module Sexp) ~load:load_sexp ~save:save_sexp (Sexp.of_string string)
    in
    let%bind () = test "()" in
    [%expect {| () |}];
    let%bind () =
      test
        "((key value) (other-key other-value) (another-key another-value) \
         (yet-another-key yet-another-value))"
    in
    [%expect
      {|
      ((key value) (other-key other-value) (another-key another-value)
       (yet-another-key yet-another-value)) |}];
    return ()
  ;;

  let load_sexps = IO.load_sexps
  let save_sexps = IO.save_sexps

  let%expect_test "[load_sexps] and [save_sexps]" =
    let test string =
      test_load_save
        (module struct
          type t = Sexp.t list [@@deriving equal, sexp_of]
        end)
        ~load:load_sexps
        ~save:save_sexps
        (Sexp.of_string_many string)
    in
    let%bind () = test "" in
    [%expect {| |}];
    let%bind () =
      test
        "(key value) (other-key other-value) (another-key another-value) \
         (yet-another-key yet-another-value)"
    in
    [%expect
      {|
      (key value)
      (other-key other-value)
      (another-key another-value)
      (yet-another-key yet-another-value) |}];
    return ()
  ;;

  let load_as_sexp = IO.load_as_sexp
  let save_as_sexp = IO.save_as_sexp

  let%expect_test "[load_as_sexp] and [save_as_sexp]" =
    let module M = struct
      type t = string Int.Map.t [@@deriving equal, sexp]
    end
    in
    let test alist =
      test_load_save
        (module M)
        ~load:(load_as_sexp ~of_sexp:M.t_of_sexp)
        ~save:(save_as_sexp ~sexp_of:M.sexp_of_t)
        (Int.Map.of_alist_exn alist)
    in
    let%bind () = test [] in
    [%expect {| () |}];
    let%bind () =
      test
        [ 1, "a unit"; 2, "a prime number"; 3, "a prime number"; 4, "a composite number" ]
    in
    [%expect
      {|
      ((1 "a unit") (2 "a prime number") (3 "a prime number")
       (4 "a composite number")) |}];
    return ()
  ;;

  let load_as_sexps = IO.load_as_sexps
  let save_as_sexps = IO.save_as_sexps

  let%expect_test "[load_as_sexps] and [save_as_sexps]" =
    let module M = struct
      type t = string * string list [@@deriving equal, sexp]
    end
    in
    let test list =
      test_load_save
        (module struct
          type t = M.t list [@@deriving equal, sexp_of]
        end)
        ~load:(load_as_sexps ~of_sexp:M.t_of_sexp)
        ~save:(save_as_sexps ~sexp_of:M.sexp_of_t)
        list
    in
    let%bind () = test [] in
    [%expect {| |}];
    let%bind () =
      test
        [ "do", [ "a deer"; "a female deer" ]
        ; "re", [ "a drop of golden sun" ]
        ; "mi", [ "a name"; "I call myself" ]
        ]
    in
    [%expect
      {|
      (do ("a deer" "a female deer"))
      (re ("a drop of golden sun"))
      (mi ("a name" "I call myself")) |}];
    return ()
  ;;

  let exists = IO.exists
  let exists_exn = IO.exists_exn
  let is_directory = IO.is_directory
  let is_directory_exn = IO.is_directory_exn
  let is_file = IO.is_file
  let is_file_exn = IO.is_file_exn

  let%expect_test "[exists] and [is_directory] and [is_file]" =
    let module M = struct
      type t =
        [ `Yes
        | `No
        | `Unknown
        ]
      [@@deriving equal, sexp_of]
    end
    in
    let test path =
      let%bind exists_exn = IO.async (fun () -> exists_exn path) in
      let%bind is_file_exn = IO.async (fun () -> is_file_exn path) in
      let%bind is_directory_exn = IO.async (fun () -> is_directory_exn path) in
      print_s
        [%sexp
          { path : File_path.t
          ; exists_exn : bool
          ; is_file_exn : bool
          ; is_directory_exn : bool
          }];
      let%bind exists = IO.async (fun () -> exists path) in
      let%bind is_file = IO.async (fun () -> is_file path) in
      let%bind is_directory = IO.async (fun () -> is_directory path) in
      require_equal [%here] (module M) exists (if exists_exn then `Yes else `No);
      require_equal [%here] (module M) is_file (if is_file_exn then `Yes else `No);
      require_equal
        [%here]
        (module M)
        is_directory
        (if is_directory_exn then `Yes else `No);
      return ()
    in
    within_temp_dir (fun () ->
      let%bind () =
        let file = File_path.of_string "file" in
        let%bind () = run "touch" [ (file :> string) ] in
        test file
      in
      [%expect
        {|
        ((path             file)
         (exists_exn       true)
         (is_file_exn      true)
         (is_directory_exn false)) |}];
      let%bind () =
        let directory = File_path.of_string "dir" in
        let%bind () = run "mkdir" [ (directory :> string) ] in
        test directory
      in
      [%expect
        {|
        ((path             dir)
         (exists_exn       true)
         (is_file_exn      false)
         (is_directory_exn true)) |}];
      let%bind () = test (File_path.of_string "nonexistent") in
      [%expect
        {|
        ((path             nonexistent)
         (exists_exn       false)
         (is_file_exn      false)
         (is_directory_exn false)) |}];
      let%bind () = test (File_path.of_string "/dev/null") in
      [%expect
        {|
        ((path             /dev/null)
         (exists_exn       true)
         (is_file_exn      false)
         (is_directory_exn false)) |}];
      let%bind () =
        let symlink = File_path.of_string "symlink" in
        let%bind () = run "ln" [ "-s"; "/"; (symlink :> string) ] in
        test symlink
      in
      [%expect
        {|
        ((path             symlink)
         (exists_exn       true)
         (is_file_exn      false)
         (is_directory_exn true)) |}];
      return ())
  ;;

  let unlink = IO.unlink

  let%expect_test "[unlink]" =
    within_temp_dir (fun () ->
      let path = File_path.of_string "my-file" in
      let%bind () = run "touch" [ (path :> string) ] in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect {|
        ./
        ../
        my-file |}];
      let%bind () = IO.async (fun () -> unlink path) in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect {|
        ./
        ../ |}];
      return ())
  ;;

  let rename = IO.rename

  let%expect_test "[rename]" =
    within_temp_dir (fun () ->
      let src = File_path.of_string "source" in
      let dst = File_path.of_string "destination" in
      let%bind () = run "touch" [ (src :> string) ] in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect {|
        ./
        ../
        source |}];
      let%bind () = IO.async (fun () -> rename ~src ~dst) in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect {|
        ./
        ../
        destination |}];
      return ())
  ;;

  let mkdir = IO.mkdir
  let rmdir = IO.rmdir

  let%expect_test "[mkdir] and [rmdir]" =
    within_temp_dir (fun () ->
      let path = File_path.of_string "etc" in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect {|
        ./
        ../ |}];
      let%bind () = IO.async (fun () -> mkdir path) in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect {|
        ./
        ../
        etc/ |}];
      let%bind () = IO.async (fun () -> rmdir path) in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect {|
        ./
        ../ |}];
      return ())
  ;;

  let chdir = IO.chdir
  let getcwd = IO.getcwd

  let%expect_test "[chdir] and [getcwd]" =
    within_temp_dir (fun () ->
      let subdir = File_path.Relative.of_string "subdir" in
      let%bind () = Unix.mkdir (subdir :> string) in
      let%bind tmp = IO.async (fun () -> getcwd ()) in
      let%bind () = IO.async (fun () -> chdir (File_path.of_relative subdir)) in
      let%bind tmp_slash_subdir = IO.async (fun () -> getcwd ()) in
      require_equal
        [%here]
        (module File_path.Absolute)
        tmp_slash_subdir
        (File_path.Absolute.append tmp subdir);
      [%expect {| |}];
      return ())
  ;;

  let ls_dir = IO.ls_dir

  let%expect_test "[ls_dir]" =
    with_temp_dir (fun tmp ->
      let%bind () = run "touch" [ tmp ^/ "regular-file" ] in
      let%bind () = run "mkdir" [ tmp ^/ "subdirectory" ] in
      let%bind () = run "touch" [ tmp ^/ "subdirectory/another-file" ] in
      let%bind ls = IO.async (fun () -> ls_dir (File_path.of_string tmp)) in
      print_s [%sexp (ls : File_path.Part.t list)];
      [%expect {| (regular-file subdirectory) |}];
      return ())
  ;;

  let make_absolute_under_cwd = IO.make_absolute_under_cwd

  let%expect_test "[make_absolute_under_cwd]" =
    within_temp_dir (fun () ->
      let%bind tmp = Sys.getcwd () in
      let test string =
        let path = File_path.of_string string in
        let%bind abspath = IO.async (fun () -> make_absolute_under_cwd path) in
        File_path.Absolute.to_string abspath
        |> replace ~pattern:tmp ~with_:"$TMP"
        |> print_endline;
        return ()
      in
      let%bind () = test "a/relative/path" in
      [%expect {| $TMP/a/relative/path |}];
      let%bind () = test "/usr/share/dict/words" in
      [%expect {| /usr/share/dict/words |}];
      return ())
  ;;

  let make_relative_to_cwd = IO.make_relative_to_cwd
  let make_relative_to_cwd_exn = IO.make_relative_to_cwd_exn
  let make_relative_to_cwd_if_possible = IO.make_relative_to_cwd_if_possible

  let%expect_test "[make_relative_to_cwd]" =
    within_temp_dir (fun () ->
      let test string =
        let path = File_path.of_string string in
        let%bind relative_to_cwd = IO.async (fun () -> make_relative_to_cwd path) in
        let%bind relative_to_cwd_exn =
          Deferred.Or_error.try_with (fun () ->
            IO.async (fun () -> make_relative_to_cwd_exn path))
        in
        let%bind relative_to_cwd_if_possible =
          IO.async (fun () -> make_relative_to_cwd_if_possible path)
        in
        print_s [%sexp (relative_to_cwd_if_possible : File_path.t)];
        require_equal
          [%here]
          (module struct
            type t = File_path.Relative.t option [@@deriving equal, sexp_of]
          end)
          relative_to_cwd
          (Or_error.ok relative_to_cwd_exn);
        require_equal
          [%here]
          (module File_path)
          relative_to_cwd_if_possible
          (Option.value_map relative_to_cwd ~f:File_path.of_relative ~default:path);
        return ()
      in
      let%bind tmp = Sys.getcwd () in
      let%bind () = test tmp in
      [%expect {| . |}];
      let%bind () = test (tmp ^/ "path/to/file") in
      [%expect {| path/to/file |}];
      let%bind () = test "/usr/share/dict/words" in
      [%expect {| /usr/share/dict/words |}];
      let%bind () = test "a/relative/path" in
      [%expect {| a/relative/path |}];
      return ())
  ;;

  let realpath = IO.realpath
  let realpath_absolute = IO.realpath_absolute
  let realpath_relative_to_cwd = IO.realpath_relative_to_cwd

  let%expect_test "[realpath_relative_to_cwd]" =
    with_temp_dir (fun tmp ->
      (* helpers *)
      let in_dir dir f =
        let%bind cwd = Sys.getcwd () in
        Monitor.protect
          (fun () ->
             let%bind () = Unix.chdir dir in
             f ())
          ~finally:(fun () -> Unix.chdir cwd)
      in
      let abspath string = File_path.Absolute.of_string (tmp ^/ string) in
      let relpath string = File_path.Relative.of_string string in
      let irrelevant_dir = Filename.temp_dir_name in
      let fns =
        (* all the [realpath*] functions, called with both abspaths and relpaths *)
        let realpath_absolute_of_abspath string =
          IO.async (fun () -> realpath_absolute (abspath string))
        in
        let realpath_of_abspath string =
          IO.async (fun () ->
            realpath
              (abspath string :> File_path.t)
              ~relative_to:(File_path.Absolute.of_string irrelevant_dir))
        in
        let realpath_of_relpath string =
          IO.async (fun () ->
            realpath
              (relpath string :> File_path.t)
              ~relative_to:(File_path.Absolute.of_string tmp))
        in
        let realpath_relative_to_cwd_of_abspath string =
          in_dir irrelevant_dir (fun () ->
            IO.async (fun () ->
              realpath_relative_to_cwd (abspath string :> File_path.t)))
        in
        let realpath_relative_to_cwd_of_relpath string =
          in_dir tmp (fun () ->
            IO.async (fun () ->
              realpath_relative_to_cwd (relpath string :> File_path.t)))
        in
        [ realpath_absolute_of_abspath
        ; realpath_of_abspath
        ; realpath_of_relpath
        ; realpath_relative_to_cwd_of_abspath
        ; realpath_relative_to_cwd_of_relpath
        ]
      in
      let test string =
        let%bind results =
          Deferred.List.map fns ~f:(fun fn ->
            Deferred.Or_error.try_with ~extract_exn:true (fun () -> fn string))
        in
        match
          List.all_equal
            results
            ~equal:[%equal: (File_path.Absolute.t, (Error.t[@equal.ignore])) Result.t]
        with
        | Some result ->
          [%sexp (result : File_path.Absolute.t Or_error.t)]
          |> replace_s ~pattern:tmp ~with_:"$TMP"
          |> print_s;
          return ()
        | None ->
          print_cr
            [%here]
            [%message
              "realpath results differ" (results : File_path.Absolute.t Or_error.t list)];
          return ()
      in
      (* test tmp directory itself *)
      let%bind () = test "." in
      [%expect {| (Ok $TMP) |}];
      (* test a nonexistent path *)
      let%bind () = test "nonexistent" in
      [%expect
        {| (Error (Unix.Unix_error "No such file or directory" realpath $TMP/nonexistent)) |}];
      (* test a normal file *)
      let%bind () = run "touch" [ tmp ^/ "real-file" ] in
      let%bind () = test "real-file" in
      [%expect {| (Ok $TMP/real-file) |}];
      (* test a symlink to a normal file *)
      let%bind () = Unix.symlink ~link_name:(tmp ^/ "link-file") ~target:"real-file" in
      let%bind () = test "link-file" in
      [%expect {| (Ok $TMP/real-file) |}];
      (* test a normal directory *)
      let%bind () = Unix.mkdir (tmp ^/ "a") in
      let%bind () = Unix.mkdir (tmp ^/ "a/b") in
      let%bind () = test "a/b" in
      [%expect {| (Ok $TMP/a/b) |}];
      (* test a symlink to a directory *)
      let%bind () = Unix.mkdir (tmp ^/ "c") in
      let%bind () = Unix.symlink ~link_name:(tmp ^/ "c/d") ~target:"../a" in
      let%bind () = test "c/d" in
      [%expect {| (Ok $TMP/a) |}];
      return ())
  ;;

  open struct
    (* helpers for temp file tests *)

    (* Supply optional arguments to a temp file creating function. *)
    let wrap f ~in_dir ~prefix ~suffix arg =
      f
        ?in_dir:(Some (File_path.of_absolute in_dir))
        ?prefix:(Some prefix)
        ?suffix:(Some suffix)
        arg
    ;;

    (* Check a temporary file or directory after creation. *)
    let check path ~in_dir ~prefix ~suffix ~expect_directory =
      let exists = Sys_unix.file_exists_exn (File_path.Absolute.to_string path) in
      require [%here] exists;
      let is_directory = Sys_unix.is_directory_exn (File_path.Absolute.to_string path) in
      require_equal [%here] (module Bool) is_directory expect_directory;
      require_equal
        [%here]
        (module File_path.Absolute)
        (File_path.Absolute.dirname_exn path)
        in_dir;
      let name = File_path.Part.to_string (File_path.Absolute.basename_exn path) in
      require [%here] (String.is_prefix name ~prefix);
      require [%here] (String.is_suffix name ~suffix)
    ;;

    (* Test a temp file creating function. *)
    let test f ~expect_directory =
      let f = wrap f in
      with_temp_dir (fun in_dir ->
        let in_dir = File_path.Absolute.of_string in_dir in
        let prefix = "the_prefix" in
        let suffix = "the_suffix" in
        (* create and check two files... *)
        let%bind path1 = IO.async (fun () -> f ~in_dir ~prefix ~suffix ()) in
        check path1 ~in_dir ~prefix ~suffix ~expect_directory;
        let%bind path2 = IO.async (fun () -> f ~in_dir ~prefix ~suffix ()) in
        check path2 ~in_dir ~prefix ~suffix ~expect_directory;
        (* ...and make sure they are unique. *)
        require
          [%here]
          (not (File_path.Absolute.equal path1 path2))
          ~if_false_then_print_s:
            (lazy
              [%message
                "temp files not unique"
                  (path1 : File_path.Absolute.t)
                  (path2 : File_path.Absolute.t)]);
        return ())
    ;;

    let modify path ~remove =
      (* Act on the temp file to make sure it gets cleaned up properly regardless of
         usage, allowing coverage for both keeping and deleting the file. *)
      let open IO.Let_syntax in
      let path = File_path.of_absolute path in
      let%bind is_dir = is_directory_exn path in
      match remove, is_dir with
      | false, false -> write_file path ~contents:"modified"
      | false, true -> write_file (path /?. ~."temp-file") ~contents:"added"
      | true, false -> unlink path
      | true, true -> rmdir path
    ;;

    (* Test a [with] function for temporary files. *)
    let test_with f ~expect_directory ~remove =
      let f = wrap f in
      (* Create an outer temporary directory using expect test helpers. *)
      with_temp_dir (fun in_dir ->
        let in_dir = File_path.Absolute.of_string in_dir in
        let prefix = "the_prefix" in
        let suffix = "the_suffix" in
        let%bind path =
          let open IO.Let_syntax in
          IO.async (fun () ->
            f ~in_dir ~prefix ~suffix (fun temp ->
              (* Check, then modify, the temporary file. *)
              check temp ~in_dir ~prefix ~suffix ~expect_directory;
              let%map () = modify temp ~remove in
              temp))
        in
        (* Make sure the temporary file gets cleaned up. *)
        require
          [%here]
          (not (Sys_unix.file_exists_exn (File_path.Absolute.to_string path)));
        return ())
    ;;
  end

  let create_temp_file = IO.create_temp_file

  let%expect_test "[create_temp_file]" =
    let%bind () = test create_temp_file ~expect_directory:false in
    [%expect {| |}];
    return ()
  ;;

  let create_temp_dir = IO.create_temp_dir

  let%expect_test "[create_temp_dir]" =
    let%bind () = test create_temp_dir ~expect_directory:true in
    [%expect {| |}];
    return ()
  ;;

  let with_temp_file = IO.with_temp_file

  let%expect_test "[with_temp_file]" =
    let%bind () = test_with with_temp_file ~expect_directory:false ~remove:false in
    [%expect {| |}];
    let%bind () = test_with with_temp_file ~expect_directory:false ~remove:true in
    [%expect {| |}];
    return ()
  ;;

  let with_temp_dir = IO.with_temp_dir

  let%expect_test "[with_temp_dir]" =
    let%bind () = test_with with_temp_dir ~expect_directory:true ~remove:false in
    [%expect {| |}];
    let%bind () = test_with with_temp_dir ~expect_directory:true ~remove:true in
    [%expect {| |}];
    return ()
  ;;

  let within_temp_dir = IO.within_temp_dir

  let%expect_test "[within_temp_dir]" =
    let test ~remove =
      test_with ~expect_directory:true ~remove (fun ?in_dir ?prefix ?suffix f ->
        let open IO.Let_syntax in
        let above = Sys_unix.getcwd () in
        let%map result =
          within_temp_dir ?in_dir ?prefix ?suffix (fun () ->
            f (File_path.Absolute.of_string (Sys_unix.getcwd ())))
        in
        let below = Sys_unix.getcwd () in
        require_equal [%here] (module String) above below;
        result)
    in
    let%bind () = test ~remove:false in
    [%expect {| |}];
    let%bind () = test ~remove:true in
    [%expect {| |}];
    return ()
  ;;
end

module Test_file_path_core = Test_file_path_io (struct
    include File_path_unix

    type 'a io = 'a

    include (Monad.Ident : Monad.S with type 'a t := 'a io)

    let async f = In_thread.run f
  end)

module Test_file_path_async = Test_file_path_io (struct
    include File_path_unix_async

    type 'a io = 'a Deferred.t

    include (Deferred : Monad.S with type 'a t := 'a io)

    let async f = f ()
  end)
