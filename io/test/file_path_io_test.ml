open! Core
open! Async
open! Expect_test_helpers_core
open! Expect_test_helpers_async

module type IO = sig
  include File_path_io.S

  (** Convert I/O results to a deferred. *)
  val async : 'a io -> 'a Deferred.t
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

  let read_file = IO.read_file
  let write_file = IO.write_file

  let%expect_test "[read_file] and [write_file]" =
    within_temp_dir (fun () ->
      let path = File_path.of_string "file.txt" in
      let write_contents = "you do the hokey pokey and you turn yourself around" in
      let%bind () = write_file path ~contents:write_contents |> IO.async in
      let%bind read_contents = read_file path |> IO.async in
      require_equal [%here] (module String) write_contents read_contents;
      [%expect {| |}];
      return ())
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
      let%bind exists_exn = exists_exn path |> IO.async in
      let%bind is_file_exn = is_file_exn path |> IO.async in
      let%bind is_directory_exn = is_directory_exn path |> IO.async in
      print_s
        [%sexp
          { path : File_path.t
          ; exists_exn : bool
          ; is_file_exn : bool
          ; is_directory_exn : bool
          }];
      let%bind exists = exists path |> IO.async in
      let%bind is_file = is_file path |> IO.async in
      let%bind is_directory = is_directory path |> IO.async in
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
      let%bind () = unlink path |> IO.async in
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
      let%bind () = rename ~src ~dst |> IO.async in
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
      let%bind () = mkdir path |> IO.async in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect {|
        ./
        ../
        etc/ |}];
      let%bind () = rmdir path |> IO.async in
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
      let%bind tmp = getcwd () |> IO.async in
      let%bind () = chdir (File_path.of_relative subdir) |> IO.async in
      let%bind tmp_slash_subdir = getcwd () |> IO.async in
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
      let%bind ls = ls_dir (File_path.of_string tmp) |> IO.async in
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
        let%bind abspath = make_absolute_under_cwd path |> IO.async in
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
        let%bind relative_to_cwd = make_relative_to_cwd path |> IO.async in
        let%bind relative_to_cwd_exn =
          Deferred.Or_error.try_with (fun () ->
            make_relative_to_cwd_exn path |> IO.async)
        in
        let%bind relative_to_cwd_if_possible =
          make_relative_to_cwd_if_possible path |> IO.async
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
end

module Test_file_path_core = Test_file_path_io (struct
    include File_path_unix

    type 'a io = 'a

    let async io = Deferred.return io
  end)

module Test_file_path_async = Test_file_path_io (struct
    include File_path_unix_async

    type 'a io = 'a Deferred.t

    let async io = io
  end)
