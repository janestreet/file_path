open! Core
open! Async
include File_path_unix_async_intf

let executable_name = lazy (File_path.of_string Sys.executable_name)

include struct
  (* file i/o *)

  let read_file path = Reader.file_contents (File_path.to_string path)
  let write_file path ~contents = Writer.save (File_path.to_string path) ~contents
  let load_as_sexp path ~of_sexp = Reader.load_sexp_exn (File_path.to_string path) of_sexp

  let load_as_sexps path ~of_sexp =
    Reader.load_sexps_exn (File_path.to_string path) of_sexp
  ;;

  let load_sexp path = load_as_sexp path ~of_sexp:Fn.id
  let load_sexps path = load_as_sexps path ~of_sexp:Fn.id

  let save_as_sexps path xs ~sexp_of =
    Writer.save_sexps_conv (File_path.to_string path) xs sexp_of
  ;;

  let save_as_sexp path x ~sexp_of = save_as_sexps path [ x ] ~sexp_of
  let save_sexp path sexp = save_as_sexp path sexp ~sexp_of:Fn.id
  let save_sexps path sexps = save_as_sexps path sexps ~sexp_of:Fn.id
end

include struct
  (* [Filename] wrappers. *)

  let realpath_relative_to_cwd path =
    In_thread.run (fun () -> File_path_unix.realpath_relative_to_cwd path)
  ;;

  let realpath_absolute path =
    In_thread.run (fun () -> File_path_unix.realpath_absolute path)
  ;;

  let realpath path ~relative_to =
    In_thread.run (fun () -> File_path_unix.realpath path ~relative_to)
  ;;
end

include struct
  (* [Sys] wrappers *)

  let exists_exn path = Sys.file_exists_exn (File_path.to_string path)
  let is_directory_exn path = Sys.is_directory_exn (File_path.to_string path)
  let is_file_exn path = Sys.is_file_exn (File_path.to_string path)
  let exists path = Sys.file_exists (File_path.to_string path)
  let is_directory path = Sys.is_directory (File_path.to_string path)
  let is_file path = Sys.is_file (File_path.to_string path)

  let ls_dir path =
    let%map ls = Sys.ls_dir (File_path.to_string path) in
    ls
    |> List.map ~f:File_path.Part.of_string
    |> List.sort ~compare:File_path.Part.compare
  ;;
end

include struct
  (* [Unix] wrappers *)

  let unlink path = Unix.unlink (File_path.to_string path)

  let rename ~src ~dst =
    Unix.rename ~src:(File_path.to_string src) ~dst:(File_path.to_string dst)
  ;;

  let mkdir ?(parents = false) path =
    Unix.mkdir ?p:(if parents then Some () else None) (File_path.to_string path)
  ;;

  let rmdir path = Unix.rmdir (File_path.to_string path)
  let chdir path = Unix.chdir (File_path.to_string path)
  let getcwd () = Unix.getcwd () >>| File_path.Absolute.of_string
end

include struct
  (* current directory functions *)

  let make_absolute_under_cwd path =
    match File_path.to_variant path with
    | Absolute abspath -> return abspath
    | Relative relpath ->
      let%map cwd = getcwd () in
      File_path.Absolute.append cwd relpath
  ;;

  let make_relative_to_cwd path =
    match File_path.to_relative path with
    | Some _ as some -> return some
    | None ->
      let%map cwd = getcwd () in
      File_path.make_relative path ~if_under:cwd
  ;;

  let make_relative_to_cwd_exn path =
    match File_path.to_relative path with
    | Some relpath -> return relpath
    | None ->
      let%map cwd = getcwd () in
      File_path.make_relative_exn path ~if_under:cwd
  ;;

  let make_relative_to_cwd_if_possible path =
    if File_path.is_relative path
    then return path
    else (
      let%map cwd = getcwd () in
      File_path.make_relative_if_possible path ~if_under:cwd)
  ;;
end
