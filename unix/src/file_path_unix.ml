open! Core
module Unix = Core_unix
include File_path_unix_intf

let executable_name = lazy (File_path.of_string Sys.executable_name)

include struct
  (* file i/o *)

  let read_file path = In_channel.read_all (File_path.to_string path)

  let write_file path ~contents =
    Out_channel.write_all (File_path.to_string path) ~data:contents
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
    Sys.ls_dir (File_path.to_string path)
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
    if parents
    then Unix.mkdir_p (File_path.to_string path)
    else Unix.mkdir (File_path.to_string path)
  ;;

  let rmdir path = Unix.rmdir (File_path.to_string path)
  let chdir path = Unix.chdir (File_path.to_string path)
  let getcwd () = Unix.getcwd () |> File_path.Absolute.of_string
end

include struct
  (* current directory functions *)

  let make_absolute_under_cwd path =
    match File_path.to_variant path with
    | Absolute abspath -> abspath
    | Relative relpath -> File_path.Absolute.append (getcwd ()) relpath
  ;;

  let make_relative_to_cwd path =
    match File_path.to_relative path with
    | Some _ as some -> some
    | None -> File_path.make_relative path ~if_under:(getcwd ())
  ;;

  let make_relative_to_cwd_exn path =
    match File_path.to_relative path with
    | Some relpath -> relpath
    | None -> File_path.make_relative_exn path ~if_under:(getcwd ())
  ;;

  let make_relative_to_cwd_if_possible path =
    if File_path.is_relative path
    then path
    else File_path.make_relative_if_possible path ~if_under:(getcwd ())
  ;;
end
