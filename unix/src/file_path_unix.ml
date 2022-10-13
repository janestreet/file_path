open! Core
module Unix = Core_unix
include File_path_unix_intf

let executable_name = lazy (File_path.of_string Sys_unix.executable_name)
let default_temp_dir = lazy (File_path.of_string Filename.temp_dir_name)

include struct
  (* [Filename] wrappers *)

  let realpath_relative_to_cwd path =
    File_path.Absolute.of_string (Filename_unix.realpath (File_path.to_string path))
  ;;

  let realpath_absolute path = realpath_relative_to_cwd (File_path.of_absolute path)

  let realpath path ~relative_to =
    realpath_absolute (File_path.make_absolute path ~under:relative_to)
  ;;
end

include struct
  (* [Sys] wrappers *)

  let exists_exn path = Sys_unix.file_exists_exn (File_path.to_string path)
  let is_directory_exn path = Sys_unix.is_directory_exn (File_path.to_string path)
  let is_file_exn path = Sys_unix.is_file_exn (File_path.to_string path)
  let exists path = Sys_unix.file_exists (File_path.to_string path)
  let is_directory path = Sys_unix.is_directory (File_path.to_string path)
  let is_file path = Sys_unix.is_file (File_path.to_string path)

  let ls_dir path =
    Sys_unix.ls_dir (File_path.to_string path)
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

include struct
  (* temporary file/directory functions *)

  (* We write [internal_] versions of functions that all take required arguments of a type
     matching [in_dir:_ -> prefix:_ -> suffix:_ -> _ -> _]. *)

  let internal_create_temp_file ~in_dir ~prefix ~suffix () =
    File_path.Absolute.of_string
      (Filename_unix.temp_file
         ~in_dir:(File_path.Absolute.to_string in_dir)
         prefix
         suffix)
  ;;

  let internal_create_temp_dir ~in_dir ~prefix ~suffix () =
    File_path.Absolute.of_string
      (Filename_unix.temp_dir ~in_dir:(File_path.Absolute.to_string in_dir) prefix suffix)
  ;;

  let internal_with_temp_file ~in_dir ~prefix ~suffix f =
    let path = internal_create_temp_file ~in_dir ~prefix ~suffix () in
    Exn.protectx path ~f ~finally:(fun path ->
      (* delete the temp file, ok if it was already deleted *)
      try unlink (File_path.of_absolute path) with
      | Unix.Unix_error (ENOENT, _, _) -> ())
  ;;

  let internal_with_temp_dir ~in_dir ~prefix ~suffix f =
    let path = internal_create_temp_dir ~in_dir ~prefix ~suffix () in
    Exn.protectx path ~f ~finally:(fun path ->
      (* recursively delete the temporary directory and its contents *)
      let process_info =
        Unix.create_process
          ~prog:"rm"
          ~args:[ "-rf"; File_path.Absolute.to_string path ]
      in
      Unix.close process_info.stdin;
      (* Will probably fail if [rm] tries to produce any output. *)
      Unix.close process_info.stdout;
      Unix.close process_info.stderr;
      Unix.waitpid_exn process_info.pid)
  ;;

  let internal_within_temp_dir ~in_dir ~prefix ~suffix f =
    internal_with_temp_dir ~in_dir ~prefix ~suffix (fun path ->
      let prev = getcwd () in
      chdir (path :> File_path.t);
      Exn.protect ~f ~finally:(fun () ->
        (* restore the previous working directory before cleaning up the temp dir *)
        chdir (prev :> File_path.t)))
  ;;

  (* We wrap the internal functions with optional arguments uniformly. *)

  let wrap f ?in_dir ?prefix ?suffix arg =
    let in_dir =
      let path =
        match in_dir with
        | Some dir -> dir
        | None -> force default_temp_dir
      in
      make_absolute_under_cwd path
    in
    let prefix = Option.value prefix ~default:"" in
    let suffix = Option.value suffix ~default:"" in
    f ~in_dir ~prefix ~suffix arg
  ;;

  let create_temp_file = wrap internal_create_temp_file
  let create_temp_dir = wrap internal_create_temp_dir

  (* We have to eta-expand the polymorphic functions. *)

  let with_temp_file ?in_dir = wrap internal_with_temp_file ?in_dir
  let with_temp_dir ?in_dir = wrap internal_with_temp_dir ?in_dir
  let within_temp_dir ?in_dir = wrap internal_within_temp_dir ?in_dir
end

include struct
  (* file i/o *)

  let read_file path = In_channel.read_all (File_path.to_string path)

  let write_file path ~contents =
    let temp =
      create_temp_file
        ~in_dir:(File_path.dirname_defaulting_to_dot_or_root path)
        ~prefix:
          ("." ^ File_path.Part.to_string (File_path.basename_defaulting_to_dot path))
        ()
    in
    try
      Out_channel.write_all (temp :> string) ~data:contents;
      rename ~src:(temp :> File_path.t) ~dst:path
    with
    | exn ->
      unlink (temp :> File_path.t);
      Exn.reraise exn "failed to atomically write to file"
  ;;

  let load_as_sexp path ~of_sexp =
    Sexp.load_sexp_conv_exn (File_path.to_string path) of_sexp
  ;;

  let load_as_sexps path ~of_sexp =
    Sexp.load_sexps_conv_exn (File_path.to_string path) of_sexp
  ;;

  let load_sexp path = load_as_sexp path ~of_sexp:Fn.id
  let load_sexps path = load_as_sexps path ~of_sexp:Fn.id

  let write_with_buffer path ~f =
    let buf = Buffer.create 16 in
    f buf;
    write_file path ~contents:(Buffer.contents buf)
  ;;

  let save_as_sexps path xs ~sexp_of =
    write_with_buffer path ~f:(fun buf ->
      List.iter xs ~f:(fun x ->
        Sexp.to_buffer_hum (sexp_of x) ~buf;
        Buffer.add_char buf '\n'))
  ;;

  let save_as_sexp path x ~sexp_of = save_as_sexps path [ x ] ~sexp_of
  let save_sexp path sexp = save_as_sexp path sexp ~sexp_of:Fn.id
  let save_sexps path sexps = save_as_sexps path sexps ~sexp_of:Fn.id
end
