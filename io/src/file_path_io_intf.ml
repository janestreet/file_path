open! Core

module type S = sig
  type 'a io

  (** The currently running executable.

      OCaml semantics do not guarantee an absolute path here. *)
  val executable_name : File_path.t Lazy.t

  (** {2 File I/O Wrappers}

      These functions abstract over either [In_channel] and [Out_channel], or
      [Async.Reader] and [Async.Writer]. *)

  val read_file : File_path.t -> string io
  val write_file : File_path.t -> contents:string -> unit io
  val load_sexp : File_path.t -> Sexp.t io
  val load_sexps : File_path.t -> Sexp.t list io
  val load_as_sexp : File_path.t -> of_sexp:(Sexp.t -> 'a) -> 'a io
  val load_as_sexps : File_path.t -> of_sexp:(Sexp.t -> 'a) -> 'a list io
  val save_sexp : File_path.t -> Sexp.t -> unit io
  val save_sexps : File_path.t -> Sexp.t list -> unit io
  val save_as_sexp : File_path.t -> 'a -> sexp_of:('a -> Sexp.t) -> unit io
  val save_as_sexps : File_path.t -> 'a list -> sexp_of:('a -> Sexp.t) -> unit io

  (** {2 [Filename] Wrappers}

      These functions abstract over [Filename_unix]. *)

  val realpath
    :  File_path.t
    -> relative_to:File_path.Absolute.t
    -> File_path.Absolute.t io

  val realpath_absolute : File_path.Absolute.t -> File_path.Absolute.t io
  val realpath_relative_to_cwd : File_path.t -> File_path.Absolute.t io

  (** {2 [Sys] Wrappers}

      These functions abstract over either [Core.Sys] or [Async.Sys]. *)

  val exists : File_path.t -> [ `Yes | `No | `Unknown ] io
  val exists_exn : File_path.t -> bool io
  val is_directory : File_path.t -> [ `Yes | `No | `Unknown ] io
  val is_directory_exn : File_path.t -> bool io
  val is_file : File_path.t -> [ `Yes | `No | `Unknown ] io
  val is_file_exn : File_path.t -> bool io
  val ls_dir : File_path.t -> File_path.Part.t list io

  (** {2 [Unix] Wrappers}

      These functions abstract over either [Core_unix] or [Async.Unix]. *)

  val rmdir : File_path.t -> unit io
  val chdir : File_path.t -> unit io
  val getcwd : unit -> File_path.Absolute.t io
  val unlink : File_path.t -> unit io
  val rename : src:File_path.t -> dst:File_path.t -> unit io
  val mkdir : ?parents:bool (** default: [false] *) -> File_path.t -> unit io

  (** {2 Current Directory Functions}

      These functions combine [File_path] and [getcwd]. *)

  (** Like [File_path.make_absolute ~under:(getcwd ())]. Avoids calling [getcwd] unless
      necessary. *)
  val make_absolute_under_cwd : File_path.t -> File_path.Absolute.t io

  (** Like [File_path.make_relative ~if_under:(getcwd ())]. Avoids calling [getcwd] unless
      necessary. *)
  val make_relative_to_cwd : File_path.t -> File_path.Relative.t option io

  (** Like [make_relative_to_cwd]. Raises instead of returning [None]. *)
  val make_relative_to_cwd_exn : File_path.t -> File_path.Relative.t io

  (** Like [make_relative_to_cwd]. Returns the original path instead of [None]. *)
  val make_relative_to_cwd_if_possible : File_path.t -> File_path.t io
end

module type File_path_io = sig
  module type S = S
end
