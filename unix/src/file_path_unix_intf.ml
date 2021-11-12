(** Blocking I/O operations for [File_path] types. See [../../doc/file-path.mdx]. *)

open! Core

module type S = sig
  include File_path_io.S with type 'a io := 'a (** @open *)
end

module type File_path_unix = sig
  module type S = S

  include S (** @open *)
end
